library(reshape2) # to load melt function
library(ggplot2)
# color blind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# Functions
source('R/adaptive_utils.R')

# Read in data
load('../data/experiment_data_contextual.RData')
load('../data/experiment_data_noncontextual.RData')

# saved hypothetical contextual probabilities
# contextual probabilities: A * A * K matrix for time, contexts, treatment arms
contextual_probs <- results$probs

# calculate AIPW scores with aw_scores function
aipw_scoresR_learn <- aw_scores(
  ws = results$ws,
  yobs = results$yobs,
  mu_hat = mu_hat,
  K = ncol(results$ys),
  balwts = balwts)

## Hyperparameters
# Number of treatment arms: W is the variable representing the treatment arms. K is the number of treatment arms. A is the number of observations.
K <- ncol(results$ys)
A <- nrow(results$ys)


# Get estimates for policies

## Define counterfactual policies
### Control policy matrix policy0. This is a matrix with A rows and K columns, where the elements in the first column are all 1s and the elements in the remaining columns are all 0s.
policy0 <- matrix(0, nrow = A, ncol = K)
policy0[,1] <- 1

### Treatment policies list policy1. This is a list with K elements, where each list contains a matrix with A rows and K columns. Identifier of treatment x: the x th column of the matrix in the x th policy in the list is 1.
policy1 <- lapply(1:K, function(x) {
  pol_mat <- matrix(0, nrow = A, ncol = K)
  pol_mat[,x] <- 1
  pol_mat
}
)

## estimating the value Q(w) of a single arm w. Here we estimate all the arms in policy1 in turn.
out_full <- output_estimates(
  policy1 = policy1,
  gammahat = aipw_scoresR_learn,
  contextual_probs = contextual_probs)

# Get estimates for treatment effects of policies as contrast to control \delta(w_1, w_2) = E[Y_t(w_1) - Y_t(w_2)]. In Hadad, Vitor, et al (2021) there are two approaches.
## The first approach: use the difference in AIPW scores as the unbiased scoring rule for \delta (w_1, w_2)
### The following function implements the first approach by subtracting policy0, the control arm, from all the arms in policy1, except for the control arm itself.
out_full_te1 <- output_estimates(
  policy0 = policy0,
  policy1 = policy1[-1],  ## remove the control arm from policy1
  contrasts = 'combined',
  gammahat = aipw_scoresR_learn,
  contextual_probs = contextual_probs)

## The second approach takes asymptotically normal inference about \delta(w_1, w_2): \delta ^ hat (w_1, w_2) = Q ^ hat (w_1) - Q ^ hat (w_2)
out_full_te2 <- output_estimates(
  contrasts = 'separate')


# Figure
library(ggplot2)

# Combine the data frames into a single data frame
combinedMatrix <- do.call(rbind, out_full_te2)
combinedMatrix <- as.data.frame(combinedMatrix)

# Add an .id column to indicate the arm number
n_arms <- length(out_full_te2)
n_reps <- nrow(out_full_te2[[1]])
combinedMatrix$id <- rep(1:n_arms, each = n_reps)

# Add a column to indicate the arm name
n_arms <- n_arms+1
combinedMatrix$arm <- rep(paste0("arm", 2:n_arms), each = n_reps)

# Add a method column
values <- c("uniform", "non_contextual_minvar", "contextual_minvar",
            "non_contextual_stablevar", "contextual_stablevar", "non_contextual_twopoint")
combinedMatrix$method <- rep(values, length.out = nrow(combinedMatrix))


# Create the plot
plot <- ggplot(combinedMatrix, aes(x = estimate, y = method)) +
  geom_point(aes(x = estimate), size = 2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), width = 0.05,
                position = position_dodge(width=0.5)) +
  xlab("Coefficient Estimate") +
  ylab("weights") +
  ggtitle("Coefficients and Confidence Intervals") +
  facet_wrap(~ arm, ncol = 2)

# Print the plot
print(plot)


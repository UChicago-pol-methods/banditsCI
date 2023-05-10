# Run Thompson Experiment

source('Thompson Sampling.R')
source('bernoulli_bandit_utils.R')
       
# Set parameters
floor_start <- 5
floor_decay <- 0.9
batch_sizes <- c(100, 200, 300, 400, 500)

# To generate a dataset with 1500 observations, 10 covariates, and 5 arms
set.seed(123)
data <- generate_bandit_data(n=1500, p=10, K=5, noise_std=1.0, signal_strength=1.0)

# access dataset components
xs <- data[[1]]$xs
ys <- data[[1]]$ys
muxs <- data[[1]]$muxs
A <- data[[1]]$A
p <- data[[1]]$p
K <- data[[1]]$K
mus <- data[[2]]

# Run experiment
results <- run_experiment(xs, ys, floor_start, floor_decay, batch_sizes)

# balwts: inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
balwts <- balwts(results$ws, results$probs)

# Save results and balwts
save(results, balwts, mus, file="experiment_data.RData")

# produce conditional means estimates for each covariate profile
## extract the coefficient estimates from results$fitted_bandit_model$mu and transpose
mu_transpose <- t(results$fitted_bandit_model$mu)

## for every matrix in the list results$fitted_bandit_model$X, add an column of 1s
X_1 <- lapply(results$fitted_bandit_model$X, cbind, 1)

## multiply X_1 by the coefficient estimates
mu_hat <- lapply(X_1, function(x) x %*% mu_transpose) 

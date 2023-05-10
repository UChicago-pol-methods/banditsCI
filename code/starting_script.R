library(reshape2) # to load melt function
library(ggplot2)
# color blind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

## Functions
source('bernoulli_bandit_utils.r')
source('adaptive_utils.R')

# Read in data
load('experiment_data.RData')

# saved hypothetical contextual probabilities 
# contextual probabilities: A * A * K matrix for time, contexts, treatment arms
contextual_probs <- results$probs

# calculate AIPW scores with aw_scores function
aipw_scoresR_learn <- aw_scores(
  ws = results$ws, 
  yobs = results$yobs, 
  mu_hat = mu_hat, 
  K = K,
  balwts = balwts)

## Hyperparameters
# Number of treatment arms: W is the variable representing the treatment arms. K is the number of treatment arms. A is the number of observations.
K <- ncol(results$e)
A <- results$T


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

# Tidy the data frame
## Panel for policy values
estimates_panel_df <- do.call(rbind.data.frame, out_full)
estimates_panel_df$weights <- rep(rownames(out_full[[1]]), 
                                  length(out_full))
estimates_panel_df$term <- rep(levels(results$e), each = nrow(out_full[[1]]))

## Panel for policy contrast with the first approach
estimates_panel_df_te1 <- do.call(rbind.data.frame, out_full_te1)
estimates_panel_df_te1$weights <- rep(rownames(out_full_te1[[1]]), 
                                      length(out_full_te1))
estimates_panel_df_te1$term <- rep(levels(results$e)[-1], each = nrow(out_full_te1[[1]]))
estimates_panel_df_te1$Respondent <- stringr::str_to_sentence(gsub('_', '',
                                                                   gsub('.*_R_', '', estimates_panel_df_te1$term)))
estimates_panel_df_te1$Headline <- stringr::str_to_sentence(gsub('_', '',
                                                                 gsub('H_|_R_.*', '', estimates_panel_df_te1$term)))
## Panel for policy contrast with the second approach
estimates_panel_df_te2 <- do.call(rbind.data.frame, out_full_te2)
estimates_panel_df_te2$weights <- rep(rownames(out_full_te2[[1]]), 
                                      length(out_full_te2))
estimates_panel_df_te2$term <- rep(levels(results$e)[-1], each = nrow(out_full_te2[[1]]))
estimates_panel_df_te2$Respondent <- stringr::str_to_sentence(gsub('_', '',
                                                                   gsub('.*_R_', '', estimates_panel_df_te2$term)))
estimates_panel_df_te2$Headline <- stringr::str_to_sentence(gsub('_', '',
                                                                 gsub('H_|_R_.*', '', estimates_panel_df_te2$term)))

# Figure
## Figure for policy contrast with the first approach
ggplot(estimates_panel_df_te1, aes(x = estimate, y = Respondent, color = weights)) +
  ggdist::stat_gradientinterval(aes(y = Respondent, 
                                    xdist = distributional::dist_normal(estimate, std.error)), 
                                .width = 0, size = 0, color = cbPalette[3], fill = cbPalette[3], 
                                position = position_dodge(width=0.5)) +
  geom_point(aes(x = estimate), size = 2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), width = 0.05, 
                position = position_dodge(width=0.5)) +
  facet_grid(rows = vars(Respondent), cols = vars(Headline), scales = 'free_y') + 
  ylab('Policy') + 
  xlab('Estimate') +
  geom_vline(xintercept = 0, colour = 'grey60', linetype = 2) +
  labs(title = 'Response function, first approach, scores',
       subtitle = 'Averages, learning split') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white', color = 'white'),
        # strip.text.y = element_blank()
  ) + 
  coord_cartesian(xlim = c(-1.5, 1))

ggsave('../../tables-figures/estimates_te1.png')

## Figure for policy contrast with the second approach
ggplot(estimates_panel_df_te2, aes(x = estimate, y = Respondent, color = weights)) +
  ggdist::stat_gradientinterval(aes(y = Respondent, 
                                    xdist = distributional::dist_normal(estimate, std.error)), 
                                .width = 0, size = 0, color = cbPalette[3], fill = cbPalette[3], 
                                position = position_dodge(width=0.5)) +
  geom_point(aes(x = estimate), size = 2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), width = 0.05, 
                position = position_dodge(width=0.5)) +
  facet_grid(rows = vars(Respondent), cols = vars(Headline), scales = 'free_y') + 
  ylab('Policy') + 
  xlab('Estimate') +
  geom_vline(xintercept = 0, colour = 'grey60', linetype = 2) +
  labs(title = 'Response function, second approach, scores',
       subtitle = 'Averages, learning split') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white', color = 'white'),
        # strip.text.y = element_blank()
  ) + 
  coord_cartesian(xlim = c(-1.5, 1))

ggsave('../../tables-figures/estimates_te2.png')

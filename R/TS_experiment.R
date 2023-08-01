# Run Thompson Experiment

source('R/experiment_utils.R')

# Set parameters ----
# To generate a dataset with 1000 observations, 5 covariates, and 4 arms
floor_start <- 5
floor_decay <- 0.9
batch_sizes <- c(1e3, 500, 500, 500)
A <- sum(batch_sizes)
p <- 5
K <- 4


set.seed(123)

# Simulate data ----

#* Data from data set ----
#** Interacted linear model ----
X <- matrix(rnorm(A*p), ncol=p) # - X: covariates of shape [A, p]
# generate a linear model
coef <- c(rnorm(2), rnorm(ncol(X)-2, sd = 0.05))
y_latent <- X %*% coef
y <- as.numeric(cut(rank(y_latent),
                    # one group is twice as large as other groups
                    breaks = c(0, A*(2:(K+1)/(K+1))) ))
data <- generate_bandit_data(X = X, y = y, noise = 0)


#* Tree data ----
# data <- simple_tree_data(
#   A=A,
#   K=K,
#   p=p,
#   split = 0.6,
#   noise_std = 0.5)

# Contextual experiment ----
# access dataset components
xs <- data[[1]]$xs
ys <- data[[1]]$ys
results <- run_experiment(ys, floor_start, floor_decay, batch_sizes, xs)

# conditional means model
mu_hat <- calculate_mu_hat(results)

# inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
balwts <- calculate_balwts(results$ws, results$probs)

# Save results, mu_hat and balwts
save(results, balwts, mu_hat, file="data/experiment_data_contextual.RData")

# plot the cumulative assignment graph for every arm and every batch size, x-axis is the number of observations, y-axis is the cumulative assignment
plot_cumulative_assignment(results, batch_sizes)
# save plot in the same directory, save the image in high resolution
dev.copy(png, file="contextual_cumulative_assignment_plot.png", width=1138, height=715)


# Non-contextual experiment ----
results <- run_experiment(ys, floor_start, floor_decay, batch_sizes, xs = NULL)

# inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
balwts <- calculate_balwts(results$ws, results$probs)

# Save results, mu_hat and balwts
save(results, balwts, file="data/experiment_data_noncontextual.RData")

plot_cumulative_assignment(results, batch_sizes)
dev.copy(png, file="noncontextual_cumulative_assignment_plot.png", width=1138, height=715)

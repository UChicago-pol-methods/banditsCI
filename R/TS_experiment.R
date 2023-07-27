# Run Thompson Experiment

source('R/experiment_utils.R')

# Set parameters
floor_start <- 5
floor_decay <- 0.9
batch_sizes <- c(100, 200, 300, 400)
set.seed(123)

# generate dummy data for `generate_bandit_data()`
n <- 1e3
p <- 5
X <- matrix(rnorm(n*p), ncol=p) # - X: covariates of shape [A, p]
# generate a linear model
X_interacted <- do.call(cbind,
                        apply(X, 2, function(x){
                          x * X
                        }, simplify = FALSE))
coef <- c(rnorm(ncol(X)), rnorm(ncol(X_interacted)-ncol(X), 0.5))
y_latent <- X_interacted %*% coef
y <- as.numeric(cut(rank(y_latent), breaks = c(0, n/4, n/2, n*3/4, n) ))
data <- generate_bandit_data(X = X, y = y,
                             noise_std=1.0, signal_strength=1.0)

# access dataset components
xs <- data[[1]]$xs
ys <- data[[1]]$ys

# Run contextual experiment
results <- run_experiment(ys, floor_start, floor_decay, batch_sizes, xs)
mu_hat <- calculate_mu_hat(results)

# inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
balwts <- balwts(results$ws, results$probs)

# Save results, mu_hat and balwts
save(results, balwts, mu_hat, file="data/experiment_data_contextual.RData")

# plot the cumulative assignment graph for every arm and every batch size, x-axis is the number of observations, y-axis is the cumulative assignment
plot_cumulative_assignment(results, batch_sizes)
# save plot in the same directory, save the image in high resolution
dev.copy(png, file="contextual_cumulative_assignment_plot.png", width=1138, height=715)


# Non-contextual experiment
results <- run_experiment(ys, floor_start, floor_decay, batch_sizes, xs = NULL)
mu_hat <- calculate_mu_hat(results)

# inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
balwts <- balwts(results$ws, results$probs)

# Save results, mu_hat and balwts
save(results, balwts, mu_hat, file="data/experiment_data_noncontextual.RData")

plot_cumulative_assignment(results, batch_sizes)
dev.copy(png, file="noncontextual_cumulative_assignment_plot.png", width=1138, height=715)

# Functions

source('Thompson Sampling.R')
       
# Set parameters
floor_start <- 5
floor_decay <- 0.9
batch_sizes <- c(100, 200, 300, 400)

# To generate a dataset with 1000 observations, 10 covariates, and 5 arms
set.seed(123)
data <- generate_bandit_data(n=1000, p=10, K=5, noise_std=1.0, signal_strength=1.0)

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


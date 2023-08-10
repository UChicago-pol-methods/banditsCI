# TS_Experiments AW Branch
# * write up a script that includes all of the content from Issue 7 combined with TS_Experiment.R
# * Create branch and pull request. .
# * Make code that works regardless of the sim code.

# Checks for Thompson Experiment

##### Are batches of correct sizes?
# Describe code
batch_splits <- sort(c(cumsum(batch_sizes), cumsum(batch_sizes) +1))
batch_splits <- batch_splits[batch_splits != 501]

batch_size_check <- results$probs[batch_splits,300,] # Analyze a subset of subjects #write in what we need to see to validate # add columns with the values that I am testing (100, 101, etc.)
batch_size_check <- as.data.frame(batch_size_check)
bsc_row_names <- c("Batch 1 End: 100", "Batch 2 Start: 101", "Batch 2 End: 200", "Batch 3 Start: 201", "Batch 3 End: 300", "Batch 4 Start: 301", "Batch 4 End: 400", "Batch 5 Start: 401", "Batch 5 End: 500")
rownames(batch_size_check) <- bsc_row_names
batch_size_check




##### In the first batch, are all assignment probabilities uniform? Is this the case across all contexts?
sum(results$probs[1:batch_sizes[1],,] == 1/data[[1]]$K) # Add in description



##### Conditional on a context and time period do assignment probabilities always sum to one?
# Create empty vector with as many elements as observations in the experiment
sums_to_one <- rep(NA, data[[1]]$A)

# Loop over each time period
for(i in 1:data[[1]]$A){
  # Save the summed probabilities across treatments for each context conditional
  # On time
  sums_to_one[i] <- table(rowSums(results$probs[i,,]))
}
sums_to_one

## Probabiliites not adding up to 1 #This is why table() is used
# Initially was results$probs[500,,]
sum(rowSums(results$probs[1:500,,])==1) # = 461
sum(rowSums(results$probs[1:500,,])!=1) # = 39
which(rowSums(results$probs[1:500,,])!=1)



##### Are probability floors being correctly implemented? Check that there is no probability below the assigned floor.
##### Does the smallest probability assigned get smaller over time?
# For loop iterates each time and makes sure nothing is below floors
floor_check <- rep(NA, data[[1]]$A) #what to input in this first slot

for (i in 1:data[[1]]$A) {
  floor_check[i] <- any(results$probs[i, ,] <  (floor_start / (floor_decay * i)))
}
floor_check

# Identify where probability floor is not being correctly implemented
which(!floor_check) # Check the probabilities to see if they are below



##### From our data, we should know which treatment arm is best for every context.
##### Plot the probability that an observation is assigned its true best treatment over time. Is this decreasing?
##### Do this separately for each context group, i.e., among observations that arm 1 is best for, among observations that arm 2 is best for, etc.

# Assignment of treatment 1
ws <- results$ws[which(data$data$muxs[,1]==1)] #treatment assigned
A <- length(ws) #number of subjects
K <- dim(results$probs)[length(dim(results$probs))] #variables, 4 treatments
dat <- matrix(0, nrow = A, ncol = K)
dat[cbind(1:A, ws)] <- 1
dat <- apply(dat, 2, cumsum)
matplot(dat, type = c("l"), col =1:K,
        main = "Assignment of Treatment 1",
        xlab = "Subjects (A)",
        ylab = "Number of Subjects Assigned")
legend("topleft", legend = 1:K, col=1:K, lty=1:K)

# Assignment of treatment 2
ws <- results$ws[which(data$data$muxs[,2]==1)] #treatment assigned
A <- length(ws) #number of subjects
K <- dim(results$probs)[length(dim(results$probs))] #variables, 4 treatments
dat <- matrix(0, nrow = A, ncol = K)
dat[cbind(1:A, ws)] <- 1
dat <- apply(dat, 2, cumsum)
matplot(dat, type = c("l"), col =1:K,
        main = "Assignment of Treatment 2",
        xlab = "Subjects (A)",
        ylab = "Number of Subjects Assigned")
legend("topleft", legend = 1:K, col=1:K, lty=1:K)

# Assignment of treatment 3
ws <- results$ws[which(data$data$muxs[,3]==1)] #treatment assigned
A <- length(ws) #number of subjects
K <- dim(results$probs)[length(dim(results$probs))] #variables, 4 treatments
dat <- matrix(0, nrow = A, ncol = K)
dat[cbind(1:A, ws)] <- 1
dat <- apply(dat, 2, cumsum)
matplot(dat, type = c("l"), col =1:K,
        main = "Assignment of Treatment 2",
        xlab = "Subjects (A)",
        ylab = "Number of Subjects Assigned")
legend("topleft", legend = 1:K, col=1:K, lty=1:K)

# Assignment of Treatment 4
ws <- results$ws[which(data$data$muxs[,4]==1)] #treatment assigned. alter [,K] value to represent treatment
A <- length(ws) #number of subjects
K <- dim(results$probs)[length(dim(results$probs))] #variables, 4 treatments
dat <- matrix(0, nrow = A, ncol = K)
dat[cbind(1:A, ws)] <- 1
dat <- apply(dat, 2, cumsum)
matplot(dat, type = c("l"), col =1:K,
        main = "Assignment of Treatment 4",
        xlab = "Subjects (A)",
        ylab = "Number of Subjects Assigned")
legend("topleft", legend = 1:K, col=1:K, lty=1:K)




# Run Thompson Experiment

source('experiment_utils.R')

# Set parameters ----
# To generate a dataset with 500 observations, 5 covariates, and 4 arms
floor_start <- 5
floor_decay <- 0.9
batch_sizes <- c(100, 100, 100, 100, 100)
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
save(results, balwts, mu_hat, file="../data/experiment_data_contextual.RData")

# plot the cumulative assignment graph for every arm and every batch size, x-axis is the number of observations, y-axis is the cumulative assignment
plot_cumulative_assignment(results, batch_sizes)
# save plot in the same directory, save the image in high resolution
dev.copy(png, file="contextual_cumulative_assignment_plot.png", width=1138, height=715)



# Non-contextual experiment ----
results <- run_experiment(ys, floor_start, floor_decay, batch_sizes, xs = NULL)

# inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
balwts <- calculate_balwts(results$ws, results$probs)

# Save results, mu_hat and balwts
save(results, balwts, file="../data/experiment_data_noncontextual.RData")

plot_cumulative_assignment(results, batch_sizes)
dev.copy(png, file="noncontextual_cumulative_assignment_plot.png", width=1138, height=715)

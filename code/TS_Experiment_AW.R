# TS_Experiments AW Branch
# * write up a script that includes all of the content from Issue 7 combined with TS_Experiment.R
# * Create branch and pull request. .
# * Make code that works regardless of the sim code.



# Checks for Thompson Experiment


##### Code to see if batches are of correct sizes

# Validation: Probabilities will be the same from each batch start to end value, but probabilities will different at each respective batch start.

# Random generated context batch size code
batch_splits <- sort(c(cumsum(batch_sizes), c(1, cumsum(batch_sizes) +1) )) # New variable that includes the start and end indexes for batches
batch_splits <- batch_splits[batch_splits != max(batch_splits)] # Removal max

sample_value <- sample(1:data[[1]]$A, size =1) # Random assigned context value

batch_size_check <- results$probs[batch_splits, sample_value, ] # Choose correct subset
batch_size_check <- as.data.frame(batch_size_check) #make the indexed data a data frame

batch_size_column <- paste0("Batch ", rep(1:(length(batch_splits)/2), each = 2) , c(' Start ', ' End '), '(', batch_splits, ')' )
batch_size_check <- cbind(Batches = batch_size_column, batch_size_check)

bsc_column_Names <- c("Batches (Time Value)", "Treatment_1", "Treatment_2", "Treatment_3", "Treatment_4")
colnames(batch_size_check) <- bsc_column_Names

batch_size_check
# perform some function on batch_size check
# save results

print(sample_value)

# To view data frame
# View(batch_size_check)

# Checks
num_rows <- nrow(batch_size_check)


for (col in bsc_column_Names[2:5]) {
  cat("Checking column:", col, "\n")

  for (i in seq(1, num_rows, by = 2)) {
    if (identical(batch_size_check[i, col], batch_size_check[i + 1, col])) {
      cat("Rows", i, "and", i + 1, "in column", col, "are equal\n")
    } else {
      cat("Rows", i, "and", i + 1, "in column", col, "are not equal\n")
    }
  }
  cat("\n")
}



# For loop code to check evry context

# iterate every context and check that batch start and end are equivalent
batch_size_check_list <- list()

for(i in 1:data[[1]]$A){

  batch_splits <- sort(c(cumsum(batch_sizes), c(1, cumsum(batch_sizes) +1) )) # New variable that includes the start and end indexes for batches
  batch_splits <- batch_splits[batch_splits != max(batch_splits)] # Removal max

  batch_size_check <- results$probs[batch_splits, i,] # Choose correct subset
  batch_size_check <- as.data.frame(batch_size_check) #make the indexed data a data frame

  batch_size_column <- paste0("Batch ", rep(1:(length(batch_splits)/2), each = 2) , c(' Start ', ' End '), '(', batch_splits, ')' )
  batch_size_check <- cbind(Batches = batch_size_column, batch_size_check)

  bsc_column_Names <- c("Batches (Time Value)", "Treatment_1", "Treatment_2", "Treatment_3", "Treatment_4")
  colnames(batch_size_check) <- bsc_column_Names

  batch_size_check_list[[i]] <- batch_size_check
  # perform some function on batch_size check
  # save results
}

print(batch_size_check_list)

# To call a specific context batch
#batch_size_check_list[[i]]

#Check that all start and end values in batches are equivalent

library(knitr)

# Empty data frame to store results and be called
results_df <- data.frame(
  Column = character(),
  Result = logical()
)

for (batch_size_check in batch_size_check_list) {
  num_rows <- nrow(batch_size_check)

  for (col in bsc_column_Names[2:5]) {
    all_pairs_equal <- TRUE

    for (i in seq(1, num_rows, by = 2)) {
      if (!identical(batch_size_check[i, col], batch_size_check[i + 1, col])) {
        all_pairs_equal <- FALSE
      }
    }

    # Store the result in the data frame
    results_df <- rbind(results_df, data.frame(Column = col, Result = all_pairs_equal))
  }
}
results_df

# results_df[1:2000,2]


##### In the first batch, are all assignment probabilities uniform? Is this the case across all contexts?

# Validation: All probabilities for time 100 for all contexts (A = 500) for all treatments (K = 4) that equal 1/data[[1]]$K will be equivalent to 100 * data[[1]]$A * data[[1]]$K

# This code indexes the first batch of results$probs and makes sure that all values are equal to 1/data[[1]]$K
first_batch_check <- sum(results$probs[1:batch_sizes[1],,] == 1/data[[1]]$K) # 1/data[[1]]$K is 1/4 = 0.25
first_batch_check

first_batch_check == 100 * data[[1]]$A * data[[1]]$K



##### Conditional on a context and time period do assignment probabilities always sum to one

# Validation: For every value of time (data[[1]]$A = 500) a value of data[[1]]$A * data[[1]]$K/4 (context * Treatment probabilities summed to one) will be output

sums_to_one <- rep(NA, data[[1]]$A) # Create empty vector with as many elements as observations in the experiment

# Loop over each time period
for(i in 1:data[[1]]$A){
  # Save the summed probabilities across treatments for each context conditional
  # On time
  sums_to_one[i] <- (names(table(rowSums(results$probs[i,,])))[1] == '1' & # first component of table is '1'
                      # AND the number of observations in that category is equal to A
                       table(rowSums(results$probs[i,,])) == data[[1]]$A) # Iterate data and make a table to correct rounding
}
table(sums_to_one) # Calculates how many values sum to one, signified by TRUE

# Checking that sums_to_one value is equal to all assignment probabilities summing to one
sum(sums_to_one) == data[[1]]$A * data[[1]]$K/4



##### Are probability floors being correctly implemented? Check that there is no probability below the assigned floor.
##### Does the smallest probability assigned get smaller over time?

# Validation: floor_check will output FALSE for every time (data[[1]]$A) across context (data[[1]]$A) and treatment (data[[1]]$K)

# For loop iterates each time and makes sure no treatment assignment probabilities are below probability floor
floor_check <- rep(NA, data[[1]]$A) #what to input in this first slot

# Iterate results$probs to check for values less than calculated probability floors
for (i in (batch_sizes[1] + 1):data[[1]]$A) # Values 101 through 500
{
  # check what batch i is in
  batch_start <- c(0, cumsum(batch_sizes))[max(which(c(0, cumsum(batch_sizes)) <i))] +1
  probability_floor <- (floor_start / (floor_decay * batch_start)) # Probability floor equation
  floor_check[i] <- any(results$probs[i, ,] < probability_floor)
}
floor_check

# Identify where probability floor is not being correctly implemented
which(floor_check)

# How many values where floor check is not being correctly implemented
sum(which(floor_check))

# Number of values where probability floor is correctly implemented
sum(floor_check) == "FALSE"


##### From our data, we should know which treatment arm is best for every context.
##### Plot the probability that an observation is assigned its true best treatment over time. Is this decreasing?
##### Do this separately for each context group, i.e., among observations that arm 1 is best for, among observations that arm 2 is best for, etc.

# Validation: Check that for each "ws <- results$ws[which(data$data$muxs[,1:K]==1)]" the best treatment is assigned to the most subjects

#The code is utilized to analyze how well TS_Experiments adapts treatment assignment to subjects based on the performance of treatments.
# Assignment of treatment 1
ws <- results$ws[which(data$data$muxs[,1]==1)] # Checks column of muxs for treatment assigned
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
        main = "Assignment of Treatment 3",
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

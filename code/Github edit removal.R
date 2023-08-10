7/31/23
```{r - muxs}
# Convert data$data$muxs to dataframe
muxs_df <- as.data.frame(data$data$muxs)
muxs_df <- tibble::rowid_to_column(muxs_df, "Index")

partial_muxs_df <- head(muxs_df,250)


# Plot muxs V1
ggplot(
  data = partial_muxs_df,
  aes(x = Index,
      y = V1
  ))  +
  geom_line()

# Plot muxs V2
ggplot(
  data = partial_muxs_df,
  aes(x = Index,
      y = V2
  ))  +
  geom_line()

# Plot muxs V3
ggplot(
  data = partial_muxs_df,
  aes(x = Index,
      y = V3
  ))  +
  geom_line()

# Plot muxs V4
ggplot(
  data = partial_muxs_df,
  aes(x = Index,
      y = V4
  ))  +
  geom_line()

# Count values in column V1
column_count_muxs <- muxs_df %>%
  count(V1,V2,V3,V4)
column_count_muxs

# Probability of values in column V1
probability_value_muxs <- muxs_df %>%
  summarize(Probability_of_value_in_muxs = sum(V1 == 1)/n())
probability_value_muxs

```

```{r - ys}
# Make data matrix into a data frame
ys_df <- as.data.frame(results$ys)

# Add an index column to data frame
ys_df <- tibble::rowid_to_column(ys_df, "Index")

partial_ys_df <- head(ys_df,1000)

# Plot content V1
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V1
  ))  +
  geom_line()

# Plot content V2
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V2
  ))  +
  geom_line()

# Plot content V3
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V3
  ))  +
  geom_line()

# Plot content V1
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V4
  ))  +
  geom_line()

# Count values in columns (LOOK INTO THIS)
ys_column_count <- ys_df %>%
  count(V1)
ys_column_count

# Probability of values in columns in ys data frame (WORK ON THIS)
ys_df_columns <- c("V1")

# Create Probability Function
probability_equation <- function(x) {
  sum(x)/length(x)
}
probability_ys_columns <- ys_df %>%
  summarize(across(all_of(ys_df_columns), # Across used to apply function
                   probability_equation,
                   .names ="Probability_of_{.col}"))

# Call probabilities
probability_ys_columns

# Alternative way to calculate probability
# probability_result <- probability_equation(ys_df$V1)
# probability_result

# Probability of Specific values
probability_value_ys <- ys_df %>%
  summarize(Probability_of_value_in_ys = sum(V1 == 2.273135e-01)/n())
probability_value_ys

```




## Steps to run the Thompson Experiment
* 1) Run libraries in experiment_utils.R
* 2) Run lines 1-52 in TS_experiment.R
* 3) Run lines 54-65 to view results and data visualization

#### **In the bandit simulations, we should be able to check:**
##### Are batches of correct sizes?
* Look into the Values section in the environment and check if the batches are correct.
* Analyze the line graph from line 65 and compare the vertical blue lines.
* Batches will be of 100, 200, 300, and 400
* start thinking about how to check this in probs as well
* results$probs[100:101,1:4,], results$probs[300:301,1:4,], results$probs[600:601,1:4,], results$probs[1000:1001,1:4,]

##### In the first batch, are all assignment probabilities uniform? Is this the case across all contexts?
* Look at data visualization from line 65 and compare starting points for the first batch
* results$probs all equal 0.25 for first batch, and for all contexts
* requires indexing: results$probs[1:101, 1:1000,]
* sum(results$probs[1:100,1:1000,] == 0.25) which outputs 400,000

##### Conditional on a context and time period do assignment probabilities always sum to one?
* look at results$probs
* requires indexing
* Yes, probabilities appear to be summing to 1
* sum(results$probs[1:1000,1:1000,]) = [1] 1e+06
* sum(results$probs[950,1,]) = [1] 1


##### Are probability floors being correctly implemented? Check that there is no probability below the assigned floor. Does the smallest probability assigned get smaller over time?
* look at results$probs
* requires indexing
* Is the probability floor .25?
  *sum(results$probs[1:100,1:1000,] < 0.25) which outputs[1] 0

##### From our data, we should know which treatment arm is best for every context. Plot the probability that an observation is assigned its true best treatment over time. Is this decreasing? Do this separately for each context group, i.e., among observations that arm 1 is best for, among observations that arm 2 is best for, etc.
* line plot for each treatment arm
* data$data$muxs

```{r - muxs}
# Convert data$data$muxs to dataframe
muxs_df <- as.data.frame(data$data$muxs)
muxs_df <- tibble::rowid_to_column(muxs_df, "Index")

partial_muxs_df <- head(muxs_df,100)


# Plot V1
ggplot(
  data = partial_muxs_df,
  aes(x = Index,
      y = V1
  ))  +
  geom_line()

# Count values in column V1
V1_count <- muxs_df %>%
  count(V1,V2,V3,V4)
V1_count

# Probability of values in column V1
probability_V1 <- muxs_df %>%
  summarize(Probability_of_V1 = sum(V1 == 1)/n())
probability_V1

```

```{r - ys}
# Make data matrix into a data frame
ys_df <- as.data.frame(results$ys)

# Add an index column to data frame
ys_df <- tibble::rowid_to_column(ys_df, "Index")

partial_ys_df <- head(ys_df,1000)

# Plot content V1
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V1
  ))  +
  geom_line()

# Plot content V2
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V2
  ))  +
  geom_line()

# Plot content V3
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V3
  ))  +
  geom_line()

# Plot content V1
ggplot(
  data = partial_ys_df,
  aes(x = Index,
      y = V4
  ))  +
  geom_line()

# Count values in columns (LOOK INTO THIS)
ys_column_count <- ys_df %>%
  count(V1,V2,V3,V4)
ys_column_count

# Probability of values in columns in ys data frame (WORK ON THIS)
ys_df_columns <- c("V1","V2","V3","V4")

# Create Probability Function
probability_equation <- function(x) {
  sum(x)/length(x)
}
probability_ys_columns <- ys_df %>%
  summarize(across(all_of(ys_df_columns),
                   probability_equation,
                   .names ="Probability_of_{.col}"))

# Call probabilities
probability_ys_columns

```
#### **Can we use the bandit code to force a non-adaptive experiment? I.e., where there is only the first, uniform batch? This will also be useful for checking assignment procedures: are all probabilities uniform throughout, across time periods and contexts?**

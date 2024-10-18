# Confidence intervals with adaptively generated data

This package provides functions for conducting frequentist inference on adaptively generated data. 
These functions produce point estimates and confidence intervals, using the methods proposed in [Zhan, Ruohan, et al. (2021)](https://arxiv.org/abs/2106.02029) and [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768). 
The code in this package is directly adapted from the original python code for those publications, documented at 
 - [github.com/gsbDBI/adaptive-confidence-intervals](https://github.com/gsbDBI/adaptive-confidence-intervals) and 
 - [github.com/gsbDBI/contextual_bandits_evaluation](https://github.com/gsbDBI/contextual_bandits_evaluation). 

For illustration, several functions for simulating non-contextual and contextual adaptive experiments using Thompson sampling are also supplied. 

### Installation

The latest release of the package can be installed through CRAN:

```R
install.packages("banditsCI")
```

The current development version can be installed from source using devtools:

```R
devtools::install_github("Uchicago-pol-methods/banditsCI")
```
### Usage Examples

```r
library(banditsCI)
set.seed(60637)

# Generate synthetic data. 
data <- generate_bandit_data(xs = as.matrix(iris[,1:4]), 
                             y = as.numeric(iris[,5]))

# Run a simulated (non-contextual) experiment. 
results <- run_experiment(ys = data$data$ys,
                          floor_start = 1/data$data$K,
                          floor_decay = 0.9,
                          batch_sizes = c(50, 50, 50))

# Evaluate mean response under treatment arms. 
## Balancing weights
balwts <- calculate_balwts(results$ws, results$probs)
## ipw scores
aipw_scores <- aw_scores(
  ws = results$ws, 
  yobs = results$yobs, 
  K = ncol(results$ys),
  balwts = balwts)

## The policies we're evaluating
policy1 <- lapply(1:data$data$K, function(x) {
  pol_mat <- matrix(0, nrow = data$data$A, ncol = data$data$K)
  pol_mat[,x] <- 1
  pol_mat
}
) 

## Estimation
out_full <- output_estimates(
  policy1 = policy1, 
  gammahat = aipw_scores, 
  probs_array = results$probs,
  floor_decay = 0.9)

out_full

```

For a more detailed description of how to use the package functions, see the [vignette](https://github.com/UChicago-pol-methods/banditsCI/tree/main/vignettes). 


### Estimation

We produce estimates under different adaptive weighting schemes in the `output_estimates()` function. 
Weighting schemes include:

- (Augmented) Inverse Probability Weighted estimates, with uniform weights, 
  - Estimated with argument `uniform = TRUE`.
- Non-contextual variance minimizing estimates,
  - Estimated with argument `non_contextual_minvar = TRUE`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029) Equation (6) with `MinVar` function $\phi(v) = \sqrt{1/v}$
- Contextual variance minimizing estimates,
  - Estimated with argument `contextual_minvar = TRUE`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029) Equation (11) with `MinVar` function $\phi(v) = \sqrt{1/v}$
- Non-contextual variance stabilizing estimates,
  - Estimated with argument `non_contextual_stablevar  = TRUE`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029) Equation (6) with `StableVar` function $\phi(v) = 1/v$
- Contextual variance stabilizing estimates,
  - Estimated with argument `contextual_stablevar = TRUE`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029) Equation (11) with `StableVar` function $\phi(v) = 1/v$
- Non-contextual two-point estimates using a "stick-breaking" procedure
  - Estimated with argument `non_contextual_twopoint = TRUE`.
  - Source: [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) Equation (18)


### Replication

We illustrate precise replication of simulated experimental results using the code from the original papers (we modify python notebooks for the benefit of illustration). 
The code for the replication is available in the following repositories:

- **Non-contextual replication**: [Replication with python experiment Data](https://github.com/UChicago-pol-methods/adaptive-confidence-intervals)
- **Contextual replication**: [Replication with python experiment Data](https://github.com/Uchicago-pol-methods/contextual_bandits_evaluation)

### References

- Zhan, Ruohan, Vitor Hadad, David A. Hirshberg, and Susan Athey. "Off-policy evaluation via adaptive weighting with data from contextual bandits." In *Proceedings of the 27th ACM SIGKDD Conference on Knowledge Discovery & Data Mining*, pp. 2125-2135. 2021. [https://arxiv.org/abs/2106.02029](https://arxiv.org/abs/2106.02029)
- Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. "Confidence intervals for policy evaluation in adaptive experiments." *Proceedings of the national academy of sciences* 118, no. 15 (2021): e2014602118. [https://arxiv.org/abs/1911.02768](https://arxiv.org/abs/1911.02768)

# Confidence intervals with adaptively generated data

This package contains a set of functions designed for running bandit-based experiments, along with conducting inference on policies evaluated on this adaptively generated data. 

In particular, the functions implement methods for estimating confidence intervals using adaptively generated data, as proposed in [Zhan, Ruohan, et al. (2021)](https://arxiv.org/abs/2106.02029) and [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768). 
The code in this package is directly adapted from the original python code for those projects documented at [github.com/gsbDBI/adaptive-confidence-intervals](https://github.com/gsbDBI/adaptive-confidence-intervals) and [github.com/gsbDBI/contextual_bandits_evaluation](https://github.com/gsbDBI/contextual_bandits_evaluation). 

### Installation

The latest release of the package can be installed through CRAN:

```R
install.packages(banditsCI")
```

The current development version can be installed from source using devtools.

```R
devtools::install_github("Uchicago-pol-methods/contextual_bandits_evaluation")
```
### Usage Examples

```r
library(banditsCI)

# Generate synthetic data. 

# Run an experiment. 

# Evaluate mean response under treatment arms. 

```


### Functions
- [Bandit-Based Experiments](#bandit-based-experiments)
- [Policy Evaluation](#policy-evaluation)
- [Estimators](#estimators)
- [Visualization](#visualization)
- [Replication](#replication)
- [References](#references)

### Bandit-Based Experiments

- **run_experiment**: Runs an experiment on simulated data using a contextual or non-contextual Thompson Sampling model for allocation.
  - **generate_bandit_data**: Generates synthetic data with a simple decision tree for simulated experiments.
  - **simple_tree_data**: Generates synthetic data with a simple decision tree for simulated experiments.


### Policy Evaluation

- **output_estimates**: Estimates treatment effects using augmented-inverse probability-weighted estimators with different weighting schemes. Can be used for both single-arm value estimation and contrast evaluation between treatments.
  - **ridge_muhat_lfo_pai**: Leave-future-out ridge-based estimates for arm expected rewards. 


### Estimators

The adaptive_utils.R contains estimator functions that are based on the [Adaptive Weighting in Contextual Bandits](https://github.com/gsbDBI/contextual_bandits_evaluation/blob/main/adaptive/inference.py) (in paper [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768)):

- AIPW (uniform weights), ![formula](https://latex.codecogs.com/svg.latex?\inline&space;\tiny&space;\hat{Q}^{DR}_T(\pi):=\frac{1}{T}\sum_{t=1}^T\hat{\Gamma}_t(X_t,\pi))
  - Estimated with argument `output_estimates()`, argument `uniform = TRUE)`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)
- non-contextual variance minimizing estimates,
  - $\phi(v) = 1/v$ yields weights $h_t$ that approximately minimize the variance of $\hat{Q}^{NC}_T$ (![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;\hat{Q}^{NC}_T(\pi):=\sum_{t=1}^{T}\frac{h_t\hat{\Gamma}_t(X_t,\pi)}{\sum_{t=1}^{T}h_s})). Non-contextual variance minimizing estimates: ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;$h_t:=\phi\left(\mathbb{E}w\left[\frac{\pi^2(X_t;w)}{e_t(X_t;w)}|H{t-1}\right]\right)$). A feasible approximation in the simulation: ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;$\tilde{h}_t:=\phi\left(\frac{1}{t-1}\sum_{s=1}^{t-1}\sum_w\frac{\pi^2(X_s;w)}{e_t(X_s;w)}\right)$).
  - Estimated with argument `output_estimates()`, argument `non_contextual_minvar = TRUE)`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)
- contextual minvar,
  - $\phi(v) = 1/v$ yields weights $h_t$ that approximately minimize the variance of $\hat{Q}^{\mathcal{C}}_T (\pi)$ (![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;\hat{Q}^{\mathcal{C}}_T(\pi)=\sum_{t=1}^T\frac{h_t(X_t)\hat{\Gamma}_t(X_t,\pi)}{\sum_{t=1}^T&space;h_s(X_t)})). Contextual variance proxy: ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;h_t(x)=\phi\left(\sum_w\frac{\pi^2(x,w)}{e_t(x,w)}\right),\quad&space;x\in\mathcal{X}).
  - Estimated with argument `output_estimates()`, argument `contextual_minvar = TRUE`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)
- non-contextual stablevar,
  - $\phi(v) = 1/\sqrt{v}$ yields weights $h_t$ that approximately standardize the terms of $\hat{Q}^{NC}_T$ (![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;\hat{Q}^{NC}_T(\pi):=\sum_{t=1}^{T}\frac{h_t\hat{\Gamma}_t(X_t,\pi)}{\sum_{t=1}^{T}h_s})). Non-contextual variance minimizing estimates: ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;$h_t:=\phi\left(\mathbb{E}w\left[\frac{\pi^2(X_t;w)}{e_t(X_t;w)}|H{t-1}\right]\right)$). A feasible approximation in the simulation: ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;$\tilde{h}_t:=\phi\left(\frac{1}{t-1}\sum_{s=1}^{t-1}\sum_w\frac{\pi^2(X_s;w)}{e_t(X_s;w)}\right)$).
  - Estimated with argument `output_estimates()`, argument `non_contextual_stablevar  = TRUE`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)
- contextual stablevar,
  - $\phi(v) = 1/\sqrt{v}$ yields weights $h_t$ that approximately standardize the terms of $\hat{Q}^{\mathcal{C}}_T (\pi)$(![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;\hat{Q}^{\mathcal{C}}_T(\pi)=\sum_{t=1}^T\frac{h_t(X_t)\hat{\Gamma}_t(X_t,\pi)}{\sum_{t=1}^T&space;h_s(X_t)})). Contextual variance proxy: ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;h_t(x)=\phi\left(\sum_w\frac{\pi^2(x,w)}{e_t(x,w)}\right),\quad&space;x\in\mathcal{X}). 
  - Estimated with argument `output_estimates()`, argument `contextual_stablevar = TRUE`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)
- non_contextual_twopoint,
  - "stick-breaking" procedure, $\frac{h_t^2}{e_t} = \left(1 - \sum_{s=1}^{t-1} \frac{h_s^2}{e_s} \right) \lambda_t$, where $\lambda_t$ satisfies ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;&space;0\leqslant\lambda_t<1) for all $1 \leq t \leq T - 1$, and $\lambda_T = 1$. ![formula](https://latex.codecogs.com/svg.image?\inline&space;\tiny&space;\lambda^{two-point}_t:=e_t\frac{1}{T-t&plus;1}&plus;(1-e_t)\frac{t^{-\alpha}}{t^{-\alpha}&plus;\frac{T^{1-\alpha}-t^{1-\alpha}}{1-\alpha}}).
  - Estimated with argument `output_estimates()`, argument `non_contextual_twopoint = TRUE`.
  - Allocation schemes: `twopoint_stable_var_ratio` function.
  - Source: [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768)

### Visualization

- **plot_cumulative_assignment**: Plots the cumulative assignments for different arms over time.

### Replication

We illustrate near-exact replication (with minor modifications) of simulated experimental results in the original papers. The code for the replication is available in the following repositories:

- **Non-contextual replication**: [Replication with python experiment Data](https://github.com/UChicago-pol-methods/adaptive-confidence-intervals)
- **Contextual replication**: [Replication with python experiment Data](https://github.com/Uchicago-pol-methods/contextual_bandits_evaluation)

### References

- Zhan, Ruohan, Vitor Hadad, David A. Hirshberg, and Susan Athey. "Off-policy evaluation via adaptive weighting with data from contextual bandits." In *Proceedings of the 27th ACM SIGKDD Conference on Knowledge Discovery & Data Mining*, pp. 2125-2135. 2021. [https://arxiv.org/abs/2106.02029](https://arxiv.org/abs/2106.02029)
- Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. "Confidence intervals for policy evaluation in adaptive experiments." *Proceedings of the national academy of sciences* 118, no. 15 (2021): e2014602118. [https://arxiv.org/abs/1911.02768](https://arxiv.org/abs/1911.02768)

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

- AIPW (uniform weights), $\hat{Q}^{DR}_T (\pi) := \frac{1}{T} \sum_{t=1}^T \hat{\Gamma}_t(X_t, \pi)$
  - Estimated with argument `output_estimates()`, argument `uniform = TRUE)`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)
- non-contextual variance minimizing estimates, 
  - Estimated with argument `output_estimates()`, argument `minvar = TRUE)`.
- contextual minvar,
- non-contextual stablevar,
- contextual stablevar,
- non_contextual_twopoint.

### Visualization

- **plot_cumulative_assignment**: Plots the cumulative assignments for different arms over time.

### Replication

We illustrate near-exact replication (with minor modifications) of simulated experimental results in the original papers. The code for the replication is available in the following repositories:

- **Non-contextual replication**: [Replication with python experiment Data](https://github.com/UChicago-pol-methods/adaptive-confidence-intervals)
- **Contextual replication**: [Replication with python experiment Data](https://github.com/Uchicago-pol-methods/contextual_bandits_evaluation)

### References

- Zhan, Ruohan, Vitor Hadad, David A. Hirshberg, and Susan Athey. "Off-policy evaluation via adaptive weighting with data from contextual bandits." In *Proceedings of the 27th ACM SIGKDD Conference on Knowledge Discovery & Data Mining*, pp. 2125-2135. 2021. [https://arxiv.org/abs/2106.02029](https://arxiv.org/abs/2106.02029)
- Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. "Confidence intervals for policy evaluation in adaptive experiments." *Proceedings of the national academy of sciences* 118, no. 15 (2021): e2014602118. [https://arxiv.org/abs/1911.02768](https://arxiv.org/abs/1911.02768)


| estimator/weighting                                                                                                                                                                         | paper                                                                               |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | In our package?                                                                                 |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| DR(uniformly-weighting)                                                                                                                                                                     | Off-Policy [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)                   | $\hat{Q}^{DR}_T (\pi) := \frac{1}{T} \sum_{t=1}^T \hat{\Gamma}_t(X_t, \pi)$                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Estimator: uniform. Output of output_estimates function                                         |
| improve on the DR estimator(stabilize variance)                                                                                                                                             | Off-Policy [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)                   | $\hat{Q}^{NC}_T(\pi) := \sum_{t=1}^{T}\frac{h_t \hat{\Gamma}_t(X_t, \pi)}{\sum_{t=1}^{T} h_s}$, $h_t := \phi \left( \mathbb{E}_w \left[ \frac{\pi^2(X_t; w)}{e_t(X_t; w)} \| H_{t-1} \right] \right)$                                                                                                                                                                                                                                                                                                                                                                                           |                                                                                                 |
| contextual-weighting                                                                                                                                                                        | Off-Policy [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)                   | $\hat{Q}^{\mathcal{C}}_T (\pi) = \sum_{t=1}^T\frac{h_t(X_t) \hat{\Gamma}_t(X_t, \pi)}{\sum_{t=1}^T h_s(X_t)}$                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |                                                                                                 |
| StableVar: improve on the DR estimator(stabilize variance)                                                                                                                                  | Off-Policy [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)                   | $\phi(v) = 1/\sqrt{v}$ yields weights $h_t$ that approximately standardize the terms of $\hat{Q}^{NC}_T$ or $\hat{Q}^{\mathcal{C}}_T (\pi)$. <br>**Non-contextual** variance proxy: $h_t := \phi \left( \mathbb{E}_w \left[ \frac{\pi^2(X_t; w)}{e_t(X_t; w)} \| H_{t-1} \right] \right)$,<br>a feasible approximation in our simulation: $\tilde{h}_t := \phi \left( \frac{1}{t - 1} \sum_{s=1}^{t-1} \sum_w \frac{\pi^2(X_s; w)}{e_t(X_s; w)} \right)$<br>**contextual variance** proxy: $h_t(x) = \phi \left( \sum_w \frac{\pi^2(x, w)}{e_t(x, w)} \right), \quad x \in \mathcal{X}$<br><br> | Estimators: non-contextual stablevar, contextual stablevar. Output of output_estimates function |
| MinVar: improve on the DR estimator(stabilize variance)                                                                                                                                     | Off-Policy [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)                   | $\phi(v) = 1/v$ yields weights $h_t$ that approximately minimize the variance of $\hat{Q}^{NC}_T$ or $\hat{Q}^{\mathcal{C}}_T (\pi)$. <br>**Non-contextual** variance proxy: $h_t := \phi \left( \mathbb{E}_w \left[ \frac{\pi^2(X_t; w)}{e_t(X_t; w)} \| H_{t-1} \right] \right)$,<br>a feasible approximation in our simulation: $\tilde{h}_t := \phi \left( \frac{1}{t - 1} \sum_{s=1}^{t-1} \sum_w \frac{\pi^2(X_s; w)}{e_t(X_s; w)} \right)$<br>**contextual variance** proxy: $h_t(x) = \phi \left( \sum_w \frac{\pi^2(x, w)}{e_t(x, w)} \right), \quad x \in \mathcal{X}$<br><br>        | Estimators: non-contextual minvar,contextual minvar. Output of output_estimates function        |
| sample mean naive(normal CI)                                                                                                                                                                | Confidence Interval [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) | $\hat{Q}^{AVG}(w) = \frac{1}{T_w} \sum_{t \leq T} Y_t, \quad T_w := \sum_{t \leq T} \mathbb{I}{\{W_t = w\}}$<br>In the code: sample_mean_naive<br>                                                                                                                                                                                                                                                                                                                                                                                                                                              |                                                                                                 |
| Sample mean (Howard et al CI)                                                                                                                                                               | Confidence Interval [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |                                                                                                 |
| non-asymptotic confidence intervals for the sample mean, based on the method of time-uniform confidence sequences described in [Howard et al. (2021)](https://arxiv.org/pdf/1810.08240.pdf) | Confidence Interval [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) | In Python code: evaluatie_gamma_exponential                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                 |
| Unweighted AIPW(uniform)                                                                                                                                                                    | Confidence Interval [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) | $\hat{Q}^{AIPW}_T (w) := \frac{1}{T} \sum_{t=1}^T \hat{\Gamma}^{AIPW}_t (w)$<br><br>"Figures 2 and 4 show that although the AIPW estimator with **uniform weights** (labeled as “unweighted AIPW”) is unbiased (P12)"<br>                                                                                                                                                                                                                                                                                                                                                                       | Estimator: uniform. Output of output_estimates function                                         |
| Weighted AIPW                                                                                                                                                                               | Confidence Interval [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) | Regression adjusted: $\hat{\Gamma}^{AIPW}_t (w) := \frac{\mathbb{I}\{W_t = w\}}{e_t(w)} Y_t + \left( 1 - \frac{\mathbb{I}\{W_t = w\}}{e_t(w)} \right) \hat{m}_t(w)$<br>*adaptively-weighted AIPW estimator*:<br><br>$\hat{Q}^{h}_T(w) = \frac{\sum_{t=1}^T h_t(w) \hat{\Gamma}^{AIPW}_t (w)}{\sum_{t=1}^T h_t(w)}$<br>                                                                                                                                                                                                                                                                          |                                                                                                 |
| Constant allocation rate(lvdl)                                                                                                                                                          | Confidence Interval [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) | "stick-breaking" procedure, $\frac{h_t^2}{e_t} = \left(1 - \sum_{s=1}^{t-1} \frac{h_s^2}{e_s} \right) \lambda_t$, where $\lambda_t$ satisfies $0 \leq \lambda_t < 1$ for all $1 \leq t \leq T - 1$, and $\lambda_T = 1$.<br><br>$\lambda^{const}_t := \frac{1}{T - t + 1}$<br><br>                                                                                                                                                                                                                                                                                                              |                                                                                                 |
| Two-point allocation rate                                                                                                                                                               | Confidence Interval [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768) | "stick-breaking" procedure, $\frac{h_t^2}{e_t} = \left(1 - \sum_{s=1}^{t-1} \frac{h_s^2}{e_s} \right) \lambda_t$, where $\lambda_t$ satisfies $0 \leq \lambda_t < 1$ for all $1 \leq t \leq T - 1$, and $\lambda_T = 1$.<br><br>$\lambda^{two-point}_t := e_t \frac{1}{T - t + 1} + (1 - e_t) \frac{t^{-\alpha}}{t^{-\alpha} + \frac{T^{1-\alpha}-t^{1-\alpha}}{1-\alpha}}$<br>                                                                                                                                                                                                                 | Allocation Schemes: twopoint_stable_var_ratio function. Estimator: non_contextual_twopoint      |


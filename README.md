# bandits_CI
## Overview
This project is a R package contains a set of functions designed for bandit-based experiments, policy evaluation, adaptive weighting schemes, and more. 
The directory contains methods for developing confidence intervals using adaptive data, proposed in [Zhan, Ruohan, et al. (2021)](https://arxiv.org/abs/2106.02029) and [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768). 
The code in this package is directly adapted from the python code for these projects documented at [github.com/gsbDBI/adaptive-confidence-intervals](https://github.com/gsbDBI/adaptive-confidence-intervals) and [github.com/gsbDBI/contextual_bandits_evaluation](https://github.com/gsbDBI/contextual_bandits_evaluation).

## Table of Contents
- [Bandit-Based Experiments](#bandit-based-experiments)
- [Policy Evaluation](#policy-evaluation)
- [Adaptive Weighting](#adaptive-weighting)
- [Allocation Schemes](#allocation-schemes)
- [Utility Functions](#utility-functions)
- [Estimators](#Estimators)
- [Visualization](#visualization)
- [Replication](#replication)
- [References](#references)

## Bandit-Based Experiments

- **LinTSModel**: Implements the Linear Thompson Sampling model.
- **update_thompson**: Updates the Thompson Sampling model with new data.
- **draw_thompson**: Draws from the Thompson Sampling model's posterior.
- **run_experiment**: Conducts a bandit experiment using the Linear Thompson Sampling model.
- **generate_bandit_data**: Simulates bandit experiment data.
- **simple_tree_data**: Generates synthetic data with a simple decision tree.
- **ridge_init**: Ridge Regression Initialization for Arm Expected Rewards.
- **ridge_update**: Ridge Regression Update for Arm Expected Rewards.
- **ridge_muhat_lfo_pai**: Plug-in Estimates for Arm Expected Rewards Using Ridge Regression.

## Policy Evaluation

- **estimate**: Estimates the policy value and its variance based on AIPW scores, policy matrices, and weights.
- **calculate_continuous_X_statistics**: Computes the estimate and variance of a policy evaluation using contextual weighting.
- **output_estimates**: Estimates treatment effects using AIPW with different weighting schemes. Can be used for both single-arm value estimation and contrast evaluation between treatments.

## Adaptive Weighting

- **aw_scores**: Computes the AIPW scores for given propensity scores and outcomes.
- **aw_estimate**: Estimates the policy value using non-contextual adaptive weighting.
- **aw_var**: Computes the variance of the policy value estimate using non-contextual adaptive weighting.
- **calculate_balwts**: Calculates balanced weights for AIPW estimation.

## Allocation Schemes

- **stick_breaking**: Implements the stick breaking algorithm for calculating weights in the stable-var scheme.
- **ifelse_clip**: Clips a numeric vector between two values.
- **twopoint_stable_var_ratio**: Calculates the allocation ratio for a two-point stable-variance bandit, given the empirical mean and the discount parameter alpha.

## Utility Functions

- **impose_floor**: Imposes a floor on a set of weights to avoid extreme values.
- **draw_thompson**: Draws from the posterior distribution in a Thompson Sampling model.

## Estimators
The adaptive_utils.R contains estimator functions that are based on the [Adaptive Weighting in Contextual Bandits](https://github.com/gsbDBI/contextual_bandits_evaluation/blob/main/adaptive/inference.py) (in paper [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768)):

- AIPW (uniform weights), ![formula](https://latex.codecogs.com/svg.latex?\hat{Q}^{DR}_T(\pi):=\frac{1}{T}\sum_{t=1}^T\hat{\Gamma}_t(X_t,\pi))
  - Estimated with argument `output_estimates()`, argument `uniform = TRUE)`.
  - Source: [Zhan et al. (2021)](https://arxiv.org/abs/2106.02029)
- non-contextual variance minimizing estimates, 
  - Estimated with argument `output_estimates()`, argument `minvar = TRUE)`.
- contextual minvar,
- non-contextual stablevar,
- contextual stablevar,
- non_contextual_twopoint.

## Visualization

- **plot_cumulative_assignment**: Plots the cumulative assignments for different arms over time.

## Replication

- **Non-Contextual Replication**: [Replication with Python Experiment Data](https://github.com/UChicago-pol-methods/adaptive-confidence-intervals/tree/non_contextual_replication)
- **Contextual Replication**: (provide the link later)

## References

- Zhan et al. (2021) [Off-Policy Evaluation via Adaptive Weighting with Data from Contextual Bandits](https://arxiv.org/abs/2106.02029)
- Hadad, Vitor, et al. (2021) [Confidence Intervals for Policy Evaluation in Adaptive Experiments](https://arxiv.org/abs/1911.02768)

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


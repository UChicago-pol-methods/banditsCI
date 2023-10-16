# bandits_CI
## Overview
This project is a R package contains a set of functions designed for bandit-based experiments, policy evaluation, adaptive weighting schemes, and more. The directory contains methods for developing confidence intervals using adaptive data, proposed in [Zhan, Ruohan, et al. (2021)](https://arxiv.org/abs/2106.02029) and [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768).

## Table of Contents
- [Bandit-Based Experiments](#bandit-based-experiments)
- [Policy Evaluation](#policy-evaluation)
- [Adaptive Weighting](#adaptive-weighting)
- [Allocation Schemes](#allocation-schemes)
- [Utility Functions](#utility-functions)
- [Estimators](#Estimators)
- [Visualization](#visualization)

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
- AIPW (uniform weights),
- non-contextual minvar,
- contextual minvar,
- non-contextual stablevar,
- contextual stablevar.

## Visualization

- **plot_cumulative_assignment**: Plots the cumulative assignments for different arms over time.

## References

- Zhan et al. (2021) [Off-Policy Evaluation via Adaptive Weighting with Data from Contextual Bandits](https://arxiv.org/abs/2106.02029)
- Hadad, Vitor, et al. (2021) [Confidence Intervals for Policy Evaluation in Adaptive Experiments](https://arxiv.org/abs/1911.02768)


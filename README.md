# bandits_CI
## Overview
This project is a R package setup for adaptively weighted confidence intervals. The directory contains methods for developing confidence intervals using adaptive data, proposed in [Zhan, Ruohan, et al. (2021)](https://arxiv.org/abs/2106.02029) and [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768).

## Functions
The adaptive_utils.R contains functions that are based on the [Adaptive Weighting in Contextual Bandits](https://github.com/gsbDBI/contextual_bandits_evaluation/blob/main/adaptive/inference.py) (in paper [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768)):
- AIPW (uniform weights),
- non-contextual minvar,
- contextual minvar,
- non-contextual stablevar,
- contextual stablevar.

and functions that are based on [contextual confidence interval inference functions](https://github.com/gsbDBI/contextual_bandits_evaluation/blob/main/adaptive/inference.py):

- non_contextual_twopoint.
- we also create codes for two approach to estimate treatment effects (mentioned in paper [Hadad, Vitor, et al. (2021)](https://arxiv.org/abs/1911.02768)).

## Experiment
bernoulli_bandit_utils.R contains scripts to run experiments.

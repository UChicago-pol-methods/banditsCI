# Bandit Contextual Inference Functions

#' Compute AIPW/doubly robust scores.
#'
#' Computes AIPW/doubly robust scores based on observed rewards, pulled arms, and inverse
#' probability scores. If \code{mu_hat} is provided, compute AIPW scores, otherwise compute IPW scores.
#'
#' @param yobs Numeric vector. Observed rewards. Must not contain NA values.
#' @param ws Integer vector. Pulled arms. Must not contain NA values. Length must match \code{yobs}.
#' @param balwts Numeric matrix. Inverse probability score \eqn{1[W_t=w]/e_t(w)} of pulling arms, shape \code{[A, K]}, where \code{A} is the number of observations and \code{K} is the number of arms. Must not contain NA values.
#' @param K Integer. Number of arms. Must be a positive integer.
#' @param mu_hat Optional numeric matrix. Plug-in estimator of arm outcomes, shape \code{[A, K]}, or \code{NULL}. Must not contain NA values if provided.
#'
#' @return Numeric matrix. AIPW scores, shape \code{[A, K]}.
#'
#' @examples
#' aw_scores(yobs = c(0.5, 1, 0, 1.5),
#'           ws = c(1, 2, 2, 3),
#'           balwts = matrix(c(0.5, 2, 1, 0.5,
#'                             1, 1.5, 0.5, 1.5,
#'                             2, 1.5, 0.5, 1),
#'                             ncol = 3),
#'           K = 3,
#'           mu_hat = matrix(c(0.5, 0.8, 0.6, 0.3,
#'                             0.9, 0.2, 0.5, 0.7,
#'                             0.4, 0.8, 0.2, 0.6),
#'                             ncol = 3))
#'
#' @export
#' @import Rdpack
aw_scores <- function(yobs, ws, balwts, K, mu_hat = NULL) {
  # Compute AIPW/doubly robust scores. Return IPW scores if mu_hat is NULL.

  # Input Check
  if (!is.numeric(yobs) || any(is.na(yobs))) stop("yobs should be a numeric vector without NAs.")
  if (length(yobs) != length(ws)) stop("Lengths of yobs and ws should be equal.")
  if (!is.numeric(K) || (K != as.integer(K)) || K <= 0) stop("K should be a positive integer.")
  if (!is.matrix(balwts) || nrow(balwts) != length(yobs) || ncol(balwts) != K || any(is.na(balwts)))
    stop("balwts should be a numeric matrix of shape [length(yobs), K] without NAs.")
  if (!is.null(mu_hat)) {
    if (!is.matrix(mu_hat) || any(dim(mu_hat) != c(length(yobs), K)) || any(is.na(mu_hat)))
      stop("mu_hat should be a numeric matrix of shape [length(yobs), K] without NAs.")
  }

  expand <- function(mat, indices, ncol) {
    output <- matrix(0, nrow(mat), ncol)
    for (i in 1:nrow(mat)) {
      output[i, indices[i]] <- mat[i, indices[i]]
    }
    return(output)
  }

  scores <- expand(balwts * yobs, ws, K)
  if (!is.null(mu_hat)) {
    scores <- scores + (1 - expand(balwts, ws, K)) * mu_hat
  }
  return(scores)
}

#' Estimate policy value via non-contextual adaptive weighting.
#'
#' Estimates the value of a policy based on AIPW scores and a policy matrix using non-contextual
#' adaptive weighting. If \code{evalwts} is not provided, uses equal weights for all observations.
#'
#' @param scores Numeric matrix. AIPW scores, shape \code{[A, K]}, where \code{A} is the number of observations and \code{K} is the number of arms. Must not contain NA values.
#' @param policy Numeric matrix. Policy matrix \eqn{\pi(X_t, w)}, shape \code{[A, K]}. Must have the same shape as \code{scores} and must not contain NA values.
#' @param evalwts Optional numeric vector. Non-contextual adaptive weights \eqn{h_t}, length \code{A}, or \code{NULL}. Default is \code{NULL}.
#'
#' @return Numeric scalar. Estimated policy value.
#'
#' @examples
#' scores <- matrix(c(0.5, 0.8, 0.6,
#'                    0.3, 0.9, 0.2,
#'                    0.5, 0.7, 0.4,
#'                    0.8, 0.2, 0.6), ncol = 3, byrow = TRUE)
#' policy <- matrix(c(0.2, 0.3, 0.5,
#'                    0.6, 0.1, 0.3,
#'                    0.4, 0.5, 0.1,
#'                    0.2, 0.7, 0.1), ncol = 3, byrow = TRUE)
#' aw_estimate(scores = scores, policy = policy, evalwts = c(0.5, 1, 0.5, 1.5))
#'
#' @export
aw_estimate <- function(scores, policy, evalwts = NULL) {
  # Estimate policy value via non-contextual adaptive weighting.

  # Input Check
  if (!is.matrix(scores) || any(is.na(scores))) stop("scores should be a numeric matrix without NAs.")
  if (!is.matrix(policy) || any(dim(policy) != dim(scores)) || any(is.na(policy)))
    stop("policy should be a numeric matrix with the same shape as scores and without NAs.")

  if (is.null(evalwts)) {
    evalwts <- matrix(1, ncol = ncol(scores), nrow = nrow(scores))
  }

  return(sum(evalwts * rowSums(scores * policy)) / sum(evalwts))
}

#' Variance of policy value estimator via non-contextual adaptive weighting.
#'
#' Computes the variance of a policy value estimate based on AIPW scores, a policy matrix, and
#' non-contextual adaptive weights.
#'
#' @param scores Numeric matrix. AIPW scores, shape \code{[A, K]}. Must not contain NA values.
#' @param estimate Numeric scalar. Policy value estimate.
#' @param policy Numeric matrix. Policy matrix \eqn{\pi(X_t, w)}, shape \code{[A, K]}. Must have the same shape as \code{scores} and must not contain NA values.
#' @param evalwts Optional numeric vector. Non-contextual adaptive weights \eqn{h_t}, length \code{A}, or \code{NULL}.
#'
#' @return Numeric scalar. Variance of policy value estimate.
#'
#' @examples
#' scores <- matrix(c(0.5, 0.8, 0.6,
#'                    0.3, 0.9, 0.2,
#'                    0.5, 0.7, 0.4,
#'                    0.8, 0.2, 0.6), ncol = 3, byrow = TRUE)
#' policy <- matrix(c(0.2, 0.3, 0.5,
#'                    0.6, 0.1, 0.3,
#'                    0.4, 0.5, 0.1,
#'                    0.2, 0.7, 0.1), ncol = 3, byrow = TRUE)
#' estimate <- aw_estimate(scores = scores, policy = policy, evalwts = c(0.5, 1, 0.5, 1.5))
#' aw_var(scores = scores, estimate = estimate, policy = policy, evalwts = c(0.5, 1, 0.5, 1.5))
#'
#' @export
aw_var <- function(scores, estimate, policy, evalwts = NULL) {
  # Variance of policy value estimator via non-contextual adaptive weighting.

  # Input Check
  if (!is.matrix(scores) || any(is.na(scores))) stop("scores should be a numeric matrix without NAs.")
  if (!is.numeric(estimate) || length(estimate) != 1) stop("estimate should be a numeric scalar.")
  if (!is.matrix(policy) || any(dim(policy) != dim(scores)) || any(is.na(policy)))
    stop("policy should be a numeric matrix with the same shape as scores and without NAs.")

  if (is.null(evalwts)) {
    evalwts <- matrix(1, ncol = ncol(scores))
  }

  return(sum((rowSums(policy * scores) - estimate)^2 * evalwts^2) / sum(evalwts)^2)
}

#' Estimate/variance of policy evaluation via non-contextual weighting.
#'
#' Computes the estimate and variance of a policy evaluation based on non-contextual weights, AIPW scores,
#' and a policy matrix.
#'
#' @param w Numeric vector. Non-contextual weights, length \code{A}. Must not contain NA values.
#' @param gammahat Numeric matrix. AIPW scores, shape \code{[A, K]}. Must not contain NA values.
#' @param policy Numeric matrix. Policy matrix \eqn{\pi(X_t, w)}, shape \code{[A, K]}. Must have the same shape as \code{gammahat} and must not contain NA values.
#'
#' @return Named numeric vector with elements \code{estimate} and \code{var}, representing the estimated
#' policy value and the variance of the estimate, respectively.
#'
#' @examples
#' w <- c(0.5, 1, 0.5, 1.5)
#' scores <- matrix(c(0.5, 0.8, 0.6,
#'                    0.3, 0.9, 0.2,
#'                    0.5, 0.7, 0.4,
#'                    0.8, 0.2, 0.6), ncol = 3, byrow = TRUE)
#' policy <- matrix(c(0.2, 0.3, 0.5,
#'                    0.6, 0.1, 0.3,
#'                    0.4, 0.5, 0.1,
#'                    0.2, 0.7, 0.1), ncol = 3, byrow = TRUE)
#' gammahat <- scores - policy
#' estimate(w = w, gammahat = gammahat,
#' policy = policy)
#'
#' @export
estimate <- function(w, gammahat, policy) {
  # Return estimate and variance of policy evaluation via non-contextual weighting.

  # Input Check
  if (!is.numeric(w) || any(is.na(w))) stop("w should be a numeric vector without NAs.")
  if (!is.matrix(gammahat) || any(is.na(gammahat))) stop("gammahat should be a numeric matrix without NAs.")
  if (!is.matrix(policy) || any(is.na(policy))) stop("policy should be a numeric matrix without NAs.")
  if (any(dim(gammahat) != dim(policy)))
    stop("gammahat and policy should have the same shape.")
  if (length(w) != nrow(gammahat) || length(w) != nrow(policy))
    stop("Length of w should be equal to the number of rows in gammahat and policy.")

  estimate <- aw_estimate(gammahat, policy, w)
  var <- aw_var(gammahat, estimate, policy, w)

  return(c(`estimate` = estimate, `var` = var))
}

#' Estimate/variance of policy evaluation via contextual weighting.
#'
#' Computes the estimate and variance of a policy evaluation based on adaptive weights, AIPW scores,
#' and a policy matrix.
#'
#' @param h Numeric matrix. Adaptive weights \eqn{h_t(X_s)}, shape \code{[A, A]}. Must be a square matrix and must not contain NA values.
#' @param gammahat Numeric matrix. AIPW scores, shape \code{[A, K]}. Must not contain NA values.
#' @param policy Numeric matrix. Policy matrix \eqn{\pi(X_t, w)}, shape \code{[A, K]}. Must have the same shape as \code{gammahat} and must not contain NA values.
#'
#' @return Named numeric vector with elements \code{estimate} and \code{var}, representing the estimated
#' policy value and the variance of the estimate, respectively.
#'
#' @examples
#' h <- matrix(c(0.4, 0.3, 0.2, 0.1,
#'               0.2, 0.3, 0.3, 0.2,
#'               0.5, 0.3, 0.2, 0.1,
#'               0.1, 0.2, 0.1, 0.6), ncol = 4, byrow = TRUE)
#' scores <- matrix(c(0.5, 0.8, 0.6, 0.3,
#'                    0.9, 0.2, 0.5, 0.7,
#'                    0.4, 0.8, 0.2, 0.6), ncol = 3, byrow = TRUE)
#' policy <- matrix(c(0.2, 0.3, 0.5,
#'                    0.6, 0.1, 0.3,
#'                    0.4, 0.5, 0.1,
#'                    0.2, 0.7, 0.1), ncol = 3, byrow = TRUE)
#' gammahat <- scores - policy
#' calculate_continuous_X_statistics(h = h, gammahat = gammahat, policy = policy)
#'
#' @export
calculate_continuous_X_statistics <- function(h, gammahat, policy) {
  # Return estimate and variance of policy evaluation via contextual weighting.

  # Input Check
  if (!is.matrix(h) || nrow(h) != ncol(h) || any(is.na(h))) stop("h should be a square numeric matrix without NAs.")
  if (!is.matrix(gammahat) || any(is.na(gammahat))) stop("gammahat should be a numeric matrix without NAs.")
  if (!is.matrix(policy) || any(is.na(policy))) stop("policy should be a numeric matrix without NAs.")
  if (any(dim(gammahat) != dim(policy)))
    stop("gammahat and policy should have the same shape.")
  if (nrow(h) != nrow(gammahat) || nrow(h) != nrow(policy))
    stop("Number of rows in h should be equal to the number of rows in gammahat and policy.")

  A <- dim(h)[1]
  Z <- colSums(h)  # size (A) \sum_{s=1}^A h_s(X_t)
  gamma_policy <- rowSums(gammahat * policy)
  hi_Xi_Z <- h[cbind(1:A, 1:A)]
  hi_Xi_Z[Z > 1e-6] <- hi_Xi_Z[Z > 1e-6] / Z[Z > 1e-6]  # size (A), h_t(X_t) / Z(X_t)
  B <- hi_Xi_Z * gamma_policy
  h_Z <- h
  h_Z[, Z > 1e-6] <- sweep(h_Z[, Z > 1e-6], 2, Z[Z > 1e-6], `/`)

  estimate <- sum(B)
  var <- sum((B - colSums(h_Z * B))^2)

  return(c(`estimate` = estimate, `var` = var))
}

#' Policy evaluation with adaptively generated data.
#'
#' Calculates average response and differences in average response under counterfactual treatment policies.
#' Estimates are produced using provided inverse probability weighted (IPW) or augmented inverse probability weighted (AIPW) scores paired with various adaptive weighting schemes, as proposed in \insertCite{hadad2021confidence;textual}{banditsCI} and \insertCite{zhan2021off;textual}{banditsCI}.
#'\cr
#'\cr
#' We briefly outline the target quantities:
#' For observations indexed \eqn{t \in \{1,\dots,A\}}, treatments \eqn{w \in \{1,\dots,K\}}, we denote as \eqn{Y_t(w)} the potential outcome for the unit at time \eqn{t} under treatment \eqn{w}.
#' A policy \eqn{\pi} is a treatment assignment procedure that is the subject of evaluation, described in terms of treatment assignment probabilities for each subject to receive each counterfactual treatment.
#' We target estimation of average response under a specified policy:
#'\deqn{Q(\pi) := \sum_{w = 1}^{K}\textrm{E}\left[\pi(w)Y_t(w)\right]}
#' The user may specify a list of list of policies to be evaluated, under \code{policy1}.
#'\cr
#'\cr
#' Alternatively, they may estimate policy contrasts if \code{policy0} is provided:
#'\deqn{\Delta(\pi^1,\pi^2) := Q(\pi^1) - Q(\pi^2) }
#'
#' @param policy0 Optional matrix. Single policy probability matrix for contrast evaluation, dimensions \code{[A, K]}. Each row represents treatment assignment probabilities for an individual subject, and so rows must sum to 1. When \code{policy0 = NULL}, the function estimates the value \eqn{Q(\pi)} of each policy matrix listed in \code{policy1}. When \code{policy0} is non-null, the function estimates differences in average response under each of the component policies in \code{policy1} and the \emph{single} policy in \code{policy0}. Must not contain NA values if provided.
#' @param policy1 List of matrices. List of counterfactual policy matrices for evaluation, dimensions \code{[A, K]}. Each row represents treatment assignment probabilities for an individual subject, and so rows must sum to 1. Must not contain NA values.
#' @param contrasts Character. The method to estimate policy contrasts, either \code{combined} or \code{separate}, discussed in \insertCite{hadad2021confidence;textual}{banditsCI} Section 3. \code{combined} indicates the difference in (A)IPW scores is directly used as the unbiased scoring rule for \eqn{\Delta (\pi^1, \pi^2)}; \code{separate} indicates that scores are used separately \eqn{\hat \Delta (\pi^1, \pi^2) = \hat Q (w_1) - \hat Q (w_2)}.
#' @param gammahat (A)IPW scores matrix with dimensions \code{[A, K]} in non-contextual settings, or \code{[A, A, K]} contextual settings. Dimensions represent time, (contexts,) treatment arms. Dimensions of \code{gammahat} and \code{probs_array} must be the same. Must not contain NA values.
#' @param probs_array Numeric array. Probability matrix or array with dimensions \code{[A, K]} in non-contextual settings, or \code{[A, A, K]} contextual settings. Dimensions represent time, (contexts,) treatment arms. Dimensions of \code{gammahat} and \code{probs_array} must be the same. Must not contain NA values.
#' @param uniform Logical. Estimate uniform weights.
#' @param non_contextual_minvar Logical. Estimate non-contextual \code{MinVar} weights described in \insertCite{zhan2021off;textual}{banditsCI} Section 4.
#' @param contextual_minvar Logical. Estimate contextual \code{MinVar} weights described in \insertCite{zhan2021off;textual}{banditsCI} Section 4.
#' @param non_contextual_stablevar Logical. Estimate non-contextual \code{StableVar} weights described in \insertCite{zhan2021off;textual}{banditsCI} Section 4.
#' @param contextual_stablevar Logical. Estimate contextual \code{StableVar} weights  described in \insertCite{zhan2021off;textual}{banditsCI} Section 4.
#' @param non_contextual_twopoint Logical. Estimate \code{two-point} allocation weights described in \insertCite{hadad2021confidence;textual}{banditsCI} Section 2.
#' @param floor_decay Numeric. Floor decay parameter used in the calculation. Default is 0.
#'
#' @return A list of treatment effect estimates under different weighting schemes.
#' @references \insertRef{hadad2021confidence}{banditsCI}
#' @references \insertRef{zhan2021off}{banditsCI}
#'
#' @examples
#' set.seed(123)
#' # In a non-contextual setting, generate example values for policy1, gammahat, and probs_array
#' gammahat <- matrix(c(0.5, 0.8, 0.6,
#'                      0.3, 0.9, 0.2,
#'                      0.5, 0.7, 0.4,
#'                      0.8, 0.2, 0.6), ncol = 3, byrow = TRUE)
#' policy0 <- matrix(c(1, 0, 0,
#'                     1, 0, 0,
#'                     1, 0, 0,
#'                     1, 0, 0), ncol = 3, byrow = TRUE)
#' policy1 <- list(matrix(c(0, 1, 0,
#'                          0, 1, 0,
#'                          0, 1, 0,
#'                          0, 1, 0), ncol = 3, byrow = TRUE))
#'
#' probs_array <- array(0, dim = c(4, 4, 3))
#' for (i in 1:4) {
#'   temp_vector <- runif(3)
#'   normalized_vector <- temp_vector / sum(temp_vector)
#'   probs_array[i, 1, ] <- normalized_vector
#' }
#' for (k in 1:3) {
#'   for (i in 1:4) {
#'     temp_vector <- runif(3)
#'     normalized_vector <- temp_vector / sum(temp_vector)
#'     probs_array[i, 2:4, k] <- normalized_vector
#'   }
#' }
#' estimates <- output_estimates(policy1 = policy1,
#'                               policy0 = policy0,
#'                               gammahat = gammahat,
#'                               probs_array = probs_array)
#' # plot
#' plot_results <- function(result) {
#'   estimates <- result[, "estimate"]
#'   std.errors <- result[, "std.error"]
#'   labels <- rownames(result)
#'
#'   # Define the limits for the x-axis based on estimates and std.errors
#'   xlims <- c(min(estimates - 2*std.errors), max(estimates + 2*std.errors))
#'
#'   # Create the basic error bar plot using base R
#'   invisible(
#'     plot(estimates, 1:length(estimates), xlim = xlims, xaxt = "n",
#'          xlab = "Coefficient Estimate", ylab = "",
#'          yaxt = "n", pch = 16, las = 1, main = "Coefficients and CIs")
#'   )
#'
#'   # Add y-axis labels
#'   invisible(
#'     axis(2, at = 1:length(estimates), labels = labels, las = 1, tick = FALSE,
#'          line = 0.5)
#'   )
#'
#'   # Add the x-axis values
#'   x_ticks <- x_ticks <- seq(from = round(xlims[1], .5),
#'                             to = round(xlims[2], .5), by = 0.5)
#'   invisible(
#'     axis(1,
#'          at = x_ticks,
#'          labels = x_ticks)
#'   )
#'
#'   # Add error bars
#'   invisible(
#'     segments(estimates - std.errors,
#'              1:length(estimates),
#'              estimates + std.errors,
#'              1:length(estimates))
#'   )
#' }
#'
#' sample_result <- estimates[[1]]
#' op <- par(no.readonly = TRUE)
#' par(mar=c(5, 12, 4, 2))
#' plot_results(sample_result)
#' par(op)
#'
#' @export
output_estimates <- function(policy0 = NULL,
                             policy1,
                             contrasts = 'combined',
                             gammahat,
                             probs_array,
                             uniform = TRUE,
                             non_contextual_minvar = TRUE,
                             contextual_minvar = TRUE,
                             non_contextual_stablevar = TRUE,
                             contextual_stablevar = TRUE,
                             non_contextual_twopoint = TRUE,
                             floor_decay = 0) {
  # Input Check
  if (!is.null(policy0) && (!is.matrix(policy0) || any(is.na(policy0)))) stop("policy0 must be a matrix without NAs or NULL.")
  #  if (!all(abs(rowSums(policy0) - 1) < 1e-8)) stop("Rows of policy0 must sum to 1.")
  #  if (!all(abs(rowSums(policy1[[1]]) -1)< 1e-8)) stop("Rows of policy1 must sum to 1.")
  if (!is.character(contrasts) || !(contrasts %in% c('combined', 'separate'))) stop("contrasts must be either 'combined' or 'separate'")
  if (!is.matrix(gammahat) || any(is.na(gammahat))) stop("gammahat must be a matrix without NAs.")
  if (!is.array(probs_array) || any(is.na(probs_array))) stop("probs_array must be an array without NAs.")
  # Determine the dimensionality of probs_array
  if (length(dim(probs_array)) == 3) {
    # Contextual case
    # Ensure the rows of probs in probs_array sum to 1
    if (!all(abs(rowSums(probs_array[,1,]) - 1) < 1e-8)) {
      stop("Rows of the probs must sum to 1")
    }
  } else if (length(dim(probs_array)) == 2) {
    # Non-contextual case
    # Ensure the sum of each row is 1
    if (!all(abs(apply(probs_array, 1, sum) - 1) < 1e-8)) {
      stop("Rows of the non-contextual probs must sum to 1")
    }
  } else {
    stop("probs_array must either be two-dimensional (non-contextual) or three-dimensional (contextual)")
  }


  if (!is.logical(uniform) || !is.logical(non_contextual_minvar) || !is.logical(contextual_minvar) || !is.logical(non_contextual_stablevar) || !is.logical(contextual_stablevar) || !is.logical(non_contextual_twopoint)) stop("The logical flags must be logical values.")

  A <- nrow(gammahat)
  K <- ncol(gammahat)
  .check_A(A)
  .check_shape(gammahat, probs_array)
  if(contrasts == 'combined' | is.null(policy0)){
    # Now we are using the first approach: use the difference in AIPW scores as the unbiased scoring rule for \eqn{\Delta (w_1, w_2)}
    if (length(dim(probs_array))==2){
      z <- array(0,dim=c(nrow(probs_array), nrow(probs_array), ncol(probs_array)))
      for(j in 1:nrow(probs_array)){
        z[j,,]=probs_array
      }
      probs_array <- z
    }
    results <- lapply(1:length(policy1), function(x) {
      out_mat <- matrix(NA, ncol = 2, nrow = 6)
      colnames(out_mat) <- c('estimate', 'std.error')
      rownames(out_mat) <- c('uniform',
                             'non_contextual_minvar',
                             'contextual_minvar',
                             'non_contextual_stablevar',
                             'contextual_stablevar',
                             'non_contextual_twopoint')
      out_mat
    })

    if (is.null(policy0)) {
      policy0 <- matrix(0, nrow = A, ncol = ncol(gammahat))
    }

    if(non_contextual_twopoint) {
      if (length(dim(probs_array)) == 3) {
        # For contextual probs_array (three-dimensional)
        e <- t(sapply(1:A, function(x) probs_array[x, x, ]))
        twopoint_ratio <- twopoint_stable_var_ratio(A = A, e = e, alpha = floor_decay)
        twopoint_h2es <- stick_breaking(twopoint_ratio)
        wts_twopoint <- sqrt(ifelse_clip(twopoint_h2es * e, 0, twopoint_h2es * e))
      } else if (length(dim(probs_array)) == 2) {
        # For non-contextual probs_array (two-dimensional)
        e <- probs_array
        twopoint_ratio <- twopoint_stable_var_ratio(A = A, e = e, alpha = floor_decay)
        twopoint_h2es <- stick_breaking(twopoint_ratio)
        wts_twopoint <- sqrt(ifelse_clip(twopoint_h2es * e, 0, twopoint_h2es * e))
      }

    }

    for (j in 1:length(policy1)) {
      policy <- policy1[[j]] - policy0

      mask <- matrix(1, nrow = A, ncol = A)
      for (i in 2:A) {
        mask[i, i:A] <- 0
      }

      all_condVars <- sapply(1:A,
                             function(x) rowSums(sweep(1/probs_array[,x,],
                                                       MARGIN = 2, policy[x,]^2, `*`)))
      all_condVars_inverse <- matrix(0,
                                     ncol = A,
                                     nrow = A)
      all_condVars_inverse[all_condVars > 1e-6] <- 1 / all_condVars[all_condVars > 1e-6]
      expected_condVars <- rowSums(all_condVars * mask)/rowSums(mask)

      expected_condVars_inverse <- expected_condVars
      expected_condVars_inverse[] <- 0
      expected_condVars_inverse[expected_condVars > 1e-6] <- 1 / expected_condVars[expected_condVars > 1e-6]

      if(uniform){
        # AIPW (uniform weights)
        result <- estimate(matrix(1, ncol = 1, nrow = A), gammahat, policy)
        result['std.error'] <- sqrt(result['var'])
        results[[j]]['uniform',] <- result[c('estimate', 'std.error')]
      }

      if(non_contextual_minvar){
        # non-contextual minvar (propscore expected)
        result <- estimate(expected_condVars_inverse,
                           gammahat, policy)
        result['std.error'] <- sqrt(result['var'])
        results[[j]]['non_contextual_minvar',] <- result[c('estimate', 'std.error')]
      }

      if(contextual_minvar){
        # contextual minvar (propscore X)
        result <- calculate_continuous_X_statistics(all_condVars_inverse,
                                                    gammahat, policy)
        result['std.error'] <- sqrt(result['var'])
        results[[j]]['contextual_minvar',] <- result[c('estimate', 'std.error')]
      }

      if(non_contextual_stablevar){
        # non-contextual stablevar (lvdl expected)
        result <- estimate(sqrt(expected_condVars_inverse),
                           gammahat, policy)
        result['std.error'] <- sqrt(result['var'])
        results[[j]]['non_contextual_stablevar',] <- result[c('estimate', 'std.error')]
      }
      if(contextual_stablevar){
        # contextual stablevar (ldvl X)
        result <- calculate_continuous_X_statistics(sqrt(all_condVars_inverse),
                                                    gammahat, policy)
        result['std.error'] <- sqrt(result['var'])
        results[[j]]['contextual_stablevar',] <- result[c('estimate', 'std.error')]
      }
      if(non_contextual_twopoint){
        # non_contextual two_point
        ## Compute weights: Two-point allocation rate
        evalwts <- rowSums(policy * wts_twopoint) #TODO check that this is correct for contrasts
        result <- estimate(evalwts, gammahat, policy)
        result['std.error'] <- sqrt(result['var'])
        results[[j]]['non_contextual_twopoint',] <- result[c('estimate', 'std.error')]
      }
    }
    return(results)

    # The second approach takes asymptotically normal inference about \eqn{\Delta(w_1, w_2)}: \eqn{\hat\Delta (w_1, w_2) = \hat Q (w_1) - \hat Q (w_2)}
  } else if (contrasts == 'separate'){
    # control estimates
    out_full0 = output_estimates(
      policy0 = NULL,
      policy1 = list(policy0),
      gammahat = gammahat,
      probs_array = probs_array,
      uniform = uniform,
      non_contextual_minvar = non_contextual_minvar,
      contextual_minvar = contextual_minvar,
      non_contextual_stablevar = non_contextual_stablevar,
      contextual_stablevar = contextual_stablevar,
      non_contextual_twopoint = non_contextual_twopoint,
      floor_decay = floor_decay
    )

    out_full1 <- output_estimates(
      policy0 = NULL,
      policy1 = policy1,
      gammahat = gammahat,
      probs_array = probs_array,
      uniform = uniform,
      non_contextual_minvar = non_contextual_minvar,
      contextual_minvar = contextual_minvar,
      non_contextual_stablevar = non_contextual_stablevar,
      contextual_stablevar = contextual_stablevar,
      non_contextual_twopoint = non_contextual_twopoint,
      floor_decay = floor_decay
    )

    out_full_te2 <- lapply(out_full1, function(x){
      # estimate the difference in means
      x_mean <- x[,'estimate'] - out_full0[[1]][,'estimate']
      # estimate the variance
      x_var <- sqrt(x[,"std.error"]^2 + out_full0[[1]][,"std.error"]^2) # Calculate the standard error. Function (24) in Zhan et al. 2021
      cbind('estimate' = x_mean, 'std.error' = x_var)
    })
    return(out_full_te2)
  }
}

#' Stick breaking function.
#'
#' Implements the stick breaking algorithm for calculating weights in the stable-var scheme.
#'
#' @param Z Numeric array. Input array, shape \code{[A, K]}. Must not contain NA values.
#'
#' @return Numeric array. Stick breaking weights, shape \code{[A, K]}. Must not contain NA values.
#'
#' @examples
#' set.seed(123)
#' Z <- array(runif(10), dim = c(2, 5))
#' stick_breaking(Z)
#'
#' @export
stick_breaking <- function(Z) {
  # Stick breaking algorithm in stable-var weights calculation

  # Input Check
  if (!is.matrix(Z) && !is.array(Z)) stop("Input Z must be a matrix or an array.")
  if (!is.numeric(Z)) stop("Elements of Z must be numeric.")

  A <- dim(Z)[1]
  K <- dim(Z)[2]
  weights <- array(0, dim = c(A, K))
  weight_sum <- rep(0, times = K)
  for (a in 1:A) {
    weights[a, ] <- Z[a, ] * (1 - weight_sum)
    weight_sum <- weight_sum + weights[a, ]
  }
  return(weights)
}

#' Clip lamb values between a minimum x and maximum y.
#'
#' Clips a numeric vector between two values.
#'
#' @param lamb Numeric vector. Values to be clipped.
#' @param x Numeric. Lower bound of the clip range.
#' @param y Numeric. Upper bound of the clip range.
#'
#' @return Numeric vector. Clipped values.
#'
#' @examples
#' lamb <- c(1, 2, 3, 4, 5)
#' ifelse_clip(lamb, 2, 4)
#'
#' @export
ifelse_clip <- function(lamb, x, y) {
  # Input Check
  if (!is.numeric(lamb)) stop("lamb must be a numeric vector.")
  if (!is.numeric(x) || length(x) != 1) stop("x must be a numeric value.")

  ifelse(lamb <= x, x, ifelse(lamb >= y, y, lamb))
}

#' Calculate allocation ratio for a two-point stable-variance bandit.
#'
#' Calculates the allocation ratio for a two-point stable-variance bandit, given the empirical mean and the discount parameter alpha.
#'
#' @param A Integer. Size of the experiment. Must be a positive integer.
#' @param e Numeric. Empirical mean. Must be a numeric value.
#' @param alpha Numeric. Discount parameter.
#'
#' @return Numeric vector. Allocation ratio lambda.
#'
#' @examples
#' # Calculate the allocation ratio for a two-point stable-variance bandit with e=0.1 and alpha=0.5
#' twopoint_stable_var_ratio(1000, 0.1, 0.5)
#'
#' @export
twopoint_stable_var_ratio <- function(A, e, alpha){
  # Input Check
  if(!is.numeric(A) || length(A) != 1 || A <= 0) stop("A must be a positive numeric value")

  a <- 1:A
  bad_lambda <- (1 - alpha) / ((1 - alpha) + A*(a/A)^alpha - a)

  # good arm, e large
  good_lambda <- 1 / (1 + A - a)

  stopifnot(bad_lambda + 1e-7 >= good_lambda) # the 1e-7 is for numerical issues

  # weighted average of both
  lamb <- (1 - e) * bad_lambda + e * good_lambda

  stopifnot(lamb >= 0)
  stopifnot(lamb <= 1 + 1e-8)
  lamb <- ifelse_clip(lamb, 0, 1)
  return(lamb)
}


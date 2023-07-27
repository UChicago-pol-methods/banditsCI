# Bandit Contextual Inference Functions 

aw_scores <- function(yobs, ws, balwts, K, mu_hat=NULL) {
  # Compute AIPW/doubly robust scores. Return IPW scores if muhat is NULL.
  # INPUT
  #     - yobs: observed rewards, shape [A]
  #     - ws: pulled arms, shape [A]
  #     - balwts: inverse probability score 1[W_t=w]/e_t(w) of pulling arms, shape [A, K]
  #     - K: number of arms
  #     - mu_hat: plug-in estimator of arm outcomes, shape [A, K]
  # OUTPUT
  #     - scores: AIPW scores, shape [A, K]
  
  expand <- function(mat, indices, ncol) {
    output <- matrix(0, nrow(mat), ncol)
    for (i in 1:nrow(mat)) {
      output[i, indices[i]] <- mat[i, indices[i]]
    }
    return(output)
  }
  scores <- expand(balwts * yobs, ws, K) 
    # Y[t]*W[t]/e[t] term
    if (!is.null(mu_hat)){
      scores <- scores + (1 - expand(balwts, ws, K)) * mu_hat
    }
  return(scores)
}

aw_estimate <- function(scores, policy, evalwts=NULL){  
  # Estimate policy value via non-contextual adaptive weighting.
  # 
  # INPUT
  #     - scores: AIPW score, shape [A, K]
  #     - policy: policy matrix pi(X_i, w), shape [A, K]
  #     - evalwts: non-contextual adaptive weights h_i, shape [A]
  # OUTPUT
  #     - estimated policy value.
  if(is.null(evalwts)){
    evalwts <- matrix(1, ncol = ncol(scores), nrow = nrow(scores))  
  }
  
  return(sum(evalwts*rowSums(scores * policy))/sum(evalwts)) 
}

aw_var <- function(scores, estimate, policy, evalwts=NULL){
  # Variance of policy value estimator via non-contextual adaptive weighting.
  # 
  # INPUT
  #     - scores: AIPW score, shape [A, K]
  #     - estimate: policy value estimate
  #     - policy: policy matrix pi(X_i, w), shape [A, K]
  #     - evalwts: non-contextual adaptive weights h_i, shape [A, 1]
  # OUTPUT
  #     - variance of policy value estimate
  # 
  # var =  sum[i=0 to A] h[i]^2 * (sum[w] scores[i, w] * policy[i, w] - estimate)^2 
  #   __________________________________________________________________________
  #                       (sum[i=0 to A] h[i])^2
  
  if(is.null(evalwts)){
    evalwts <- matrix(1, ncol = ncol(scores)) 
  }
  
  return(sum((rowSums(policy * scores)-estimate)^2*evalwts^2)/sum(evalwts)^2) 
}

estimate <- function(w, gammahat, policy){
  # Return estimate and variance of policy evaluation via non-contextual weighting.
  # 
  # INPUT
  #     - w: non-contextual weights of shape [A]
  #     - gammahat: AIPW score of shape [A, K]
  #     - policy: policy matrix pi(X_i, w), shape [A, K]
  # 
  # OUTPUT
  #     - vector (estimate, var)
  estimate <- aw_estimate(gammahat, policy, w)
  var <- aw_var(gammahat, estimate, policy, w)
  
  return(c(`estimate` = estimate, `var` = var)) 
}

calculate_continuous_X_statistics <- function(h, gammahat, policy){
  # Return estimate and variance of policy evaluation via contextual weighting.
  # 
  # INPUT
  # - h: adaptive weights h_i(X_s) of size (A, A) 
  # - gammahat: AIPW score of shape [A, K]
  # - policy: policy matrix pi(X_i, w), shape [A, K]
  # 
  # OUTPUT:
  #   - vector (estimate, var)
  A <- dim(h)[1] 
  Z <- colSums(h)  # size (A) \sum_{s=1}^A h_s(X_i)
  gamma_policy <- rowSums(gammahat * policy)
  hi_Xi_Z <- h[cbind(1:A, 1:A)]
  hi_Xi_Z[Z > 1e-6] <- hi_Xi_Z[Z > 1e-6] / Z[Z > 1e-6]  # size (A), h_i(X_i) / Z(X_i)
  B <- hi_Xi_Z * gamma_policy
  h_Z <- h
  h_Z[, Z > 1e-6] <- sweep(h_Z[, Z > 1e-6], 2, Z[Z > 1e-6], `/`)
  
  estimate <- sum(B)
  var <- sum((B - colSums(h_Z*B))^2)
  
  return(c(`estimate` = estimate, `var` = var))
}

output_estimates <- function(policy0 = NULL, 
                             policy1, 
                             contrasts = 'combined',
                             gammahat, 
                             contextual_probs, 
                             uniform = TRUE,
                             non_contextual_minvar = TRUE,
                             contextual_minvar = TRUE,
                             non_contextual_stablevar = TRUE,
                             contextual_stablevar = TRUE,
                             non_contextual_twopoint = TRUE){
  # policy0: A * K control policy matrix for contrast evaluation, with probabilities under control
  ## when policy0 = NULL, the function is estimating the value Q(w) of a single arm w
  ## when policy0 doesn't equal to NULL, the function is estimating treatment effects of policies as compared to control \delta(w_1, w_2), using the difference in AIPW scores as the unbiased scoring rule for \delta (w_1, w_2)
  # policy1: list of A * K counterfactual treatment policy matrices for evaluation, with assignment probabilities under each policy 
  # contrasts: define the approach to estimate treatment effects. 'combined' indicates the first approach -- the difference in AIPW scores as the unbiased scoring rule for \delta (w_1, w_2); 'separate' indicates the second approach -- \delta ^ hat (w_1, w_2) = Q ^ hat (w_1) - Q ^ hat (w_2)
  # gammahat: scores matrix
  # contextual_probs: A * A * K matrix for contextual probabilities, with dimensions representing time, contexts, treatment arms 
  # uniform: logical, estimate uniform weights
  # non_contextual_minvar: logical, estimate non-contextual minvar weights
  # contextual_minvar: logical, estimate contextual minvar weights
  # non_contextual_stablevar: logical, estimate non-contextual stablevar weights
  # contextual_stablevar: logical, estimate contextual stablevar weights
  
  if(contrasts == 'combined'){
    # Now we are using the first approach: use the difference in AIPW scores as the unbiased scoring rule for \delta (w_1, w_2)
    if (length(dim(contextual_probs))==2){
      z <- array(0,dim=c(nrow(contextual_probs), nrow(contextual_probs), ncol(contextual_probs)))
      for(j in 1:nrow(contextual_probs)){
        z[j,,]=contextual_probs
      }
      contextual_probs <- z
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
    
    if(is.null(policy0)){
      policy0 <- matrix(0, nrow = A, ncol = ncol(gammahat))
    }
    
    if(non_contextual_twopoint){
      e <- t(sapply(1:A, function(x) contextual_probs[x,x,])) 
      twopoint_ratio <- twopoint_stable_var_ratio(e=e, alpha=0) 
      twopoint_h2es <- stick_breaking(twopoint_ratio)
      wts_twopoint <- sqrt(ifelse_clip(twopoint_h2es * e, 0, twopoint_h2es * e))
    }
    
    for(j in 1:length(policy1)){
      
      policy <- policy1[[j]] - policy0
      
      # Reciprocal of interior of (10) in Zhan et al. 2021
      mask <- matrix(1, nrow = A, ncol = A)
      
      for(i in 2:A){
        mask[i, i:A] <- 0
      }
      
      all_condVars <- sapply(1:A, 
                             function(x) rowSums(sweep(1/contextual_probs[,x,], 
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
        result <- estimate(matrix(1:A, ncol = 1), gammahat, policy)
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
    
    # The second approach takes asymptotically normal inference about \delta(w_1, w_2): \delta ^ hat (w_1, w_2) = Q ^ hat (w_1) - Q ^ hat (w_2)
  } else if (contrasts == 'separate'){
    out_full0 <- out_full[[1]]
    out_full1 <- out_full
    out_full1[[1]] <- NULL 
    out_full_te2 <- lapply(out_full1, function(x){
      # estimate the difference in means
      x_mean <- x[,'estimate'] - out_full0[,'estimate'] 
      # estimate the variance
      x_var <- sqrt(x[,"std.error"]^2 + out_full0[,"std.error"]^2) # Calculate the standard error. Function (24) in Zhan et al. 2021
      cbind('estimate' = x_mean, 'std.error' = x_var)
    })
    return(out_full_te2)
  }
}

# Two-point allocation scheme
stick_breaking <- function(Z){
  # Stick breaking algorithm in stable-var weights calculation
  #
  # Input
  #   Z: input array of shape [A, K]
  # Output
  #   weights: stick_breaking weights of shape [A, K]
  weights <- array(0, dim = c(A, K))
  weight_sum <- rep(0, times = K)
  for (a in 1:A) {
    weights[a,] <- Z[a,] * (1 - weight_sum)
    weight_sum <- weight_sum + weights[a,]
  }
  return(weights)
}

# Compute two-point weights

## Clip lamb values between a minimum x and maximum y
ifelse_clip <- function(lamb, x, y) {
  ifelse(lamb <= x,  x, ifelse(lamb >= y, y, lamb))
}


twopoint_stable_var_ratio <- function(e, alpha){
  a <-  c(length(A+1), 1)
  # bad arm, e small
  # this rearranging of the formula in the paper seems to be slightly more
  # numerically accurate. the exact formula in the paper occasionally produces 
  # weights that are just a little over 1 (which is impossible).
  bad_lambda <- (1 - alpha) / ((1 - alpha) + A*(a/A)^alpha - a)
  
  # good arm, e large 
  good_lambda <- 1 / (1 + A - a)                        
  
  stopifnot(bad_lambda + 1e-7 >= good_lambda) # the 1e-7 is for numerical issues
  
  # weighted average of both
  lamb <- (1 - e) * bad_lambda + e * good_lambda
  
  # Sometimes, due to numerical issues the lambdas end up very slightly above 1.
  # This clipping ensures that everything is okay.
  stopifnot(lamb >= 0)
  stopifnot(lamb <= 1 + 1e-8)
  lamb <- ifelse_clip(lamb, 0, 1)
  return(lamb)
}


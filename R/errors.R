#' @export
.check_first_batch <- function(batch_sizes, ys) {
  err <- paste0('First batch size must be greater than the number of treatment arms. ',
               'First batch size is: ', batch_sizes[1],
               '. Number of counterfactual conditions is: ', dim(ys)[2],'.')
  if(batch_sizes[1]>=dim(ys)[2]) return(NULL)
  stop(err)
}


#' @export
.check_A <- function(A) {
  err <- paste0('Number of observations must be greater than 1 to conduct inference. ',
                'Number of observations is: ', A,'.')
  if(A>1) return(NULL)
  stop(err)
}

#' @export
.check_shape <- function(gammahats, contextual_probs) {
  if (!identical(dim(gammahats), dim(contextual_probs))) {
    stop("Error: gammahats and contextual_probs must have the same shape.")
  }

  if (length(gammahats) <= 1 || length(contextual_probs) <= 1) {
    stop("Error: gammahats and contextual_probs must have a length greater than 1.")
  }
}

#' @export
.check_shape <- function(gammahat, contextual_probs) {
  if (!is.matrix(gammahat) || !is.matrix(contextual_probs)) {
    stop("Both gammahats and contextual_probs should be matrices.")
  }

  shape_gammahats <- dim(gammahat)
  shape_contextual_probs <- dim(contextual_probs)

  if (length(shape_gammahat) != 2 || length(shape_contextual_probs) != 2) {
    stop("Both gammahats and contextual_probs should have two dimensions.")
  }

  if (any(shape_gammahat != shape_contextual_probs)) {
    stop("gammahats and contextual_probs should have the same shape.")
  }
}

#' @export
.check_gammahat <- function(gammahat) {
  if (!is.matrix(gammahat)) {
    stop("gammahat should be a matrix.")
  }
}

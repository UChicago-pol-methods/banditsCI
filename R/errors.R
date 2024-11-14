#' Check First Batch Validity
#'
#' This function checks if the first batch size is greater than or equal to the number of treatment arms.
#'
#' @param batch_sizes A numeric vector specifying batch sizes.
#' @param ys A matrix of counterfactual conditions.
#' @return Returns NULL if the first batch size is valid; otherwise, throws an error.
#' @export
.check_first_batch <- function(batch_sizes, ys) {
  err <- paste0('First batch size must be greater than the number of treatment arms. ',
                'First batch size is: ', batch_sizes[1],
                '. Number of counterfactual conditions is: ', dim(ys)[2],'.')
  if(batch_sizes[1]>=dim(ys)[2]) return(NULL)
  stop(err)
}

#' Check Number of Observations for Inference
#'
#' This function checks if the number of observations is greater than 1, which is required for conducting inference.
#'
#' @param A An integer representing the number of observations.
#' @return Returns NULL if the number of observations is valid; otherwise, throws an error.
#' @export
.check_A <- function(A) {
  err <- paste0('Number of observations must be greater than 1 to conduct inference. ',
                'Number of observations is: ', A,'.')
  if(A>1) return(NULL)
  stop(err)
}

#' Check Shape Compatibility of Probability Objects
#'
#' This function checks the dimensional compatibility of `gammahats` and `contextual_probs`. It validates the dimensions and types based on whether the probability objects are contextual or non-contextual.
#'
#' @param gammahats A matrix representing estimates.
#' @param contextual_probs An object representing probabilities, either a matrix (non-contextual) or an array (contextual).
#' @return Returns NULL if the shapes and types are valid; otherwise, throws an error.
#' @export
.check_shape <- function(gammahats, contextual_probs) {
  ndim <- length(dim(contextual_probs))
  if(!(ndim %in% c(2,3))){
    stop(
      paste0("Error: probability objects should have two dimensions when non-contextual, or three dimensions when accounting for contexts. Number of dimensions is: ", ndim,'.'))
  }
  if(ndim == 2){
    if (!is.matrix(gammahats) || !is.matrix(contextual_probs)) {
      stop("Error: when using two-dimensional probability objects, both gammahats and contextual_probs should be matrices.")
    }
    if (!identical(dim(gammahats), dim(contextual_probs))) {
      stop("Error: when using two-dimensional probability objects, gammahats and contextual_probs must have the same shape.")
    }
  }
  if(ndim == 3){
    if (!is.matrix(gammahats) || !is.array(contextual_probs)) {
      stop("Error: when using three-dimensional probability objects, gammahats should be a matrix and contextual_probs should be an array.")
    }
    if (!(identical(dim(gammahats)[1],
                    dim(contextual_probs)[1],
                    dim(contextual_probs)[2]) &
          identical(dim(gammahats)[2],
                    dim(contextual_probs)[3])) ) {
      stop("Error: when using three-dimensional probability objects, the first dimension of gammahats and the first two dimensions of contextual_probs must be identical sizes; the second dimension of gammahats and the third dimension of contextual_probs must also be identical sizes.")
    }
  }
  if (length(gammahats) <= 1 || length(contextual_probs) <= 1) {
    stop("Error: gammahats and contextual_probs must have a length greater than 1.")
  }
}

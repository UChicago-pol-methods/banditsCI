#' @export
check_first_batch <- function(batch_sizes, ys) {
  err <- paste0('First batch size must be greater than the number of treatment arms. ',
               'First batch size is: ', batch_sizes[1],
               '. Number of counterfactual conditions is: ', dim(ys)[2],'.')
  if(batch_sizes[1]>=dim(ys)[2]) return(NULL)
  stop(err)
}


#' @export
check_A <- function(A) {
  err <- paste0('Number of observations must be greater than 1 to conduct inference. ',
                'Number of observations is: ', A,'.')
  if(A>1) return(NULL)
  stop(err)
}

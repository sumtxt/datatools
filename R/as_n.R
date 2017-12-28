#' Wrapper around as.numeric()
#' 
#' @param x numeric vector
#' 
#' @return numeric \code{vector}. 
#' 
#' 
#' @examples 
#'  \dontrun{
#'
#'  as_n(rpois(10, 5))
#'  
#'  }
#' 
#' 
#' @export
as_n <- function(x) as.numeric(x)
#' Rescales vector to base value 
#' 
#' @param x numeric vector
#' @param base value that will equal 1 
#' 
#' @return standardized \code{vector}. 
#' 
#' @details 
#' The function rescales all values such that the value \code{base} in \code{x} equals 1 in the returned vector. 
#' 
#' @examples 
#' 
#'  x <- sort(rpois(10, 5))
#'  set_base(x,3)
#'  
#' 
#' 
#' @export
set_base <- function(v, base){
	return ((-1 * (base - v) ) + 1)
	}

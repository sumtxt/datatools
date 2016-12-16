#' Function to discretize a variable based on a selected quantile
#' 
#' @param x variable to discretize
#' @param q probability corresponding to quantile 
#' 
#' @return \code{vector} with a discretized version of x. 
#' 
#' @examples 
#'  \dontrun{
#' 
#'  x <- runif(100,-1,1)
#'  y <- x^2 + rnorm(100)
#'  
#'  summary( lm(y ~ Q(x,0.25) ) )
#'  summary( lm(y ~ Q(x,0.75) ) )
#'  
#'  }
#' 
#' @export
Q <- function(x,q){
	q_ <- quantile(x,probs=q)
	return(as.numeric(x > q_))
	}
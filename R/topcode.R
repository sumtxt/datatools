#' Top-coding values in a vector 
#' 
#' @param x numeric vector
#' @param probs scalar of the quantile cut-point. 
#' 
#' @return \code{vector} top-coded vector. 
#' 
#' @details 
#' This function sets all values larger than the selected quantile to the largest value of the remaining values. 
#' 
#' @examples 
#'  \dontrun{
#' 
#'  x <- sort(rnorm(10))
#'  topcode(x)
#'  
#'  }
#' 
#' @export
topcode <- function(x,probs=0.99){
	tobetopcoded <- as.numeric(x > quantile(x,probs,na.rm=TRUE))
	x[tobetopcoded==1 & !is.na(tobetopcoded)] <- max(x[tobetopcoded==0 & !is.na(tobetopcoded)])
	return(x)
	}

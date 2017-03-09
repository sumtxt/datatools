#' Standardize a Vector 
#' 
#' @param x numeric vector
#' @param scale scale variable by twice its standard deviation?
#' 
#' @return standardized \code{vector}. 
#' 
#' @details 
#' The function mean-centers values in the vector and scale them by twice their standard deviation. 
#' The function ignores all missing values. 
#' 
#' @examples 
#'  \dontrun{
#' 
#'  x <- runif(100, 0, 1.2)
#'  xz <- standardize2(x)
#'  sd(xz)
#'  
#'  }
#' 
#' @references 
#'
#' Gelman, A. (2008). Scaling Regression Inputs by Dividing by two Standard Deviations. Statistics in Medicine 27(2865–2873).
#' 
#' @export
standardize2 <- function(x,scale=TRUE)  {
		x <- x-mean(x,na.rm=TRUE)
		if (scale == TRUE) return(x/(2*sd(x,na.rm=TRUE)))
		return(x)
		}
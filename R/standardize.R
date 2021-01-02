#' Standardize a Vector 
#' 
#' @param x numeric vector
#' @param scale scale variable by (twice) its standard deviation?
#' 
#' @return standardized \code{vector}. 
#' 
#' @details 
#' \code{standardize} centers values and scales them by their standard deviation. \code{standardize2} does the same but scales by twice their standard deviation. 
#' 
#' @examples 
#' 
#'  x <- runif(100, 0, 1.2)
#'  xz <- standardize2(x)
#'  sd(xz)
#'  
#'  xz <- standardize(x)
#'  sd(xz)
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

#' @rdname standardize2
#' @export
standardize <- function(x,scale=TRUE)  {
		x <- x-mean(x,na.rm=TRUE)
		if (scale == TRUE) return(x/sd(x,na.rm=TRUE))
		return(x)
		}
#' Discretize a variable either by user-defined breaks or by quantiles
#' 
#' @param x vector to discretize 
#' 
#' @param breaks vector of breaks 
#' 
#' @param quantiles vector of quantiles or number of evenly spaced quantiles 
#' 
#' @return integer vector
#' 
#' @details
#' If no breaks or quantiles are given two bins are formed with \code{mean(x)} as break. 
#' 
#' 
#' @examples 
#'  \dontrun{
#' 
#'  require(dplyr)
#' 
#'  w <- sort(rnorm(10))
#'  make_bins(w,breaks=c(-1,0,1))
#' 
#'  } 
#' 
#' 
#' 
#' @export
make_bins <- function(x,breaks=NULL,quantiles=NULL){
	if(!is.null(breaks) & !is.null(quantiles)) stop("Either supply breaks or quantiles, not both.")
	if(is.null(breaks) & is.null(quantiles)) breaks <- mean(x)
	if(is.null(breaks) & !is.null(quantiles) ){
		if( length(quantiles)==1 ){
				if( quantiles>=1) {
					K <- quantiles+2
					p <- seq(0,1,length=K)[c(-1,-K)]
					breaks <- quantile(x,probs=p)
				} else {
					breaks <- as.vector(quantile(x,probs=quantiles))
				}
		} else {
			breaks <- as.vector(quantile(x,probs=quantiles))
		}
	}
	z <- cut(x, breaks=c(-Inf,breaks,Inf),labels=FALSE)
	return(z)
	}
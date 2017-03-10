#' Compute descriptive statistics
#' 
#' @param data a \code{data.frame}
#' @param ... variable names (unquote) for which to compute the descriptive statistics 
#' 
#' @return \code{data.frame} with summary statistics. 
#' 
#' @details 
#' If no a single variable name is passed to the function, it computes 
#' descriptive statistics across all variables. Before computing all 
#' variables are coerced via \code{as.numeric()}. Missing values are ignored 
#' (\code{na.rm=TRUE}). 
#' 
#' @examples 
#'  \dontrun{
#' 
#'  x <- rpois(100,10)
#'	y <- rnorm(100,0, 2)
#'	z <- rexp(100, 1)
#'	dat <- data.frame(x=x, y=y, z=z)
#'  descriptives(dat, z, x)
#'  descriptives(dat)
#' 
#'  }
#' 
#' @export
descriptives <- function(data, ... ){
	data <- as.data.frame(data)
	obj <- as.list(substitute(list(...)))[-1L]
	varlist <- as.character(obj)
	if (length(varlist)==0) varlist <- colnames(data)
	nv <- varlist[!(varlist %in% colnames(data))] 
	if ( length(nv)!=0 ) stop(paste("Variable missing:", nv, sep=" ") )
	desc <- lapply(varlist, function(v) {
	    vals <- as.numeric(data[,v])
	    mu <- mean(vals,na.rm=TRUE)
	    med <- median(vals,na.rm=TRUE)
	    sd <- sd(vals,na.rm=TRUE)
	    q025 <- quantile(vals, probs=0.025,na.rm=TRUE)
	    q975 <- quantile(vals, probs=0.975,na.rm=TRUE)
	    min <- min(vals, na.rm=TRUE)
	    max <- max(vals, na.rm=TRUE)
	    miss <- sum(is.na(vals))/length(vals)
	    res <- data.frame(var=v, mu=mu, med=med, sd=sd, 
	    		q025=q025, q975=q975, min=min, max=max, miss=miss)
	    rownames(res) <- NULL
		return(res)
		})
	desc <- do.call(rbind, desc)
	return(desc)
	}


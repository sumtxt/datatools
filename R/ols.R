#' This function is a wrapper around \code{lm} and \code{coeftest}
#'
#' @param ... (required) arguments passed to \code{lm}
#' @param HC the HC matrix to be used for the robust standard error calculations 
#' @param cluster the name of the cluster variable in the \code{data.frame} passed to \code{lm} 
#' 		for the cluster robust standard error calculations 
#' @param data_frame return estimates in a \code{data.frame} or \code{coeftest} object(default)
#' @param as_attr return \eqn{R^2} and \eqn{N} as column or attributes when \code{data_frame=TRUE}? 
#'
#' @details 
#' This function calls \code{lm()} and passes the model object further to \code{coeftest()} to 
#' calculate (cluster robust) standard errors. The results are equivalent to calling 
#' \code{coeftest(model, vcov=vcovHC(m,HC))} or \code{cl(model,cluster)} after estimating
#' \code{model} via \code{lm()}.
#' 
#' The default settings produce estimates that are identical to calling \code{lm(...)}.
#' 
#' To reproduce STATA's default robust standard errors use \code{HC="HC1"}, to reproduce the 
#' STATA cluster robust standard errors (\code{, vce(cluster ___)}), supply the name of the cluster variable. 
#' 
#' @return a \code{data.frame} if \code{data_frame=TRUE} otherwise a \code{coeftest} object. 
#' The \code{data.frame} object has two additional attributes "N" (the number of observations) and 
#' "r2" the \eqn{R^2} of the model.
#'
#' @author 
#'  The cluster robust standard error function \code{cl()} by Mahmood Ara at Stockholm University, see:
#' 	\url{https://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/}
#' 
#' @seealso \code{\link{lm}}, \code{\link{vcovHC}}
#'
#' @examples
#' 
#'	x <- runif(100, -1, 1)
#'  g <- sample(1:10, 100, replace=TRUE)
#'  y <- 1 + x + rnorm(100)
#' 
#'  dat <- data.frame(y=y,x=x,g=g)
#'  dat[1,1] <- NA; dat[10,1] <- NA
#'
#'  ols(y~x,dat=dat,cluster='g', na.action='na.omit')
#'
#'
#' @export
ols <- function(..., HC="const", cluster=NULL, data_frame=FALSE, as_attr=TRUE){
	m <- do.call(lm, list(...) )
	N <- nobs(m)
	r2 <- summary(m)$r.squared
	if (is.null(cluster)){
		m <- coeftest(m, vcov = vcovHC(m, HC))
	} else { 
		clustvar <- expand.model.frame(m, 
			cluster, envir=environment())[,cluster]
		m <- cl(fm=m,cluster=clustvar)
	}
	if( data_frame==TRUE) {
		res <- get_lmHC_est(m)
		if (as_attr==TRUE){
			attr(res, "r2") <- r2
			attr(res, "N") <- N
		} else {
			res$r2 <- r2
			res$N <- N
		}
		return(res)
		}
	else return(m)
	}

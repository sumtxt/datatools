get_lmHC_est <- function(m){
  var <- rownames(m)
	est <- m[,'Estimate'] 
	se <- m[,'Std. Error'] 
	pval <- m[,4] 
	low <- est + (se * qnorm(0.975))
	hig <- est + (se * qnorm(0.025))
  res <- data.frame(var=var, est=est, lo=low, hi=hig, se=se, pval=pval)
  rownames(res) <- NULL
	return(res)
	}


# Source: https://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
cl   <- function(fm, cluster){
           M <- length(unique(cluster))
           N <- length(cluster)
           K <- fm$rank
           dfc <- (M/(M-1))*((N-1)/(N-K))
           uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
           vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
           return(coeftest(fm, vcovCL)) 
       }

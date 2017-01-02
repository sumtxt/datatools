#' Estimates the Expected Number of Errors in a Data Entry Job
#' 
#' @param S size of the evaluation sample 
#' @param F number of errors in the evaluation sample
#' @param prior can be either 'postmen', 'bad_postmen' or a named vector with prior values. See below for details. 
#' @param quantity can be either 'mean', '95q' or 'param'. See below for details. 
#' 
#' 
#' @details 
#' The function calcualtes (Bayesian posterior) estimates for the expected number of errors (\code{quantity='mean'}) or the 
#' upper bound of the 95\% (Bayesian) credible interval (\code{quantity='95q'}) in the population of entered values when a 
#' random sample of \code{S} entered values contains \code{F} errors. For a discussion on the type of inference, see Winkler 
#' et al. (2002).
#' 
#' The calculations are done assuming a Beta-distribution prior over the proportion of errors for all entered values. The 
#' users must supply hyper-parameters for the Beta-prior. The function provides two informed default hyper-parameter sets. 
#' Both induce an informed prior and are formed from data about postmen's performance in typing post codes as reported in 
#' Baddeley/Longman (1978). 
#' 
#' The first prior (\code{prior='postmen'}) says that the a-priori the proportion of errors relative to the total number of entered 
#' values is about 1.4\% with a variance 0.7. The second prior (\code{prior='bad_postmen'}) is more conservative saying that 
#' it is about 2.1\% with a variance of 1.1. Users can also supply their own prior using a named vector that contains the alpha 
#' and beta hyper-parameters for the Beta prior (the output of \code{\link{calc_beta_param}} can be used). 
#' 
#' 
#' 
#' @return value or vector of the requested quanty/ies. 
#' 
#' @seealso \code{\link{calc_beta_param}}
#' 
#' @references 
#'  Baddeley, A. D., and D. J. A. Longman. 1978. "The influence of length and frequency of training session on the rate of learning to type." Ergonomics 21(8), 627-635.
#' 
#'  Winkler, Robert L. and Smith, James E. and Fryback, Dennis G. 2002. "The Role of Informative Priors in Zero-Numerator Problems: Being Conservative Versus Being Candid". The American Statistician 56(1), 1-4.
#' 
#' @examples 
#'  \dontrun{
#' 
#'  est_typing_err(S=20, F=0, prior='postmen', quantity='95q')
#' 
#'  Example from Winkler et al. (2002) p.3:
#' 
#'  est_typing_err(167, 0, prior=c('alpha'=0.042, 'beta'=27.96), quantity='param' ) 
#' 
#'  } 
#' 
#' 
#' 
#' @export
est_typing_err <- function(S, F, prior='postmen', quantity='mean'){

	# Prior: 
	if ( length(prior)==1 ){

		if ( !( prior=='postmen' | prior=='bad_postmen') ) {
			stop("Quantity must be either 'postmen', 'bad_postmen' or a named vector with parameters named 'alpha'/'beta'.")	
		}

		if ( prior=='postmen'){
			# calc_beta_param( (1.4/100), (0.7/100)^2)
			alpha0 <- 3.93
			beta0 <- 276.7843
			}

		if (prior=='bad_postmen'){
			# calc_beta_param( (2.06/100), (1.07/100)^2)
			alpha0 <- 3.60957
			beta0 <- 171.6123
			}

	} else {

		if ( !(!is.na(prior['alpha']) & !is.na(prior['beta']) ) ){
			stop("Quantity must be either 'postmen', 'bad_postmen' or a 
				named vector with parameters named 'alpha'/'beta'.")	
		}

		alpha0 <- prior['alpha']
		beta0 <- prior['beta']

	}

	# Posterior: 
	alpha <- alpha0 + F
	beta <- beta0 + S - F

	# Return 
	if ( !(quantity %in% c('mean', 'param', '95q') )  ){
		stop("Quantity must be either 'mean', 'param', or '95q'.")
	}

	if ( quantity == 'mean' ){
		return( alpha/(alpha+beta) )
	}

	if (quantity == 'param'){
		return( c(alpha=alpha, beta=beta))
	}

	if (quantity == '95q'){
		tmp <- rbeta(100000, alpha, beta)
		return(quantile(tmp, probs=0.95))
	}
	}
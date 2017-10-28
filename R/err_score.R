#' Computes various scores useful for evaluating prediction accuracy 
#' 
#' @param actual numeric vector with known values
#' @param predicted numeric vector with predicted values
#' @param slc logical vector indicating which values to use (default: all of them)
#' @param m score, can be either "rmse" (root mean squared error), 
#' 	"mad" (mean absolute error) or "cor" (Pearon's correlation).
#' 
#' @details 
#' Missing values are ignored, ie. the score is computed based on 
#' complete cases. 
#' 
#' @return scalar score  
#' 
#' @examples 
#'  \dontrun{
#' 
#' 
#'  a <- rnorm(10)
#'  b <- rnorm(10)
#'  
#'  err_score(a,b)
#' 
#'  } 
#' 
#' 
#' 
#' @export
err_score <- function(actual,predicted,slc=TRUE, m="rmse"){
	actual <- as.vector(actual)
	predicted <- as.vector(predicted)
	if (m=='rmse') return(sqrt(mean((actual[slc]-predicted[slc])^2,na.rm=TRUE)))
	if (m=='cor') return(cor(actual[slc],predicted[slc],use="complete.obs"))
	if (m=='mad') return(mean(abs(actual[slc]-predicted[slc]),na.rm=TRUE))
	stop(paste("Error:", m, "unknown metric.", sep=" "))
}

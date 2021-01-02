#' Remove sparse categories 
#' 
#' @param x factor variable
#' @param threshold threshold
#' @param label label for the new category
#' 
#' @details 
#' Combines levels with strictly less than \code{threshold} number of observations into a new category. NA are ignored. 
#' 
#' @return factor variable 
#' 
#' @examples 
#' 
#'  a <- rpois(100,1)
#'  k <- length(unique(a))
#'  a <- factor(a, labels=LETTERS[1:k])
#'  rem_sparse_cat(a, threshold=20)
#' 
#' 
#' @export
rem_sparse_cat <- function(x,threshold, label="(Other)"){
	if(!is.factor(x)) return(x)
	tab <- table(x)
	counts <- as.vector(tab)
	names(counts) <- names(tab)
	to_drop <- names(counts[counts < threshold])
	levels(x) [levels(x) %in% to_drop] <- label
	return(x)
	}
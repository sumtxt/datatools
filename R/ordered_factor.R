#' Creates an ordered factor based on order of another variable 
#' 
#' @param var variable to be converted into a factor
#' @param seq variable that implies the sequence of \code{var} when the function \code{order} is applied to it
#' 
#' 
#' @details 
#' This function helps to re-order groups in a \code{ggplot2} plot. See example below. 
#' 
#' 
#' @return \code{factor(var)} with the implied order by \code{seq}.
#'
#' @examples 
#'
#' val <- rpois(10,100)
#' num <- sample(seq(1,10))
#' 	
#' levels(ordered_factor(val, num))
#' 
#' @export
ordered_factor <- function(var, seq){
	seq_var <- unique(var[order(seq)])
	return(factor(var, levels=seq_var))
	}
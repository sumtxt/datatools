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
#'  \dontrun{
#' 
#'  require(ggplot2)
#'  require(dplyr)
#' 
#'  economics_long <- economics_long %>% group_by(variable) %>% 
#' 	 mutate( value01_mean=mean(value01) ) 
#' 
#' 	economics_long <- ungroup(economics_long) %>% 
#' 	 mutate(variable_mean_ordered = ordered_factor(as.character(variable),value01_mean) )
#' 
#'  ggplot(economics_long, aes(date, value01, colour = variable)) +
#'  	geom_line()
#' 
#'  ggplot(economics_long, aes(date, value01, colour = variable_mean_ordered)) +
#'  	geom_line()
#' 
#'  }
#' 
#' @export
ordered_factor <- function(var, seq){
	seq_var <- unique(var[order(seq)])
	return(factor(var, levels=seq_var))
	}
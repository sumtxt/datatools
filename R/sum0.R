#' Sum of Vector Elements with alternative empty-set definition
#' 
#' @param x numeric, complex or logical vector
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' 
#' @return \code{scalar} sum of all values in \code{x}. Different to \code{\link{sum}}, the 
#' sum of an empty set is undefined (see example). This is helpful for aggregating count data 
#' across observations that are nested in groups and for some groups all observations are missing 
#' 
#' @seealso \code{\link{sum}}
#' 
#' @examples 
#'  \dontrun{
#' 
#'  x <- c(NA,NA,NA)
#'  sum(x,na.rm=TRUE)
#'  sum0(x,na.rm=TRUE)
#'  
#'  }
#' 
#' @export
sum0 <- function(x, na.rm=TRUE){
  if( sum(is.na(x))==length(x) ) return(NA)
  else return(sum(x, na.rm=na.rm)) 
  } 
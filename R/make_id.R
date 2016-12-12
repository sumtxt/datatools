#' Creates a unique, numeric ID sequence based on a set of variables
#' 
#' @param ... variables to be used to create unique ID-sequence
#' 
#' @return vector that contains unique numeric id sequence starting with 0 up to K. 
#' 
#' @examples 
#'  \dontrun{
#' 
#'  require(dplyr)
#' 
#'  mtcars %>% mutate( id = make_id(cyl, vs) )
#' 
#'  } 
#' 
#' 
#' 
#' @export
make_id <- function(...) as.numeric(factor(paste(..., sep="")))
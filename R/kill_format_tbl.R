#' Disables all formatting of the tibble() package 
#' 
#' This function overrides the tibble print() function 
#' such that calling print() on a tbl object displays 
#' the first 10 rows in the format of a data.frame in base R. 
#' Call after loading the tibble package. 
#'  
#' @examples 
#' \dontrun{
#' 
#' library(tibble)
#' 
#' tibble(x = runif(10), y = x * 2)
#' 
#' kill_format_tbl()
#' tibble(x = runif(10), y = x * 2)
#' 
#' } 
#' 
#' 
#' 
#' @export
kill_format_tbl <- function() { 

print.tbl_df <<- function(x, ...) {
  print(head(as.data.frame(x),n=10), ...)
  if (nrow(x)>10) cat("...", nrow(x), " more rows.\n", sep="")
  invisible(x)
}

}
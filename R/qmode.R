#' Value occuring most often (simple mode)
#' 
#' @param x numeric vector
#' 
#' @return scalar estimate.   
#' 
#' 
#' @details 
#' If NA is mode, then qmode(x)=NA. 
#' If there are two modes, qmode(x) takes the first. 
#' 
#' @examples 
#'  \dontrun{
#'
#'  qmode(rpois(10, 1))
#'  
#'  }
#' 
#' 
#' @export
qmode <- function(x) {
  ux <- unique(x)
  findmax <- which.max(tabulate(match(x, ux)))
  return(ux[findmax])
  }

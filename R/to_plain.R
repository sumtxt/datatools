#' Convert UTF8 string to ASCII string 
#' 
#' @param x character vector
#' 
#' @return \code{vector} with ASCII characters only
#' 
#' @details 
#' This function substitutes ASCII characters for (many) none-ASCII characters, e.g. ä,ö,ü
#' 
#' @examples 
#'  \dontrun{
#' 
#'  x <- "Viele Grüße nach Eisenhüttenstadt"
#'  to_plain(x)
#'  
#'  }
#' 
#' @references 
#' 	Source: \url{https://stackoverflow.com/questions/17517319/r-replacing-foreign-characters-in-a-string}
#' 
#' @export

to_plain <- function(s) {
   old1 <- "šžþàáâãåçèéêëìíîïðñòóôõùúûýŠŽÞÀÁÂÃÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÙÚÛÝ"
   new1 <- "szyaaaaaceeeeiiiidnoooouuuySZYAAAAACEEEEIIIIDNOOOOUUUY"
   s1 <- chartr(old1, new1, s)
   old2 <- c("œ", "ß", "æ", "ø", "ä", "Ä", "ö", "Ö", "Ü", "ü")
   new2 <- c("oe", "ss", "ae", "oe", "ae", "A", "oe", "Oe", "Ue", "ue")
   s2 <- s1
   for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
   return(s2)
   }

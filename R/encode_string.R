#' Encode a string vector into a set of numeric vectors
#'
#' Computes the distance matrix for a string and then applies multidimensional scaling to this distance matrix.
#' 
#' @param string a string vector.
#' @param method the name of the distance metric.
#' @param k the maximum dimension of the space which the string is to be represented in.
#' 
#' 
#' @details 
#' The method for computing distance, can be any of the string distance metrics implemented as part of the \code{stringdist} package (see \code{\link[stringdist]{stringdist-metrics}} for a list).
#'  
#' @seealso 
#' \code{\link[stringdist]{stringdist-metrics}}
#' 
#' @returns
#' \code{data.frame}
#' 
#' @examples 
#' a <- c("Berlin", "Hamburg", "München", "Köln") 
#' encode_string(a, method='cosine', k=3)
#' 
#' 
#'@importFrom stats cmdscale 
#'@importFrom stringdist stringdistmatrix 
#'@export 
encode_string <- function(
	string, method='osa',k=2, ...){

	D <- stringdist::stringdistmatrix(
	string, method=method, ...)
	M <- as.data.frame(cmdscale(D, k=k))

	colnames(M) <- paste0("k",1:k)
	return(M)
	}

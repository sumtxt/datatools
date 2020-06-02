#'	
#'
#' @name datatools-package
#' 
#' @docType package
#' @aliases datatools
#' @title Simple tools I tend to use a lot when working with data in R
#' @author Moritz Marbach \email{moritz.marbach@gess.ethz.ch}
#'
#' 
#' @useDynLib datatools
#' @importFrom data.table rbindlist
#' @importFrom dplyr dense_rank select_if mutate_if
#' @importFrom sandwich vcovHC estfun sandwich
#' @importFrom lmtest coeftest
#' @importFrom adagio assignment
#' @importFrom stringdist stringdist
#' @importFrom rlang dots_list
#' @importFrom httr content POST GET
#' @importFrom purrr map
#' @importFrom tibble tibble as_tibble
#' @importFrom forcats fct_explicit_na
#' @importFrom VIM gowerD
NULL
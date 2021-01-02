#' Encode missing values as category and/or add missing data indicator 
#' 
#' A function useful to analyze missing data patterns. 
#' 
#' @param df \code{data.frame}
#' 
#' @details 
#' Variables of class logical, character, factor, and ordered factor: Convert missings to a separate category ("(Missing)"). Character variables are converted to a factor variable. 
#' All other variables: Impute 0 and add a missing data indicator to data frame ("[varname]_na").
#' 
#' @return 
#' \code{data.frame}
#' 
#' @examples 
#' df <- data.frame(a=1:5, b=LETTERS[1:5])
#' df[['a']][1] <- NA
#' df[['b']][4] <- NA
#' encode_na(df)
#' 
#' 
#'@importFrom dplyr select_if 
#'@importFrom dplyr mutate_if
#'@importFrom forcats fct_explicit_na
#'@export
encode_na <- function(df){
	dat_na <- is.na(select_if(df, is_num))
	if(!is.null(dat_na)){
		colnames(dat_na) <- paste0(colnames(dat_na), "_na")
	}
	dat <- mutate_if(df,  is_cat, convert_categorical)
	dat <- mutate_if(dat, is_num, impute_zero)
	if(!is.null(dat_na)){
		dat <- cbind(dat,dat_na)
	}
	return(dat)
	}


convert_categorical <- function(x){
	fct_explicit_na(as.factor(x))
	}

impute_zero <- function(x){
	x[is.na(x)] <- 0
	return(x)
	}

is_cat <- function(x){
		is.character(x) | 
		is.factor(x) | 
		is.logical(x) | 
		is.ordered(x)	
	}

is_num <- function(x){
		!(is.character(x) | 
		is.factor(x) | 
		is.logical(x) | 
		is.ordered(x))	
	}

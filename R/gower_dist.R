#' Computes The Extended Gower Distance Of Two Data Sets
#' 
#' Wrapper around \code{\link[VIM]{gowerD}} with a simplified API.
#' 
#' @param data \code{data.frame}
#' @param query \code{data.frame}
#' @param return_index logical if TRUE return the index of the minimum distance
#' @param n_min integer number of values with smallest distance to be returned
#' @param return_min logical if the computed distances for the indices should be returned
#' @param verbose return information about variables 
#' 
#' @details 
#' \code{\link[VIM]{gowerD}} treats all variables as numeric if no type is supplied by the user.  \code{gower_dist} guess the variable type from the class name of the variable. 
#' 
#' @return 
#' \code{data.frame}
#' 
#' @seealso 
#' \code{\link[VIM]{gowerD}}
#' 
#' @examples 
#' data <- tibble(a=1:5, b=LETTERS[1:5])
#' data[['a']][1] <- NA
#' data[['b']][4] <- NA
#' 
#' query <- tibble(a=1:5, b=LETTERS[6:10])
#' gower_dist(data,query)
#' 
#' 
#' @export
gower_dist <- function(
		data, query, 
		return_index=FALSE,
		n_min=1L,
		return_min=FALSE,
		verbose=TRUE){

	data <- as.data.frame(data)
	query <- as.data.frame(query)

	if( ncol(data)!=ncol(query) ) stop("Data frames with different number of columns are not allowed.")

	query <- query[,colnames(data)]
	
	orders <- sapply(data,is.ordered)
  orders <- colnames(data)[orders]

  factors <- sapply(data,is.factor) | sapply(data,is.character) | sapply(data,is.logical)
  factors <- colnames(data)[factors]
  factors <- factors[!factors%in%orders]
  
  numerical <- sapply(data,is.numeric) | sapply(data,is.integer)
  numerical <- colnames(data)[numerical]
  
  if(length(orders)>0){
  	orders_lev <- sapply(data[,orders,drop=FALSE], 
  			function(x) length(levels(x)) ) 
  	} else {
  	orders_lev <- vector()
  	}

  k_ = (length(numerical) + 
  			length(factors) + 
  			length(orders))

  if(verbose){
  	cat("Numerical variables:", numerical, "\n")
  	cat("Factor variables:", factors, "\n")
  	cat("Ordered factor variables:", orders, "\n")
 	 }

  if(ncol(data)!=k_) {
  	vars <- c(numerical,factors,orders)
  	var_miss <- colnames(data)[!(colnames(data) %in% vars)]
  	stop(paste("Unrecognized variables:", var_miss))
  	}

  dist <-  gowerD(
  		data.x=data, 
  		data.y=query, 
  		numerical=numerical, 
  		factors=factors, 
  		orders=orders, 
  		levOrders=orders_lev,
  		returnIndex=return_index,
  		nMin=n_min,
  		returnMin=return_min)

  return(dist)

	}
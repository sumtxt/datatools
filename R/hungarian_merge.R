#' Merge Two Data Frames using the Hungarian Method 
#' 
#' Merge two data frames by a common column, s.t. the Damerau-Levenshtein distance 
#' is minimized 
#' 
#' @param x,y  data frames with the same number of observations, or objects to be coerced to one.
#' @param by.x,by.y specifications of the columns used for merging.
#' 
#' 
#' @details 
#' Finds the optimal matches using (a fast version of) the Hungarian method as implemented in 
#' \code{\link[adagio]{assignment}}. 
#' 
#' @seealso 
#' \code{\link[adagio]{assignment}} and \code{\link[stringdist]{stringdist}}
#'  
#' 
#' @export
hungarian_merge <- function(x,y, by.x=NULL, by.y=NULL){
	
	x <- as.data.frame(x)
	y <- as.data.frame(y)

	if( nrow(x) != nrow(y) ) stop("Number of rows in each data.frame must match")
	
	K <- nrow(x)

	x[,'id_x'] <- 1:K
	y[,'id_y'] <- 1:K

	x_ <- x
	y_ <- y

	colnames(x)[colnames(x)==by.x] <- "name_x"
	colnames(y)[colnames(y)==by.y] <- "name_y"

	dat <- merge(x[,c("id_x","name_x")], y[,c("id_y","name_y")], by=NULL)
	dat[,'dist'] <- with(dat, stringdist(name_x,name_y))
	
	dat <- dat[,c("id_x","id_y", "dist")]

	dat <- stats::reshape(dat, direction='wide', idvar="id_x",
		timevar="id_y")

	m <- as.matrix(dat)[,-1]
	m_ass <- adagio::assignment(m)

	res <- data.frame(id_x=1:K, 
		id_y=(1:K)[m_ass$perm], 
		stringsAsFactors=FALSE) 

	res <- merge(res,x_,by=c("id_x"))
	res <- merge(res,y_,by=c("id_y"))

	res <- res[,!(colnames(res) %in% c("id_x", "id_y")) ]

	return(res)

	}

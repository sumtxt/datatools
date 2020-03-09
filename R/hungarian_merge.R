#' Merge Two Data Frames using the Hungarian Method 
#' 
#' Merge two data frames by a common column, s.t. (by default) the Damerau-Levenshtein distance 
#' is minimized 
#' 
#' @param x,y  data frames where nrow(x) => nrow(y), or objects to be coerced to one.
#' @param by.x,by.y specifications of the columns used for merging without any duplicates. 
#' @param FUN function to be used to calculate the distance between potential matches. 
#' @param distance_col A distance column to output? 
#' @param ... parameters passed to FUN
#' 
#' 
#' @details 
#' 
#' Finds the optimal matches using (a fast version of) the Hungarian method as implemented in 
#' \code{\link[adagio]{assignment}}. 
#' 
#' The merge is performed s.t. that all rows of \code{y} are matched with exactly one 
#' row of \code{x} leaving some rows in \code{x} unmatched.  
#' 
#' The function is most useful if \code{x} and |code{y} are lists with different 
#' but unique realizations of names from a third master list. For example, two lists 
#' with county names spelled slightly different as in the example below. 
#' 
#' 
#' @seealso 
#' \code{\link[adagio]{assignment}} and \code{\link[stringdist]{stringdist}}
#'  
#' 
#' # Matching German county names 
#' 
#' dat1 <- data.frame(gem=c("Rosenheim", 
#' 	"Rosenheim, Stadt", "München", "München, Stadt") , size=rnorm(4))
#' 
#' dat2 <- data.frame(kr=c("Rosenheim, Landkreis", 
#' 		"Rosenheim, kreisefreie Stadt", "München, Landeshauptstadt") , pop=rpois(3,10))
#' 
#' hungarian_merge(dat1,dat2,by.x="gem", by.y="kr",distance_col=TRUE )
#' 
#' 
#' 
#' # User-defined function 
#' 
#' dat1 <- data.frame(id=c(12,5,1,100), size=rnorm(4))
#' dat2 <- data.frame(id=c(10,1000,0,5), size=rnorm(4))
#' 
#' hungarian_merge(dat1,dat2,by.x="id", 
#' 	by.y="id", FUN=function(x,y){abs(x-y)},
#' 	distance_col=TRUE )
#' 
#'  
#' @export
hungarian_merge <- function(x,y, by.x=NULL, by.y=NULL, FUN=NULL, distance_col=FALSE, max_diff=Inf, C=1, ...){
	
	if(!(length(max_diff)==1 & is.numeric(max_diff))) stop("max_diff must be a scalar.")

	x <- as.data.frame(x)
	y <- as.data.frame(y)

	Kx <- nrow(x)
	Ky <- nrow(y)

	if( !(Kx >= Ky) ) stop("The number of rows in y must be at most equal the number of rows in x.")

	if( "id_x" %in% colnames(x) | 
			"id_y" %in% colnames(y)  ) stop("Column names id_x,id_y are not allowed. Please rename and run again.")	
	
	x[,'id_x'] <- 1:Kx
	y[,'id_y'] <- 1:Ky

	x_ <- x
	y_ <- y

	if ( !is.null(FUN) ) { FUN <- match.fun(FUN) } 
		else { FUN <- get("stringdist", envir=environment(stringdist)) } 

	dat <- merge(x[,c("id_x",by.x)], y[,c("id_y",by.y)], by=NULL)
	dat[,'dist'] <- FUN(dat[,by.x],dat[,by.y], ...)*C
	
	dat <- dat[,c("id_x","id_y", "dist")]

	# Build cost matrix
	dat <- stats::reshape(dat, direction='wide', idvar="id_x",
		timevar="id_y")

	m <- as.matrix(dat)[,-1]

	# Add dummy variables if nrow(y) < nrow(x)
	if (Kx!=Ky){ 
			maxcost <- max(m) + 1
			toadd <- matrix(maxcost, Kx, Kx-Ky)
			m <- cbind(m,toadd)
	} 

	# Find assignments 
	m_ass <- adagio::assignment(m)
	sol <- m_ass$perm

	# Generate merged dataset
	res <- data.frame(id_x=1:Kx, 
		id_y=(1:Kx)[sol], stringsAsFactors=FALSE) 

	# Distance metric 
	res$diff <-  m[cbind(1:Kx,sol)]
	
	# Drop merges if max_diff smaller than diff 
	res[res$diff>max_diff,"id_y"] <- NA
	res[res$diff>max_diff,"diff"] <- NA

	# Drop distance metric if not requested
	if(distance_col!=TRUE){
		res$diff <- NULL
	} 

	# Merge back information
	res <- merge(res,x_,by=c("id_x"))
	res <- merge(res,y_,by=c("id_y"), all=TRUE)

	res <- res[,!(colnames(res) %in% c("id_x", "id_y")) ]

	return(res)
	}


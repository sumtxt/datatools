#' Alternative to \code{source()} that also logs the output via \code{sink()}
#' 
#' @param file file name 
#' @param ... parameters passed to \code{\link{source}}
#' 
#' @details 
#' The logfile is written to ./logs/ using \code{file}
#' as the filename appended with a timestamp.
#' 
#' @details 
#' The header and footer are appended with a timestamp 
#' and the footer features the output of \code{\link{sessionInfo}}.
#' 
#' @seealso 
#'  \code{\link{source}} and \code{\link{sink}}
#' 
#' @export
source_log <- function(file,...){

	# Construct logging file path/name
	ts <- format(Sys.time(), "_%Y%m%d_%H%M%S")
	lf <- normalizePath(dirname(file))
	ln <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
	ln <- paste(ln, ts, ".txt", sep="")
	logpath <- file.path(lf, "logs", ln)

	# Start logging and execute 
	sink(logpath, split=TRUE)
	timestamp()	
	source(file, echo=TRUE, max.deparse.length=Inf, width.cutoff=500, ...)
	timestamp()
	if(info==TRUE) sessionInfo()
	sink()
	
	}


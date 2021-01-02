#include <Rcpp.h>


//' Creates a unique ID for all uninterrupted series in a vector
//'
//' 
//' @param x numeric vector to be used
//' @param delta increment with default \code{delta}=0 
//' 
//' @return an integer vector 
//' 
//' @details
//' If x[t] = (x[t-1] + delta) then ID(x[t])=ID(x[t-1]) 
//'  otherwise ID(x[t]) = ID(x[t-1]) + 1. 
//' 
//' @examples 
//' 
//'  x <- c(0,1,1,1,2,2,1,3,1,0,0)
//'  y <- c(1998:2000,2005:2010)
//' 
//'  make_series_id(y, delta=1)
//'  make_series_id(x)
//' 
//' 
//' 
//' 
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector make_series_id(Rcpp::NumericVector x, double delta=0) {

int N = x.size();
Rcpp::IntegerVector out(N);

out(0) = 1;

for (int n = 1; n < N; n++){
 	if ( x(n)==(x(n-1)+delta) ) { 
 		out(n) = out(n-1);
 	} else { 
 		out(n) = out[n-1]+1;
		}
	}
return(out);
}
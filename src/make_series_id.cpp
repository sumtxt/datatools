#include <Rcpp.h>


//' Creates a unique ID for all uninterrupted series 
//'
//' 
//' @param x numeric vector to be used
//' 
//' @return an integer vector 
//' 
//' @details
//' Creates a unique, numeric ID for each uninterrupted sequence of equal values in a vector 
//' 
//' @examples 
//'  \dontrun{
//' 
//'  x <- c(0,1,1,1,2,2,1,3,1,0,0)
//' 
//'  make_series_id(x)
//' 
//'  } 
//' 
//' 
//' 
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector make_series_id(Rcpp::NumericVector x) {

int N = x.size();
Rcpp::IntegerVector out(N);

out(0) = 1;

for (int n = 1; n < N; n++){
 	if ( x(n)==x(n-1) ) { 
 		out(n) = out(n-1);
 	} else { 
 		out(n) = out[n-1]+1;
		}
	}
return(out);
}
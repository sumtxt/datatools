#include <Rcpp.h>

//' Cumulative sum of uninterrupted '1'
//' 
//' 
//' @param x integer vector to be used
//' 
//' @return integer vector 
//' 
//' @details 
//' Returns a vector whose elements are the 
//' cumulative sums of uninterrupted entries equal to 1 in an integer vector
//' 
//' @examples 
//'  \dontrun{
//' 
//'  x <- c(0,1,1,1,0,0,1,1,1,0,0)
//' 
//'  cumsum1(x)
//' 
//'  } 
//' 
//' 
//' 
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector cumsum1(Rcpp::IntegerVector x) {

int N = x.size();
Rcpp::IntegerVector out(N, -1);

out(0) = x(0);

for (int n = 1; n < N; n++){
 	if( x(n)==1 ) {
 		if ( x(n)==x(n-1) ) { 
 			out(n) = out(n-1) + 1;
 		} else { 
 			 out(n) = 1;
 		}
 	} else { 
 		out(n) = 0;
 	}
 }

return(out);
}


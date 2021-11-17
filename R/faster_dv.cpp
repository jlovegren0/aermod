#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector faster_dv(NumericMatrix x) {
    int ncol = x.ncol();
    NumericVector nday = {366, 365, 365, 365, 366};
    int ndays = 0;
    int nyr = nday.size();
    for ( int i = 0; i < nyr; ++i ){ ndays += nday[i]; }
    NumericMatrix dhs(ndays,ncol);
    for ( int i = 0; i < ncol; ++i )
    {
        for ( int j = 0; j < ndays; ++j)
        {
            double dmax = 0;
            for ( int k = 0; k < 24; ++k )
            {
                if ( x( j*24 + k , i ) > dmax )
                    dmax = x( j*24 + k, i);
            }
            dhs( j , i ) = -1 * dmax;
        }
    }
    NumericMatrix h4hs(5,ncol);
    for ( int i = 0; i < ncol; ++i )
    {
        int offs = 0;
        NumericVector y = dhs(_,i);
        for ( int j = 0; j < 5; ++j )
        {
            if ( j > 0 )
                offs += nday[ j - 1 ];
            NumericVector z = y[Rcpp::Range( offs , offs + nday[j] - 1 )];
            std::nth_element(z.begin() , z.begin() + 3, z.end() );
            h4hs( j , i ) = z[3];
        }
    }
    NumericVector dvs(ncol);
    for ( int i = 0; i < ncol; ++i )
    {
        double dv = 0;
        for ( int j = 0; j < 5; ++j )
        {
            dv += h4hs( j , i );
        }
        dvs[i] = -1 * dv / 5;
    }
    return dvs;
}

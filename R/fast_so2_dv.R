#' Calculate 2010 SO2 design values using base functions only
#' 
#' @name fast_so2_dv
#' @param mat An impact matrix over five years of met data
#' @param sumfn Use \code{max} for max DV at any receptor, default is \code{identity} (DV at each receptor, in order). 
#' @return A vector of design values.
#' @export
## usethis namespace: start
#' @useDynLib aermod, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
fast_so2_dv <- function(mat,sumfn=identity){
    hrbaseline <- attr(mat,'hrbaseline')
    y1 <- as.integer(lubridate::year(hrbaseline + lubridate::hours(1)))
    y2 <- y1 + 4
    daylens <- 365L + lubridate::leap_year(y1:y2)
    nday <- sum( daylens )
r1 <- purrr::map_dbl( 1:(length(mat)/24) , function(.x) max( mat[  ( 24 * (.x - 1) + 1 ): (24 * .x) ] ) ) %>%
    matrix(nrow=nday)
h4 <- function(vec)
{
    idx <- list( c(1, (cumsum(daylens) + 1))[1:5] , cumsum(daylens) )
    auxfn <- function(x) x[order(x,decreasing=TRUE,method='radix')][4]
    purrr::map_dbl( 1L : 5L , function(.x) auxfn( vec[ idx[[1]][.x] : idx[[2]][.x] ] ) ) %>% mean
}
purrr::map_dbl( 1 : dim(r1)[2] , ~ h4( r1[,.] ) ) %>% sumfn
}

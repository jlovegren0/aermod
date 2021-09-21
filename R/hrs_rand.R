#' Generate random hours of operation
#' 
#' Generate random hours of operation over the meteorological
#' database. Hours within each meteorological year are randomly sampled (without replacement).
#' Random sampling can consider blocks of consecutive hours, or individual hours (block length of 1).
#' 
#' @name hrs_rand
#' @param yrspan Calendar years for which operating hours are to be generated. A numeric vector
#' @param nblock Number of blocks of consecutive hours. 
#' @param blocklen Number of hours in each block. Default is 1.
#' @param units for \code{blocklen}, either \code{"hours"} (default) or \code{"days"}. Return value is always vector of hours.
#' @param emis_scale , for binned randomized emission rates, a vector of emission scalars of length \code{blocklen}. Default is \code{1} (no binning).
#' @param which_filter Integer specifying special filters on the random number generation. Default is 1 (no filter). 2 is case where nblock = 1 and blocks in successive years must be spaced at least 365 days apart.
#' @return An integer vector of operating hours, which may be used to index rows in an impact matrix.
#' @export
hrs_rand <- function(yrspan=2016L:2020L,
        nblock=6L,
        blocklen=1L,
        which_filter=1L,
        units=c("hours","days"),
        emis_scale = 1)
{   
    if ( all( length(emis_scale) != c(1L,nblock) ) ){ stop('Error: Bad emission scalar length (need 1 or nblock)') }
    filters <- list( function(x){TRUE}, function(x) all( diff(x) > 365L*24L ) )
    if( blocklen > 1L ){ which_filter=1L }
    dyy <- purrr::map_int( yrspan , ~ dplyr::case_when( lubridate::leap_year(.) ~ 366L , TRUE ~ 365L ) )
    mag <- ifelse(all(units=="days"),1L,24L) 
    anti_mag <- ifelse(all(units=="days"),24L,1L)
    hry <- `*`(mag,dyy)
    offs_hr <- c(0,cumsum(hry)[1:(length(hry)-1)])
    L <- function(N){ 
        draw <- function()  sample(1L:(N-blocklen+1),nblock)  %>% sort
        u <- draw()
        if ( nblock == 1 ){ return(u) }
        while( ! all((u[2:length(u)] - u[1:(length(u)-1)]) >= blocklen )){ 
        u <- draw() }
        return(u)
    }
    repeat 
    {
        start_hrs <- purrr::map( seq_along(yrspan), ~ L(hry[.]) + offs_hr[.] ) %>% unlist 
        if ( filters[[which_filter]](start_hrs) ){ break }
    }
    hrlist <- purrr::map( start_hrs, ~ .:(.+(blocklen-1L)) ) %>% unlist %>%
        purrr::map( function(.x) ((.x-1)*anti_mag+1):(.x*anti_mag) ) %>% unlist 
    if ( length(emis_scale) > 1 )
    {
    emscal <- unlist(purrr::map(1:length(yrspan),~sample(emis_scale)))
    elist <- rep(emscal,each=blocklen) %>% rep(each=anti_mag) 
    }
    else elist <- emis_scale
    tibble::tibble(hrlist,elist)
}

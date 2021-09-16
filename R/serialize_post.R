#' Serialize impact matrix
#'
#' Serialize impact matrix to AERMOD unformatted binary POST file.
#'
#' @param mat matrix
#' @param fname character
#' @param srcgrp character
#'
#' @return logical
#' @name serialize_post
#' @export
serialize_post <- function( mat , fname , srcgrp ) {
nhr <- dim(mat)[1]
nrcp <- dim(mat)[2]
u <- attr(mat,'hrbaseline') +lubridate::hours(1:nhr)
hrlabz <- as.integer(sprintf("%02.0f%02.0f%02.0f%02.0f", lubridate::year(u)-2000, 
    lubridate::month(u), 
    lubridate::day(u), 
    lubridate::hour(u)+1 ))
if ( missing(srcgrp) ) srcgrp <- attr(mat,'srcgrp')
srcgrp %<>% stringr::str_pad( side="right",width=8,pad=" ")
eachreclen <- 16L + 8L*nrcp
fp <- file(fname,open='wb',raw=TRUE)
on.exit(close(fp))
wr_rec <- function(hrno){ 
    writeBin( eachreclen , fp , 4 , endian="little")
    writeBin( hrlabz[hrno] , fp , 4 , endian="little")
    writeBin( 1L , fp , 4 , endian="little")
    writeBin( charToRaw(srcgrp) , fp )
    writeBin( mat[hrno,] , fp  )
    writeBin( eachreclen , fp , 4 , endian="little"  )
}
purrr::walk( 1:nhr , wr_rec )
}

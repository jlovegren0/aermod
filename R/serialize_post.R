function( mat , fname , srcgrp ) {
require(purrr)
require(magrittr)
# Serialize impact matrix to AERMOD unformatted binary POST file
# requires purrr, lubridate,magrittr
nhr <- dim(mat)[1]
nrcp <- dim(mat)[2]
u <- attr(mat,'hrbaseline') +hours(1:nhr)
hrlabz <- as.integer(sprintf("%02.0f%02.0f%02.0f%02.0f", year(u)-2000, month(u), day(u), hour(u)+1 ))
if ( missing(srcgrp) ) srcgrp <- attr(mat,'srcgrp')
srcgrp %<>% str_pad( side="right",width=8,pad=" ")
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

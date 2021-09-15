#' Scan POST file
#' 
#' Reads Fortran unformatted I/O POSTFILE and conver to matrix.
#' Hours are not written individually, nor are source groups
#' as they are saved as attributes of the matrix.
#'
#' @param f character
#' @return matrix
#' @export
function(f){
require(lubridate)
require(stringr)
rewind <- function(fp) seek(fp,0)
fp <- file(f,open="rb",raw=TRUE)
on.exit(close(fp))
	reclen <- readBin(fp,what='integer')
	hr <- readBin(fp,what='integer')
	aveper <- readBin(fp,what='integer')
	srcgrp <- readBin(fp,what='raw',n=8) %>% rawToChar
	nc <- (reclen - 16) / 8
seek(fp,where=0,origin="end")
nbytes <- seek(fp)
rewind(fp)
nhrs <- nbytes / (reclen + 8)
container <- vector(mode="double",length= nhrs * nc )
for ( hh in 1:nhrs )
{
	seek(fp,where=20,origin="current")
	container[ ((hh-1) * nc + 1) : (hh*nc) ] <- readBin(fp,what='double',n=nc)
	seek(fp,where=4,origin="current")
}
res <- matrix( container , ncol = nc , byrow=TRUE)
attr(res,'srcgrp') <- str_trim(srcgrp)
attr(res,'hrbaseline') <- ymd_h(str_glue('20{hr}')) - hours(2)
return(res)
}

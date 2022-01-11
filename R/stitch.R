#' Recombine receptor-disjoint POST files
#' 
#' Parallel AERMOD runs using N workers generate an ordered set of POSTFILEs
#' that need to be recombined. The POSTFILEs all correspond to the same met
#' database and therefore have the same number of rows. The receptors (columns)
#' need to be re-bound in order.
#'
#' The POST are all named in a standard way such that PST003_0012 is the twelfth
#' disjoint POST file corresponding to the thid source group.
#'
#' @name stitch
#' @param srcgrp_idx Source-group specific numeric index. I.e., \code{srcgrcp_idx=12} will pick out PST012_xxxx, which are the files for the 12th defined source group
#' @return Combined POST file
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
stitch <- function(srcgrp_idx,path='.',...){
    pat <- sprintf("PST%03.0f_",srcgrp_idx)
    file_lst <- dir( path = path , pattern = pat , full.names=TRUE )
    pst_lst <- purrr::map( file_lst , aermod::scan_postfile )
    res <- purrr::reduce( pst_lst , cbind )
    attr( res , "srcgrp" ) <- attr( pst_lst[[1]] , "srcgrp" )
    attr( res , "hrbaseline" ) <- attr( pst_lst[[1]] , "hrbaseline" )
    res
}

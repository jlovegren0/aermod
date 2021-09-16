#' Calculate SO2 Design value
#' 
#' Report 2010 SO2 NAAQS DESIGN VALUE
#' @name get_dv
#' @param mat matrix
#' @return double
#' @export
get_dv <- function(mat,diag=FALSE){
	hrbaseline <- attr(mat,'hrbaseline')
	nhr <- dim(mat)[1]
	receps <- paste0("V",1:dim(mat)[2])
tb <- mat %>% as.data.frame %>% tibble::as_tibble %>%
	dplyr::mutate( `hr` = (hrbaseline + lubridate::hours(1:nhr))) %>%
	dplyr::mutate( `dy` = lubridate::as_date(lubridate::floor_date(`hr`,'days'))) %>%
	dplyr::mutate( `yr` = as.integer(lubridate::year(lubridate::floor_date(`hr`,'years'))) ) %>%
	dplyr::select( `hr`,`dy`,`yr`,tidyselect::all_of(receps)) %>%
	tidyr::pivot_longer(tidyselect::all_of(receps)) %>% dplyr::rename(`recep`=`name`,`conc`=`value`) %>%
	dplyr::group_by(`yr`,`dy`,`recep`) %>%
	dplyr::summarize( `day_hi` = max(`conc`) ) %>%
	dplyr::ungroup %>%
	dplyr::group_by(`recep`,`yr`) %>%
	dplyr::slice_max(`day_hi`,n=4)  
    if ( diag ) return(tb)
tb %<>% dplyr::slice_min(`day_hi`) %>%  
    dplyr::ungroup %>%
	dplyr::group_by(`recep`) %>%
	dplyr::summarize( `dv` = mean(`day_hi`) ) %>%
	dplyr::ungroup %>%
	dplyr::summarize(`DV`=max(`dv`)) %>% dplyr::pull(`DV`) %>%
	suppressMessages
return(tb)}


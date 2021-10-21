#' Report Lambda Values for State Health Effects Review
#' 
#' Generates various statistics used in TCEQ Toxicology Tier III RFC reviews.
#' If the impact matrix covers more than one year of met data, only the first year is used.
#'
#' @name sher_lambda
#' @param mat An impact matrix over one year of met data
#' @param an esl against which to report lambda values
#' @return A list of various data tables useful for running Monte Carlo simulations, or reporting unadjusted lambda values.
#' @export
sher_lambda <- function(mat,esl){
	hrbaseline <- attr(mat,'hrbaseline')
	nhr <- dim(mat)[1]
	receps <- paste0("V",1:dim(mat)[2])
tb <- mat %>% as.data.frame %>% tibble::as_tibble() %>% 
	dplyr::mutate( `hr` = (hrbaseline + lubridate::hours(1:nhr))) %>%
	dplyr::mutate( `yr` = as.integer(lubridate::year(lubridate::floor_date(`hr`,'years'))) ) %>%
	dplyr::select( `hr`,`yr`,tidyselect::all_of(receps)) %>%
	tidyr::pivot_longer(tidyselect::all_of(receps)) %>% dplyr::rename(`recep`=`name`,`conc`=`value`) %>%
	dplyr::group_by(`yr`,`recep`) 
glc_max_tbl <- tb %>% dplyr::filter( conc == max(conc) ) %>% select(yr,recep,conc) %>% arrange(yr,desc(conc))
glc_max <- head(glc_max_tbl,1) %>% pull(conc)
glc_max_recep <- head(glc_max_tbl,1) %>% pull(recep)
glc_max_yr <- head(glc_max_tbl,1) %>% pull(yr)
exc_tbl <- tb %>% dplyr::filter( recep == glc_max_recep , yr == glc_max_yr ) %>%
       mutate( e1 = as.integer( conc > esl ) , e2 = as.integer( conc > 2*esl ), e4 = as.integer( conc > 4*esl), e10 = as.integer(conc > 10*esl ) )	
lambda_tbl <- exc_tbl %>% summarize( L1 = sum( e1 ) , L2 = sum( e2 ), L4 = sum( e4), L10 = sum(e10 ) )	
 return(list(   receptor_max_vals = glc_max_tbl , exc_tbl = exc_tbl , lambda_tbl = lambda_tbl , glc_max = list( glc_max ,glc_max_recep,glc_max_yr) ) )
}


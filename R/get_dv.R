#' Calculate SO2 Design value
#' 
#' Report 2010 SO2 NAAQS DESIGN VALUE
#' @param mat matrix
#' @return double
#' @export
function(mat,diag=FALSE){
	require(tidyr)
	hrbaseline <- attr(mat,'hrbaseline')
	nhr <- dim(mat)[1]
	receps <- paste0("V",1:dim(mat)[2])
tb <- mat %>% as.data.frame %>% as_tibble %>%
	mutate( hr = (hrbaseline + hours(1:nhr))) %>%
	mutate( dy = as_date(floor_date(hr,'days'))) %>%
	mutate( yr = as.integer(year(floor_date(hr,'years'))) ) %>%
	select( hr,dy,yr,all_of(receps)) %>%
	pivot_longer(all_of(receps)) %>% rename(recep=name,conc=value) %>%
	group_by(yr,dy,recep) %>%
	summarize( day_hi = max(conc) ) %>%
	ungroup %>%
	group_by(recep,yr) %>%
	slice_max(day_hi,n=4)  
    if ( diag ) return(tb)
tb %<>% slice_min(day_hi) %>%  
    ungroup %>%
	group_by(recep) %>%
	summarize( dv = mean(day_hi) ) %>%
	ungroup %>%
	summarize(DV=max(dv)) %>% pull(DV) %>%
	suppressMessages
return(tb)}


#' Calculate SO2 Design value
#' 
#' Calculate the 2010 SO2 NAAQS design value for an impact matrix. The impact matrix should have rows corresponding to the number of hours in a five-year meteorological database (e.g., 43848 hours for 2016--2020). The number of receptors (columns) can be arbitrary. 
#' 
#' Per EPA's 2014 guidance (pp. A-24--A-25), the calculation is carried out as follows:
#'
#' l. At each receptor, for each hour of the modeled period, calculate a total concentration
#' across all sources including backgro1md concentrations if applicable. This can be done in
#' AERMOD using SRCGROUP ALL or by adding individual source groups outside of
#' AERMOD, using hourly POSTFILEs. If the user is totaling the concentrations outside of
#' AERMOD, the source groups used in the calculations need to be mutually exclusive, i.e.
#' no one source should be in multiple source groups.
#' 
#' 2. From the total concentrations calculated in step 1, obtain the 1-hr maximum
#' concentration at each receptor for each modeled day.
#' 
#' 3. From the output of step 2, for each year modeled, calculate the 99th percentile (4th
#' highest) daily maximum 1-hour concentration at each receptor. If modeling 5 years of
#' meteorological data, this results in five 99th percentile concentrations at each receptor.
#' 
#' 4. Average the 99th percentile (or 4th highest) concentrations across the modeled years to
#' obtain a design value at each receptor.
#' 
#' 5. Modeled source contributions to a NAAQS violation can be determined by analyzing the
#' hourly concentrations from the individual source groups POSTFILES corresponding to
#' the same hour as the 4th daily maximum 1-hour concentration from each year. See 75 FR
#' at 35540.
#'
#' @name get_dv
#' @param mat An impact matrix, typically a linear combination of impact matrices, which may be the result of a Monte Carlo simulation.
#' @param diag Do not report full calculation. Instead, output a tibble giving the top four daily highs at each receptor x year.
#' @return A double corresponding to the calculated design value.
#' @export
get_dv <- function(mat,diag=FALSE){
	hrbaseline <- attr(mat,'hrbaseline')
	nhr <- dim(mat)[1]
	receps <- paste0("V",1:dim(mat)[2])
tb <- mat %>% as.data.frame %>% tibble::as_tibble() %>%
	dplyr::mutate( `hr` = (hrbaseline + lubridate::hours(1:nhr))) %>%
	dplyr::mutate( `dy` = lubridate::as_date(lubridate::floor_date(`hr`,'days'))) %>%
	dplyr::mutate( `yr` = as.integer(lubridate::year(lubridate::floor_date(`hr`,'years'))) ) %>%
	dplyr::select( `hr`,`dy`,`yr`,tidyselect::all_of(receps)) %>%
	tidyr::pivot_longer(tidyselect::all_of(receps)) %>% dplyr::rename(`recep`=`name`,`conc`=`value`) %>%
	dplyr::group_by(`yr`,`dy`,`recep`) %>%
	dplyr::summarize( `day_hi` = max(`conc`) ) %>%
	dplyr::ungroup() %>%
	dplyr::group_by(`recep`,`yr`) %>%
	dplyr::slice_max(`day_hi`,n=4)  
    if ( diag ) return(tb)
tb %<>% dplyr::slice_min(`day_hi`) %>%  
    dplyr::ungroup() %>%
	dplyr::group_by(`recep`) %>%
	dplyr::summarize( `dv` = mean(`day_hi`) ) %>%
	dplyr::ungroup() %>%
	dplyr::summarize(`DV`=max(`dv`)) %>% dplyr::pull(`DV`) %>%
	suppressMessages
return(tb)}


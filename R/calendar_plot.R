calendar_plot <- function(mat,simno=0,diag=FALSE){
##################################
    hrs_ins <- attr(mat,'hrs_inserted')
    hrbaseline <- attr(mat,'hrbaseline')
    dybaseline <- lubridate::as_date(lubridate::floor_date(hrbaseline,'days'))
    ybaseline <- lubridate::year(hrbaseline+hours(1))
    blank <- tibble::tibble( dy = dybaseline + lubridate::days(1:( (1/24)*dim(mat)[1]) ) ) 
    if ( !is.null(hrs_ins) ) intermitt <- blank %>% 
        dplyr::left_join( tibble::tibble( dy = 
            lubridate::as_date(lubridate::floor_date(hrbaseline+lubridate::hours(hrs_ins),'days')),
            `Daily High`='Intermittent Src') )
    else intermitt <- blank %>% dplyr::mutate( `Daily High` = as.character(NA) )
##################################
    U <- get_dv(mat,diag=TRUE) %>% 
        mutate( HnH = dplyr::dense_rank(dplyr::desc(day_hi)) )
    dv <- U %>% 
        dplyr::filter(HnH == 4) %>% 
        dplyr::ungroup() %>% dplyr::group_by(recep) %>% dplyr::summarize( dv = mean(day_hi) ) %>%
        dplyr::slice_max(dv) 
    U %<>% dplyr::filter( recep == dv$recep ) %>% 
        dplyr::ungroup() %>% dplyr::right_join( blank ) %>% 
        dplyr::arrange(dy) %>% dplyr::select(-yr,-recep) %>% 
        dplyr::mutate(dow = as.integer(format(dy, "%w")), 
            month = lubridate::month(dy), year = lubridate::year(dy)) %>%
        dplyr::mutate(woy = as.integer(format(dy, "%U")) ) %>%
        dplyr::group_by(year,month) %>% 
        dplyr::mutate( wom = as.integer( dplyr::dense_rank(dplyr::desc(woy)) ) ) %>% 
        dplyr::ungroup() %>%
        dplyr::left_join(intermitt) %>%
       dplyr::mutate( `Daily High` = dplyr::case_when( 
            !is.na(`Daily High`) ~ `Daily High` , 
            !is.na(HnH) ~ paste0("H",1:8,"H")[HnH] , 
            TRUE ~ `Daily High` ) ) %>%
        dplyr::mutate( dh_lab = dplyr::case_when( !is.na(day_hi) ~ sprintf("%3.0f",day_hi), 
            TRUE ~ "" ) ) %>%
        dplyr::select(dy,month,year,dow,woy,wom,day_hi,dh_lab,HnH,`Daily High`)
    if ( diag ){return(U)}
    ##################################################
    budge_x <- -0.25
    budge_y <- -0.65
    plot_title <- stringr::str_glue("Simulation {sprintf('%04.0f',simno)}-- DV {sprintf('%4.1f',dv$dv)} ug/m3 @ {dv$recep}") 
    p <- U %>% ggplot2::ggplot() + 
        ggplot2::geom_tile(aes(x=dow,y=wom,fill=`Daily High` ),color="black") + 
        ggplot2::facet_grid( year ~ month ) + 
        ggplot2::scale_fill_brewer(na.value="white",palette="Pastel1") +
        #geom_text( inherit.aes = FALSE , aes(label = jd , x = dow + -1*budge_x , y = woy + 0.6*budge_y ), size = 1.0 ) +
        #geom_text( inherit.aes = FALSE , aes(label = hr , x = dow + budge_x , y = woy2 + 1 + budge_y ), size = 1.5 ) +
        ggplot2::geom_text( inherit.aes = FALSE , aes(label = dh_lab , x = dow , y = wom  ), size = 2.1 ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA, color = NA),
                axis.ticks = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.text.x =  ggplot2::element_blank()) + ggplot2::labs(title=plot_title)
                #strip.placement = "outsite") + 
return(p)
}


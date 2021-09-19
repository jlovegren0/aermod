#' Generate a list of random operating hours, along with
#' the PRNG seeds needed to recreate them.
#' 
#' @name make_simplan
#' @param N_sim The number of sets of operating hours to generate.
#' @param ... Input parameters for call to \code{hrs_rand}.
#' @return a nested list of N_sim elements, each of which is a list with elements \code{seed} and \code{hrs}
#' @export
make_simplan <- function( N_sim = 5 , ... ){
onesim <- function(){ res <- list( seed = .Random.seed ); res$hrs <- hrs_rand(...); res }
purrr::map( 1:N_sim , ~ onesim() )
}


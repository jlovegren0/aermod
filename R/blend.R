#' Blend two impact matrices together.
#'
#' The rows from \code{src} replace corresponding rows in \code{target}
#'
#' @name blend
#' @param src Matrix corresponding to source group for which intermittent operation is simulated.
#' @param indices Hours of operation (integer vector). Typically generated with \code{hrs_rand}
#' @param target Matrix representing source group that does *not* operate when intermittent source does. By default this is the zero matrix (i.e., intermittent source may operate at same time as all other sources).
#' @return Blended matrix. Has an attribute listing all of the hours for which a substitution was made. Existing attributes are carried over from \code{target}.
#' @export
blend <- function(src,indices,target)
{
	if (missing(target)){ target <- 0L * src }
	target[indices,] <- src[indices,]
	attr(target,'hrs_inserted') <- indices
	target
}

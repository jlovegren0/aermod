#' Blend two impact matrices together.
#'
#' The \code{blend} operation inserts rows from a source impact matrix (typically corresponding to an impact matrix for a non-continuous source)
#' into a target impact matrix (typically corresponding to a continuous source that does not operate at the same time as the non-continuous source).
#' Blend also generates an impact matrix which is a zero matrix of the same dimensions as the source matrix, except for the selected rows.
#'
#' Indices are intended to be a set of randomly generated operating hours created by \code{hrs_rand} (which may be created via \code{make_simplan}).
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
	target[indices$hrlist,] <- src[indices$hrlist,]
	target[indices$hrlist,] %<>% `*`(indices$elist)
	attr(target,'hrs_inserted') <- indices
	target
}

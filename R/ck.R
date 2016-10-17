#' ck
#'
#' Compute the sub-surface celerity
#' @param NoL number of storage layers
#' @param gtcel quantile in celerity distribution for overland flow
#' @param Gsh shape par gamma distribution for lower case lambda
#' @param Gsc scale par gamma distribution for lower case lambda
#' @param midDL mean distance (from distance distribution, dd)
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @keywords celerity
#' @export
#' @examples
#' \dontrun{
#' ck()
#' }

ck <-function(NoL,gtcel,Gsh,Gsc,midDL,Timeresinsec) {

  dp <- 1/(NoL-1)

  probvec <- rep(0,NoL)
  for(i in 1:(NoL-1))probvec[i+1] <-i*dp-dp/2  #celerities are estimated at center of level, hence dp/2
  probvec <- 1-probvec
  probvec[1] <-gtcel

  res <-qgamma(probvec,Gsh,1/Gsc)*midDL/Timeresinsec

  return(res)
}

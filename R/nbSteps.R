#' nbSteps
#'
#'  calculates the delay-steps for each level
#' @param maxL max distance (from distance distribution, DD)
#' @param speed celerity
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @keywords celerity
#' @export
#' @examples
#' \dontrun{
#' nbSteps()
#' }
nbSteps <-function(maxL,speed,Timeresinsec) {
  res <- trunc(maxL/(speed*Timeresinsec))+1
  return(res)
}

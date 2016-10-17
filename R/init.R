#' init
#'
#' The function \code{init()} initializes the sub-surface celerity and the delay-steps
#' @param NoL Number of [groundwater] Layers
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param modelk list of parameters about k the different levels of the SOIL
#'  list(gtcel=a,Gsh=b,Gsc=c,midDL=d)
#' @param modelSaturation list of parameters about the saturation
#'  list(gtcel=,CapacityUpperLevel=b ,mLam=c,varLam=d,distr="qgamma")
#' @param modelLayer list of parameters about the Layers
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e), NoL=f)
#' @param modelRiver list of parameters about the river
#'  list(maxL=a,speed=b,nbStepsDelay=c ,z=d,distr="dnorm",param=c(e,f))
#' @param modelBog list of parameters of the bog
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))
#' @param modelMAD list of parameters about the Mean Annual Discharge
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))
#' @return The output is a list of modelayer, modeleRiver, modelBog and modelMAD with an updated celerity and an updated delay-steps
#' @keywords celerity
#' @export
#' @examples
#' \dontrun{
#' init()
#' }

init <-function(NoL,Timeresinsec,modelk,modelSaturation,modelLayer,modelRiver,modelBog,modelMAD) {

  # LAYER
  # Celerity in the different level of the SOIL and number of steps for each level of the SOIL
  k <- ck(NoL=NoL,gtcel=modelk$gtcel,Gsh=modelk$Gsh,Gsc=modelk$Gsc,midDL=modelk$midDL,Timeresinsec=Timeresinsec)
  nbStepsLevel <- nbSteps(maxL=modelLayer$maxL,speed=k,Timeresinsec=Timeresinsec)
  modelLayerUpdate <- modelLayer
  modelLayerUpdate$speed <- k
  modelLayerUpdate$nbStepsDelay <- nbStepsLevel

  # RIVER
  # speed and number of steps for the RIVER
  riverSpeed <- modelRiver$speed
  nbStepsRiv <- nbSteps(maxL=modelRiver$maxL,speed=riverSpeed,Timeresinsec=Timeresinsec)
  modelRiverUpdate <- modelRiver
  modelRiverUpdate$speed <- riverSpeed
  modelRiverUpdate$nbStepsDelay <- nbStepsRiv

  # BOG
  # speed and number of steps for the RIVER
  bogSpeed <- ifelse(is.null(modelBog$speed), k[1]*1,modelBog$speed)
  nbStepsBog <- nbSteps(maxL=modelBog$maxL,speed=bogSpeed,Timeresinsec=Timeresinsec)
  modelBogUpdate <- modelBog
  modelBogUpdate$speed <- bogSpeed
  modelBogUpdate$nbStepsDelay <- nbStepsBog

  #MAD
  nbStepsMAD <- nbSteps(maxL=modelMAD$maxL,speed=modelMAD$speed,Timeresinsec=Timeresinsec)
  modelMADUpdate <- modelMAD
  modelMADUpdate$nbStepsDelay <- nbStepsMAD


  res <- list(modelLayer = modelLayerUpdate,
              modelRiver = modelRiverUpdate,
              modelBog   = modelBogUpdate,
              modelMAD   = modelMADUpdate)

  return(res)
}

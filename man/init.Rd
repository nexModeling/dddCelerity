% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init.R
\name{init}
\alias{init}
\title{init}
\usage{
init(NoL, Timeresinsec, modelk, modelSaturation, modelLayer, modelRiver,
  modelBog, modelMAD)
}
\arguments{
\item{NoL}{Number of [groundwater] Layers}

\item{Timeresinsec}{time resolution of the process in second (1hour: 3600s, ... etc)}

\item{modelk}{list of parameters about k the different saturation levels
list(gtcel=a,Gsh=b,Gsc=c,midDL=d)}

\item{modelSaturation}{list of parameters about the saturation
list(gtcel=,CapacityUpperLevel=b ,mLam=c,varLam=d,distr="qgamma")}

\item{modelLayer}{list of parameters about the Layers
list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e), NoL=f)}

\item{modelRiver}{list of parameters about the river
list(maxL=a,speed=b,nbStepsDelay=c ,z=d,distr="dnorm",param=c(e,f))}

\item{modelBog}{list of parameters of the bog
list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))}

\item{modelMAD}{list of parameters about the Mean Annual Discharge
list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))}
}
\value{
The output is a list of modelayer, modeleRiver, modelBog and modelMAD with an updated celerity and an updated delay-steps
}
\description{
The function \code{init()} initializes the sub-surface celerity and the delay-steps
}
\examples{
\dontrun{
init()
}
}
\keyword{celerity}


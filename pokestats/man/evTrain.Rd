% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evTrain.R
\name{evTrain}
\alias{evTrain}
\title{effort values training}
\usage{
evTrain(
  targetPoke,
  hp = 0,
  hpFill = FALSE,
  hpMax = FALSE,
  atk = 0,
  atkFill = FALSE,
  atkMax = FALSE,
  def = 0,
  defFill = FALSE,
  defMax = FALSE,
  spa = 0,
  spaFill = FALSE,
  spaMax = FALSE,
  spd = 0,
  spdFill = FALSE,
  spdMax = FALSE,
  spe = 0,
  speFill = FALSE,
  speMax = FALSE
)
}
\arguments{
\item{targetPoke}{pokemon data frame to perform effort value training on}

\item{hp}{integer value for HP stat effort value. Default is 0}

\item{hpFill}{Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the HP stat. Default is FALSE}

\item{hpMax}{Boolean, indicates that the user wants to apply 252 effort value points to the HP stat. Default is FALSE}

\item{atk}{integer value for ATK stat effort value. Default is 0}

\item{atkFill}{Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the ATK stat. Default is FALSE}

\item{atkMax}{Boolean, indicates that the user wants to apply 252 effort value points to the ATK stat. Default is FALSE}

\item{def}{integer value for DEF stat effort value. Default is 0}

\item{defFill}{Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the DEF stat. Default is FALSE}

\item{defMax}{Boolean, indicates that the user wants to apply 252 effort value points to the DEF stat. Default is FALSE}

\item{spa}{integer value for SPA stat effort value. Default is 0}

\item{spaFill}{Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the SPA stat. Default is FALSE}

\item{spaMax}{Boolean, indicates that the user wants to apply 252 effort value points to the SPA stat. Default is FALSE}

\item{spd}{integer value for SPD stat effort value. Default is 0}

\item{spdFill}{Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the SPD stat. Default is FALSE}

\item{spdMax}{Boolean, indicates that the user wants to apply 252 effort value points to the SPD stat. Default is FALSE}

\item{spe}{integer value for SPE stat effort value. Default is 0}

\item{speFill}{Boolean, indicates whether the user wants to apply any remaining effort values after all other calculations are completed to the SPE stat. Default is FALSE}

\item{speMax}{Boolean, indicates that the user wants to apply 252 effort value points to the SPE stat. Default is FALSE}
}
\value{
targetPoke
}
\description{
applies effort values to the target pokemon and returns the pokemon frame with calculated final stat and the ev fields filled out
}
\examples{
\dontrun{
evTrain(targetPoke,hp=0,hpFill=FALSE,hpMax=FALSE,atk=0,atkFill=FALSE,atkMax=FALSE,def=0,defFill=FALSE,defMax=FALSE,spa=0,spaFill=FALSE,spaMax=FALSE,spd=0,spdFill=FALSE,spdMax=FALSE,spe=0,speFill=FALSE,speMax=FALSE)
}
}
\keyword{ev,train}

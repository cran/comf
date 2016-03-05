\name{calcPMVPPD}
\alias{calcPMVPPD}
\title{
Calculating PMV and PPD
}
\description{
\code{calcPMVPPD} calculates pmv and ppd indices according to Fanger.
}
\usage{
calcPMVPPD(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, basMet = 58.15)
}
\arguments{
  \item{ta}{
a numeric value presenting air temperature in [degree C]
}
  \item{tr}{
a numeric value presenting mean radiant temperature in [degree C]
}
  \item{vel}{
a numeric value presenting air velocity in [m/s]
}
  \item{rh}{
a numeric value presenting relative humidity [\%]
}
  \item{clo}{
a numeric value presenting clothing insulation level in [clo]
}
  \item{met}{
a numeric value presenting metabolic rate in [met]
}
  \item{wme}{
a numeric value presenting external work in [met]
}
  \item{basMet}{
a numeric value presenting basal metabolic rate [w/m2]
}
}
\details{
All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.
Note that the adjustments in the value for basMet need to made with great cautiousness as the pmv calculation is an empirical model and might not be valid for other values of basMet than the one commonly used.
}
\value{
\code{calcPMVPPD} returns a data.frame with the following items:
\item{pmv}{Predicted mean vote}
\item{ppd}{Predicted percentage dissatisfied}
}
\references{
Fanger, P. O. Thermal Comfort Analysis and Applications in Environmental Engineering McGraw-Hill, New York, 1970.

ISO 7730 Ergonomics of the thermal environment analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005.
}
\author{
Code implemented in to R by Marcel Schweiker. Further contribution by Sophia Mueller.
}

\seealso{
see also \code{\link{calcComfInd}}
}
\examples{
## Calculating pmv
calcPMVPPD(25, 20, .2, 50)
}

\keyword{ manip }
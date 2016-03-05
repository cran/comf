\name{calc2Node}
\alias{calc2Node}
\title{
Calculating Comfort Indices based on the 2-Node-Model
}
\description{
\code{calc2Node} calculates ET, SET, TSENS, DISC, PD, PS and PTS based on the 2-Node-Model by Gagge et al.
}
\usage{
calc2Node(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
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
  \item{pb}{
a numeric value presenting barometric pressure in [torr] or [mmHg]
}
  \item{ltime}{
a numeric value presenting exposure time in [minutes] 
}
  \item{ht}{
a numeric value presenting body height in [cm] 
}
  \item{wt}{
a numeric value presenting body weight in [kg] 
}
  \item{tu}{
a numeric value presenting turbulence intensity in [\%] 
}
  \item{obj}{
a character element, either "set" or "pmvadj". 
}
  \item{csw}{
a numeric value presenting the driving coefficient for regulatory sweating
}
  \item{cdil}{
a numeric value presenting the driving coefficient for vasodilation
}
  \item{cstr}{
a numeric value presenting the driving coefficient for vasoconstriction
}
}
\details{
All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.

The value of \code{obj} defines whether the function will use the version presented in ASHRAE 55-2013 for adjustment of pmv (obj = "pmvadj"), or the original code by Gagge to calculate set (obj = "set").
}
\value{
\code{calc2Node} returns a data.frame with the following items:
\item{et}{Effective temperature}
\item{set}{standard effective temperature}
\item{tsens}{Predicted thermal sensation }
\item{disc}{Predicted discomfort}
\item{ps}{Predicted percentage satisfied with the level of air movement}
\item{pd}{Predicted percentage dissatisfied due to draft}
\item{pts}{Predicted thermal sensation vote based on set}
}
\references{
Gagge, A. P., Fobelets, A. P. and Berglund, L. G. A standard predictive index of human response to the thermal environment, ASHRAE transactions, 1986, 92 (2B), 709-731.

ASHRAE Standard 55-2013. Thermal environmental conditions for human occupancy. American society of heating, Refrigerating and Air-Conditioning Engineering, Atlanta, USA, 2013.
}
\author{
The translation of the formula given in Gagge et al. into VBA-code was done by M. Shukuya, the translation into R-language and comparison with ASHRAE 55-2013 conducted by Marcel Schweiker.
}
\note{
In case one of the variables is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
}
\seealso{
see also \code{\link{calcComfInd}}, \code{\link{calcPtsadj}}
}
\examples{
## Calculation of a single values.
calc2Node(22, 25, .50, 50)
}
\keyword{ manip }
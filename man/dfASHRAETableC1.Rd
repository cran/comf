\name{dfASHRAETableC1}
\alias{dfASHRAETableC1}
\alias{dfASHRAE}
\docType{data}
\title{
Adjusted data from Table C1 of ASHRAE 55-2013
}
\description{
Callibration data for SET model (used for calculation of PMVadj) from ASHRAE 55-2013 Table C1
}
\usage{data(dfASHRAETableC1)}
\format{
  A data frame with 22 rows on the following 11 variables.
  \describe{
    \item{\code{ta}}{a numeric vector of air temperature [degree C]}
	\item{\code{taF}}{a numeric vector of air temperature [degree F]}
    \item{\code{tr}}{a numeric vector of radiant temperature [degree C]}
	\item{\code{trF}}{a numeric vector of radiant temperature [degree F]}
	\item{\code{vel}}{a numeric vector of indoor air velocity [m/s]}
	\item{\code{velFPM}}{a numeric vector of indoor air velocity [fpm]}
    \item{\code{rh}}{a numeric vector of relative humidity [\%]}
	\item{\code{met}}{a numeric vector of metabolic rate [MET]}
    \item{\code{clo}}{a numeric vector of clothing insulation level [CLO]}
    \item{\code{set}}{a numeric vector of standard effective temperature (SET) [degree C]}
    \item{\code{setF}}{a numeric vector of standard effective temperature (SET) [degree F]}   
  }
}
\references{
ASHRAE standard 55-2013. Thermal environmental conditions for human occupancy. American society of heating, Refrigerating and Air-Conditioning Engineering, Atlanta, Usa, 2013
}
\note{
These are partly not the original values from ASHRAE 55-2013 Table C1, as a comparison with the calculation done by this package and the values given by the CBE Thermal Comfort Tool (ASHRAE approved) showed slightly adjusted values. The differences are:
- row 6 should be 26.4 degree C not 26.2
- row 7 should be 34.3 degree C not 33.6
- row 9 should be 24.9 degree C not 24.4
- row 12 should be 20.3 degree C not 20.4
- row 15 should be 31.8 degree C not 31.9
- row 18 should be 32.5 degree C not 32.6
- row 19 should be 37.7 degree C not 38.0
In addition, rows 21 and 22 can only be compared to the function \code{calcSET} in case \code{obj="pmvadj"}, because they are differing according to adjusted version to calc SET for pmvadj.
No changes have been made to the Fahrenheit values, which might be different as well.
}
\examples{
data(dfASHRAETableC1)
head(dfASHRAETableC1)

# using option obj="pmvadj" in order to compare with ASHRAE Table C1
SET <- sapply(seq(nrow(dfASHRAETableC1)), function(x) { calcSET(dfASHRAETableC1$ta[x], 
dfASHRAETableC1$tr[x], dfASHRAETableC1$vel[x], dfASHRAETableC1$rh[x], dfASHRAETableC1$clo[x], 
dfASHRAETableC1$met[x], obj="pmvadj") } ) 
plot(SET~dfASHRAETableC1$set)
data.frame(set=dfASHRAETableC1$set, SET)

# using option obj="set" (standard) in order to calculate values for original SET values
SET2 <- sapply(seq(nrow(dfASHRAETableC1)), function(x) { calcSET(dfASHRAETableC1$ta[x], 
dfASHRAETableC1$tr[x], dfASHRAETableC1$vel[x], dfASHRAETableC1$rh[x], dfASHRAETableC1$clo[x], 
dfASHRAETableC1$met[x], ) } ) 
plot(SET2~dfASHRAETableC1$set)
data.frame(set=dfASHRAETableC1$set, SET2)

}
\keyword{datasets}

\name{calcMRT}
\alias{calcMRT}
\alias{MRT}
\alias{calcSimpleMRT}
\alias{ViewFactor}
\alias{mrt}
\alias{simpleMRT}
\alias{viewfactor}
\title{
Calculating View Factor and MRT
}
\description{
These function calculate view factors and MRT based on room geometry, temperature, and position. Note that this is a preliminary version. Be cautious when using.
}
\usage{
calcMRT(a, b, c, Fmax, A, B, C, D, E, Temperatures)
calcSimpleMRT(length,width,height,position,Temperatures)
}
\arguments{
  \item{a, b, c}{
numeric values presenting distances to surface (calculated by \code{calcSimpleMRT}) [m] 
}
  \item{Fmax}{
a numeric value presenting view factor coefficient 
}
  \item{A, B, C, D, E}{
numeric values presenting coefficients for view factor calculation 
(calculated by \code{calcSimpleMRT})
}
  \item{Temperatures}{
a numeric vector with 6 surface temperatures in the order
(back wall, front wall, left wall, right wall, ceiling, floor) ) [degree C] 
}
  \item{length}{
a numeric value presenting length of room in [m] 
}
  \item{width}{
a numeric value presenting width of room in [m] 
}
  \item{height}{
a numeric value presenting height of room in [m] 
}
  \item{position}{
a numeric vector presenting position of person in room in the order
(distance from left wall, distance from front wall, height of disc) in [m] 
}
}
\details{
The length of variables should be one, except for position and Temperatures.
}
\value{
Both functions return MRT reflecting the view factor.

}
\references{
Coefficients are given on page 33 of "Ergonomics of the thermal environment - Instruments for measuring physical quantities (ISO 7726:1998)".
}
\author{
M. Reza Safizadeh (xlsx code), David Fischer (r code), Marcel Schweiker (r code and documentation)
}
\examples{
## Calc single value of MRT in room with only right angles
length <- 6 # in m
width <- 4
height <- 3
position <- c(2,3,0.6)
Temperatures <- c(21,21,21,21,30,21) # uniform room with ceiling heating

calcSimpleMRT(length,width,height,position,Temperatures)
}
\keyword{ manip }
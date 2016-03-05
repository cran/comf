\name{calcMeanBias}
\alias{calcMeanBias}
\title{
Calculating the Mean Bias between Predicted and Actual Thermal Sensation Vote
}
\description{
\code{calcMeanBias} calculates the mean bias between predicted thermal sensation votes and actual obtained sensation votes
}
\usage{
calcMeanBias(ref, pred)
}
\arguments{
  \item{ref}{
a numeric item or vector containing categorical or continuous actual thermal sensation votes coded from -3 'cold' to +3 'hot'
}
  \item{pred}{
a numeric item or vector containing categorical or continuous predicted thermal sensation votes coded from -3 'cold' to +3 'hot'
}
}
\value{
\code{calcMeanBias} returns a single value presenting the mean bias between actual and predicted thermal sensation votes. 
}
\references{
Humphreys, M. A. and Nicol, J. F. The validity of IsO-pmv for predicting comfort votes in every-day thermal environments, Energy and Buildings, 2002, 34, 667-684

Schweiker, M. and Wagner, A. Exploring potentials and limitations of the adaptive thermal heat balance framework Proceedings of 9th Windsor Conference: Making comfort relevant Cumberland Lodge, windsor, UK, 2016
}
\author{
Marcel Schweiker
}
\seealso{
see also \code{\link{calcTPRTSV}}, \code{\link{calcAvgAcc}}
}
\examples{
## Define data
ref <- rnorm(5) # actual thermal sensation votes

pred <- rnorm(5) # predicted thermal sensation votes

calcMeanBias(ref, pred)
}
\keyword{ manip }

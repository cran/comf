\name{cutTSV}
\alias{cutTSV}
\title{
Binning Thermal Sensation Votes
}
\description{
\code{cutTSV} bins continuous thermal sensation votes.
}
\usage{
cutTSV(pred)
}
\arguments{
  \item{pred}{
a numeric item or vector containing continuous thermal sensation votes coded from -3 'cold' to +3 'hot'
}
}
\details{
Binning is realized including all values lower and equal then -2.5 to be -3, higher than -2.5 and lower or equal -1.5 to be -2, and so on.
}
\value{
\code{cutTSV} returns an item or a vector with binned thermal sensation votes.
}
\author{
Marcel Schweiker
}
\examples{
## define example data
pred <- rnorm(5)

## bin values
cutTSV(pred)
}
\keyword{ manip }

\name{comf-package}
\alias{comf-package}
\alias{comf}
\docType{package}
\title{
Calculation of Common and Less Common Comfort Indices
}
\description{
This package contains several functions to calculate a series of comfort indices:

\tabular{ll}{
	Element \tab Description  \cr
	"all" \tab Calculation of all indices described below  \cr
	"pmv" \tab Predicted mean vote  \cr
	"ppd" \tab Predicted precentage dissatisfied \cr
	"tnHumphreysNV" \tab Neutral temperature in naturally ventilated buildings according to Humphreys 1978 \cr
	"tnHumphreysAC" \tab Neutral temperature in climate-controlled buildings according to Humphreys 1978 \cr
	"tnAuliciems" \tab Neutral temperature according to Auliciems 1981 \cr
	"tAdapt15251" \tab Adaptive comfort temperature according to EN 15251 \cr
	"dTNZ" \tab Distance to thermoneutral zone \cr
	"ATHBpmv" \tab Adaptive thermal heat balance vote based on pmv \cr
	"ATHBset" \tab Adaptive standard effective temperature \cr
	"ATHBpts" \tab Adaptive thermal heat balance vote based on set \cr
	"apmv" \tab Adaptive predicted mean vote according to Yao et al. \cr
	"ptsa" \tab Adaptive predicted thermal sensation vote according to Gao et al. \cr
	"epmv" \tab pmv adjusted with expectancy factor based on Fanger and toftum \cr
	"ptse" \tab Predicted thermal sensation vote based on set and adjusted with expectancy factor according to Gao et al. \cr
	"set" \tab standard effective temperature based on two node model by Gagge et al. \cr
	"et" \tab Effective temperature based on two node model by Gagge et al. \cr
	"tsens" \tab Predicted thermal sensation \cr
	"disc" \tab Predicted discomfort \cr
	"ps" \tab Predicted percentage satisfied with the level of air movement \cr
	"pd" \tab Predicted percentage dissatisfied due to draft \cr
	"pts" \tab Predicted thermal sensation vote based on set \cr
	"HBxst" \tab Human body exergy consumPtion rate using steady state method
}
}
\details{
\tabular{ll}{
Package: \tab comf\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-02-06\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
To create input parameters, the function \code{createCond}, which creates a list of input parameters may be helpful.
The main function of this packages is \code{calcComfInd}, which returns the desired comfort parameters.
}
\author{
Marcel Schweiker in cooperation with Sophia Mueller

Contact: marcel.schweiker@kit.edu

}
\references{
See references in function descriptions.
}
\keyword{ package }
\seealso{
see also \code{\link{createCond}}, \code{\link{calcComfInd}}
}
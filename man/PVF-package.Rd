\name{PVF-package}
\alias{PVF-package}
\alias{PVF}
\docType{package}
\title{
Forecast of Power and Ramps Produced by Grid-Connected PV
Systems
~~ package title ~~
}
\description{
Forecast of power and ramps produced by grid-connected PV systems
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab PVF\cr
Type: \tab Package\cr
Version: \tab 0.11\cr
Date: \tab 2014-10-26\cr
Authors@R: \tab c(person("Marcelo", "Pinho Almeida", email= "marcelopa@iee.usp.br", role='aut'), person("Oscar", "Perpinan Lamigueiro", email="oscar.perpinan@gmail.com", role=c('cre', 'aut')))\cr
URL: \tab http://github.com/oscarperpinan/PVF\cr
BugReports: \tab https://github.com/oscarperpinan/PVF/issues\cr
License: \tab GPL-3\cr
Depends: \tab quantregForest\cr
Imports: \tab sp, raster, zoo, xts, meteoForecast, solaR, parallel, gstat,
rgdal, lattice, latticeExtra\cr
Suggests: \tab rasterVis\cr
Packaged: \tab 2014-11-21 12:08:33 UTC; oscar\cr
Built: \tab R 3.1.1; ; 2014-11-21 12:08:43 UTC; unix\cr
}

Index:
\preformatted{
extractForecast         extractForecast
plotQR                  plotQR
predVarsLocal           predVarsLocal
predictGoal             predictGoal
predictOneDay           predictOneDay
radarDiagram            Radar Diagram
rfPredict               rfPredict
rfScenario              rfScenario
scenarioSet             scenarioSet
targetDiagram           targetDiagram
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Marcelo Pinho Almeida [aut],
  Oscar Perpinan Lamigueiro [cre, aut]

Maintainer: Oscar Perpinan Lamigueiro <oscar.perpinan@gmail.com>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}

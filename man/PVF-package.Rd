\name{PVF-package}
\alias{PVF-package}
\alias{PVF}
\docType{package}
\encoding{UTF-8}


\title{Forecast of AC Power Produced by Grid-Connected PV Systems}

\description{
This package implements methods for forecasting the AC power output of a PV  following a nonparametric approach. It uses as inputs several forecasts of meteorological variables from a Numerical Weather Forecast model, and actual AC power measurements of PV plants. It uses Quantile Regression Forests as the machine learning tool to generate forecasts with a confidence interval. 
}

\details{
  The package includes 4 main functions: \itemize{

  \item{\code{\link{rfPredict}} uses Quantile Regression Forests to predict the median and the quantiles 0.1 and 0.9, using a training set with predictors and past measurements.}
  
  \item{\code{\link{predictPac}} uses \code{rfPredict} with a certain scenario (combination of predictors extracted from a Numerical Weather Prediction model) to produce a forecast for a defined day. The days included in the training set can be chosen with three different methods (days immediately before the day to be predicted, according to the absolute difference between clearness indexes, or according to the similarity between empirical distribution functions of irradiance)}

    \item{\code{\link{rfScenario}} uses \code{predictPac} to produce a forecast for every day included in the training set. This function is useful to asses the performance of a certain scenario and method of days selection.}

    \item{\code{\link{predVarsLocal}} and \code{\link{extractForecast}} use the functions of the \code{meteoForecast} package to build a time series with one or more Numerical Weather Prediction variables from a specified remote source for a time period at a defined location. Each variable is quantified with the value at the location, a spatial interpolation using nearby locations, three spatial variability indexes (tri, tpi and rough) and one time variabiliy index.}
  }

  Additionally \code{PVF} includes a graphical function to display the model performance results, \code{\link{targetDiagram}}, and a auxiliary function to calculate error statistics, \code{\link{PVFstats}}.

}

\author{
Marcelo Pinho Almeida and Oscar Perpinan Lamigueiro

Maintainer: Oscar Perpinan Lamigueiro <oscar.perpinan@gmail.com>
}

\references{
  Meinshausen, N. (2006). Quantile regression forests. The Journal of Machine Learning Research, 7, 983-999.

  Pinho Almeida, M., Perpiñán Lamigueiro, O., and Narvarte L., PV Power Forecast Using a Nonparametric Model, Solar Energy (under review)
}

\keyword{ package }

\seealso{
  \code{\link{meteoForecast-package}}
  \code{\link{quantregForest}}
  \code{\link{raster-package}}
  \code{\link{zoo}}
}


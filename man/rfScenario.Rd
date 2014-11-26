\name{predictPac}

\alias{rfScenario}
\alias{predictPac}
\alias{scenarioSet}
\encoding{UTF-8}

\title{
Forecast of AC power produced by a PV system
}
\description{
Apply Random Forest to predict AC power of a photovoltaic plant with
different combinations of predictors and training set definition methods.
}

\usage{
predictPac(goal, history, id, nDays, method, lat, lon,...)

rfScenario(history, id, nDays, method, typeRes, mc.cores=1,...)

scenarioSet(id, vals)
}

\arguments{

  \item{goal}{A \code{Date} or a character that can be coerced to a
  \code{Date}. It defines the day to be predicted. If \code{method =
  'previous'} it must be included in the time index of the
  \code{history} time series.}
  
  \item{history, vals}{A \code{zoo} object, a time series including historic
  values of the variable to predict (first column of the object), and
  past values of predictors.}

  \item{id}{Character that identifies the scenario (combination of
  predictors to be included in the training set). There are 17 scenarios
  defined in this version of the package. For example, if you wish to use the scenario 3, you have to use \code{id = 'S03'}.
}
  \item{nDays}{numeric, number of days to be included in the training
  set. Read the help page of \code{rfPredict} for details about this
  argument when used with \code{method = 'previous'}.
}
  \item{method}{Character. It must be one of \code{'previous'},
  \code{'ks'}, or \code{'kt'}.}

\item{typeRes}{Character, if \code{typeRes = 'power'} the function \code{rfScenario} provides intradaily results, and daily error statistics with \code{typeRes='stats'}}

\item{lat, long}{Numeric, latitude and longitude of the location in degrees.}

    \item{mc.cores}{}
}
\details{
\code{predictPac} provides the prediction produced by \code{rfPredict}
with the selected scenario and method, for the day defined with \code{goal}. It is a \code{zoo} object, a time series, with a column for each quantile prediction, named \code{q1}, \code{q5}, and \code{q9}. 

\code{rfScenario} uses \code{predictPac} to construct a time series of
forecasts covering the whole extent of the \code{history} time
series. If \code{typeRes = 'power'} the result is a time series with the
same frequency as \code{history}. If \code{typeRes = 'stats'} the result
is a daily time series with error statistics and the clearness index, with a row for each day included in \code{history}.
}

\value{
A \code{zoo} time series.
}


\author{
Marcelo Pinho Almeida and Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{calcSol}}
  \code{\link{local2Solar}}
  \code{\link{ksDistance}}
  \code{\link{rfPredict}}
}
\examples{
}

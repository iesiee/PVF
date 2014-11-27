\name{predictPac}

\alias{rfPredict}
\alias{rfScenario}
\alias{predictPac}
\alias{scenarioSet}

\encoding{UTF-8}

\title{
Quantile Regression Forests for AC photovoltaic power forecasting
}

\description{
Apply Random Forest to predict AC power of a photovoltaic plant with
different combinations of predictors and training set definition methods.
}

\usage{
rfPredict(test, history, nDays)

predictPac(goal, history, id, nDays, method, lat, lon,...)

rfScenario(history, id, nDays, method, typeRes, mc.cores=1,...)

scenarioSet(id, vals)
}

\arguments{
  \item{test}{A \code{zoo} object, a time series including the same predictors as in the \code{history} time series. The prediction will have the same time index as this object.}

  \item{goal}{A \code{Date} or a character that can be coerced to a \code{Date}. It defines the day to be predicted. If \code{method = 'previous'} it must be included in the time index of the \code{history} time series.}
  
  \item{history, vals}{A \code{zoo} object, a time series including historic values of the variable to predict (first column of the object), and past values of predictors.}

  \item{id}{Character that identifies the scenario (combination of predictors to be included in the training set). There are 17 scenarios defined in this version of the package. For example, if you wish to use the scenario 3, you have to use \code{id = 'S03'}.}

  \item{nDays}{numeric. The prediction is computed using a train set of N days from \code{history} located before the \code{test} day. When \code{nDays = 0}, prediction is computed using all the days from history, excluding the \code{test} day. When nDays='all', prediction is computed using all the days from \code{history} including the \code{test} day. This argument is used in \code{rfScenario} only when \code{method = 'previous'}.}

  \item{method}{Character. It defines the method used to select the days included in the training set:
    \itemize{

      \item{\code{'previous'}, selects those N days immediately before the day to be predicted. As a consequence, the database must be complete up to the day prior the prediction.}

      \item{\code{'kt'}, selects N days according to the absolute difference between the clearness index of the day to be predicted and the clearness index of each day included in the database. Both clearness index are computed with the irradiance forecast retrieved from the NWP model. The N days with the lowest absolute difference are chosen to conform the training set. Therefore, the database does not need to be complete up to the day prior the prediction, and could also be composed by older information.}
      
      \item{\code{'ks'}, selects N days according to the similarity between the empirical distribution function of the irradiance forecast for the day to be predicted and the empirical distribution function of the irradiance forecast for each day included in the database. Here the Kolmogorov-Smirnov statistic is used to compute the distance between the distributions. The N days with the lowest Kolmogorov-Smirnov distance are chosen to conform the training set. As in the method KT, the database does not need to be complete up to the day prior the prediction.}
    }
  }

  \item{typeRes}{Character, if \code{typeRes = 'power'} the function \code{rfScenario} provides intradaily results, and daily error statistics with \code{typeRes='stats'}}
  
  \item{lat, lon}{Numeric, latitude and longitude of the location in degrees.}

  \item{mc.cores}{The number of cores to use, i.e. at most how many child processes will be run simultaneously.  Parallelization requires at least two cores. It relies on forking and hence is not available on Windows.}


  \item{...}{Additional arguments for \code{\link{predVarsLocal}}, which can pass additional arguments to \code{\link{getRasterDays}}}

}

\details{
\code{rfPredict} returns a \code{zoo} object, a time series, with a
column for each quantile prediction, named \code{q1}, \code{q5}, and
\code{q9}. The time index is the same as the \code{test} object. This
function uses Quantile Regression Forests to produce the quantile prediction.

\code{predictPac} returns the prediction produced by \code{rfPredict}
with the selected scenario and method, and for the day defined with \code{goal}. It is a \code{zoo} object, a time series, with a column for each quantile prediction, named \code{q1}, \code{q5}, and \code{q9}. 

\code{rfScenario} uses \code{predictPac} to construct a time series of
forecasts for every day included in the \code{history} time series. This
function is indicated to asses the performance of the predictions with a
certain scenario. If \code{typeRes = 'power'} the result is a time
series with the same frequency as \code{history}. If \code{typeRes =
'stats'} the result is a daily time series with error statistics and the
clearness index, with a row for each day included in \code{history}.  }

\value{
A \code{zoo} time series.
}


\author{
Marcelo Pinho Almeida and Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{quantregForest}}
  \code{\link{predict.quantregForest}}
  \code{\link{zoo}}
  \code{\link{calcSol}}
  \code{\link{local2Solar}}
}

\examples{
}

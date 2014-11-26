\name{rfPredict}
\alias{rfPredict}

\encoding{UTF-8}


\title{
Quantile Regression Forests used for forecasting
}
\description{
Returns the quantiles (0.1, 0.5, and 0.9) prediction of a variable with
past values of several predictors included in a time series used as
input for training. This function uses Quantile Regression Forests to
produce the quantile prediction.

}
\usage{
rfPredict(test, history, nDays)
}

\arguments{
  \item{test}{A \code{zoo} object, a time series including the same
  predictors as in the \code{history} time series. The prediction will
  have the same time index as this object.
}
  \item{history}{A \code{zoo} object, a time series including historic
  values of the variable to predict (first column of the object), and past values of the same
  predictors included in the \code{test} object.
}
  \item{nDays}{numeric. The prediction is computed using a train set of
  N days from \code{history} located before the \code{test} 
  day. When \code{nDays = 0}, prediction is computed using all the days
  from history, excluding the \code{test} day. When nDays='all',
  prediction is computed using all the days from \code{history}
  including the \code{test} day.
}
}

\value{
A \code{zoo} object, a time series, with a column for each quantile prediction,
named \code{q1}, \code{q5}, and \code{q9}. The time index is the same as
the \code{test} object.
}

\author{
Marcelo Pinho Almeida and Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{quantregForest}}
  \code{\link{predict.quantregForest}}
  \code{\link{zoo}}
}

\examples{}

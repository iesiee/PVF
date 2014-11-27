\name{PVFstats}
\alias{PVFstats}
\alias{qqStats}
\encoding{UTF-8}

\title{
Error Statistics
}

\description{
Error Statistics
}

\usage{
PVFstats(m, o,
         functions = c("mo", "mm",
                       "sdo", "sdm",
                       "mbe", "mae", "rmse",
                       "nmbe", "cvmbe",
                       "nmae", "cvmae",
                       "nrmse", "cvrmse",
                       "r2", "tStone"))

qqStats(x, q1, q9)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{Numeric, model values}

  \item{o, x}{Numeric, observation values}

  \item{functions}{Character, name of functions to compute model
    errors. The package includes definitions for these functions:
    \itemize{

      \item{Mean: \code{mo}, \code{mm}}
      \item{Standard Deviation: \code{sdo}, \code{sdm}}
      \item{Mean Bias Error: \code{mbe}}
      \item{Mean Absolute Error: \code{mae}}
      \item{Root Mean Square Error: \code{rmse}}
      \item{t of Stone: \code{tStone}}
      \item{Coefficient of determination, \eqn{R^2}: \code{r2}}
    }
    There are also functions that normalize the MBE, MAE and RMSE values. \code{nmbe}, \code{nmae}, and \code{nrmse} use the  observation range as factor of normalization, while \code{cvmbe}, \code{cvmae}, and \code{cvrmse}, use the observation average to normalize.}

  \item{q1, q9}{Numeric, quantiles 0.1 and 0.9}
}

\details{
\code{qqStats} provides several measures of the accuracy of the
confidence interval:
\itemize{
  \item{\code{difSUM}, the amplitude of the confidence interval, its
    area, normalized respect to the area of the observations.}

  \item{\code{difMED}, the median of all the amplitudes of the
    confidence interval normalized respect to the maximum amplitude.}

  \item{\code{uq1NUM}, number of events when the observed value is
    smaller than the quantile 0.1.}

  \item{\code{uq9NUM}, number of events when the observed value is
    larger than the quantile 0.9}

  \item{\code{uq1MED}, median of the differences between the quantile 0.1
  and the observation when the observed value is below the quantile. The
  differences are normalized with the observation value.}

  \item{\code{uq9MED}, median of the differences between the quantile 0.9
  and the observation when the observed value is above the quantile. The
  differences are normalized with the observation value.}
}
}

\value{
A numeric with length equal to the number of functions.
}

\author{
Marcelo Pinho Almeida and Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{targetDiagram}}
}

\examples{
}

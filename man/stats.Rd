\name{qqStats}
\alias{qqStats}
\encoding{UTF-8}

\title{
Error Statistics
}

\description{
Error Statistics
}

\usage{
qqStats(x, q1, q9)
}

\arguments{
  \item{x}{Numeric, observation values}

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

\examples{
}

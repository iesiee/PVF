\name{plotQR}
\alias{plotQR}
\encoding{UTF-8}

\title{
Plot Model and Observations with the Quantile Band
}

\description{
Display the result of a Quantile Regression Forest compared with the observations.
}

\usage{
plotQR(z, vrbl = NULL, col = c("black", "red"), ...)
}

\arguments{

  \item{z}{A \code{zoo} time series, including three columns, 'q1', 'q5', and 'q9', and the variable defined by \code{vrbl} if it is not \code{NULL}. The band comprised between the quantiles Q1 and Q9 is
    displayed with a gray band, and the quantile Q5 and the variable with lines of different colors.}

  \item{vrbl}{Character, name of a column included in \code{z}}

  \item{col}{Character, colors of lines representing the quantile 0.5 (default is black), and of the variable defined by \code{vrbl} (default is red).}

  \item{\dots}{Additional arguments for \code{\link{xyplot}}}

}

\value{
A trellis object.
}

\author{
Oscar Perpiñán Lamigueiro and Marcelo Pinho Almeida.
}

\examples{
}

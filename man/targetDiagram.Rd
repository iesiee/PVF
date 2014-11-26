\name{targetDiagram}
\alias{targetDiagram}

\title{
Target Diagrams
}
\description{
Summary diagrams
}
\usage{

targetDiagram(data, class = "",
              xlab = expression("RMSEc" \%.\% "sign(" * sigma^"*" * ")"),
              ylab = "MBE",
              auto.key = list(space = "right"),
              default.scales = list(cex = 0.6),
              scales = list(),
              type = "quantile",
              cuts = seq(0.25, 1, 0.25), ...)

}
\arguments{

  \item{data}{A \code{data.frame} with these columns: nrmse (Root Mean
    Square Error), nmbe (Mean Bias Error), sdm (Standard Deviation of
    the model), sdo (Standard Deviation of observations)}

  \item{class}{Character, the name of the variable that defines the
    panels of a trellis graphic.}
  
  \item{xlab, ylab}{Character or expression, define the axis labels.}

  \item{auto.key, default.scales, scales}{Legend and scales
    customization. Read the \code{\link{xyplot}} and
    \code{\link{panel.xyplot}} help pages for details.}
  
  \item{type}{Character, defines the type of cuts between 'quantile' and 'at'.}

  \item{cuts}{Numeric, defines the radius of the RMSE circles. If \code{type = 'quantile'} it defines the
    probabilities with values between 0 and 1, and the radius are such
    that the circles are the corresponding quantile. If \code{type =
      'at'} \code{cuts} defines the radius of the circles.}
  
  \item{\dots}{Additional arguments for \code{\link{xyplot}}}

}

\details{
TODO
}

\value{
A trellis object.
}

\references{
TODO: paper
}

\author{
Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{xyplot}}
  \code{\link{radarDiagram}}
}

\examples{
}

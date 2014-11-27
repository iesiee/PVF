\name{indexes}
\alias{idwNear}
\alias{terrainIndex}
\alias{tempIndex}

\encoding{UTF-8}

\title{
  Variability Indexes
}

\description{
Spatial and time variability indexes.
}

\usage{
idwNear(surround, point, nmax, ...)
terrainIndex(surround, point)
tempIndex(runVals)
}

\arguments{
  \item{surround}{A \code{Raster} object. It includes the location
    defined by \code{point}.}
  
  \item{point}{A \code{SpatialPoints*} object, the location of
    interest.}
  
  \item{nmax}{the number of nearest observations that
          should be used for the interpolation.}

\item{runVals}{A \code{zoo} object, a time series, produced by \code{\link{getPointRuns}}}

  \item{\dots}{Additional arguments for \code{\link{idw}}}
}

\details{
\code{idwNear} produces a deterministic interpolation at \code{point}
with the values of \code{surround}.

\code{terrainIndex} computes three spatial variability indexes: TRI,
PI, and roughness. Terrain Ruggedness Index is defined as the mean of
the absolute differences between a central cell and its surrounding 8
cells in a 3 by 3 grid. Topographic Position Index, is defined as the
difference between a central cell and the mean of its surrounding 8
cells in a 3 by 3 grid. Roughness is the largest inter-cell difference
of a central cell and its surrounding 8 cells in a 3 by 3 grid. The
results are normalized with the value at \code{point}.

\code{tempIndex} is a simple function that calculates the standard
deviation by rows.
}

\value{
A \code{data.frame}
}

\references{
Wilson, M. F., O’Connell, B., Brown, C., Guinan, J. C., & Grehan, A. J. (2007). Multiscale terrain analysis of multibeam bathymetry data for habitat mapping on the continental slope. Marine Geodesy, 30(1-2), 3-35.
}

\author{
Marcelo Pinho Almeida and Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{idw}}
  \code{\link{terrain}}
}

\examples{
}


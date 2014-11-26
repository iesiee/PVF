\name{extractForecast}
\alias{extractForecast}
\alias{predVarsLocal}

\encoding{UTF-8}

\title{
Extract forecast with variability indexes
}

\description{
Build a time series with one or more WRF variables from a specified
source for a time period at a defined location.
The variable is quantified with the value at the location, the IDW
interpolation, three terrain indexes (tri, tpi and rough) and one
temporal index.
}

\usage{
extractForecast(point, seqDays, vrbl,
                src='meteogalicia',
                remote=TRUE, dataDir='.', ...)

predVarsLocal(point, seqDays, vrbls, mc.cores = 1, ...)
}


\arguments{
  \item{point}{point represented by a two-column matrix or data.frame,
    or \code{SpatialPoints*}, \code{SpatialPolygons*},
    \code{SpatialLines}, \code{Extent}, or a numeric vector representing
    cell numbers. Read the help page of \code{\link{extract}} for
    additional details.
}
  \item{seqDays}{Sequence of days defined by a \code{Date} object, or
    a character that can be coerced to a \code{Date}.
}
  \item{vrbl, vrbls}{Character. The name(s) of the variable(s) to retrieve. Inspect
    the help page of \code{\link{grepVar}} to know what variables are available in each service.
}
  \item{src}{Character, which service to use, 'meteogalicia', 'gfs',
          'openmeteo', 'nam' or 'rap'. Read the help page of
	  \code{\link{getRaster}} for details.
}
  \item{remote}{Logical. If \code{TRUE} (default) data is downloaded
    from the remote service. If \code{FALSE} the \code{RasterBrick} is
    produced with the files available in the local folder.}

  \item{dataDir}{Character, path of the folder where files are stored (if
  \code{remote = 'FALSE'})}

\item{mc.cores}{The number of cores to use, i.e. at most how many child
          processes will be run simultaneously.  Parallelization
          requires at least two cores. It relies on forking and hence is not available on Windows.
}

  \item{\dots}{Additional arguments for \code{\link{getRasterDays}}}
}

\value{
A time series (using the \code{zoo} class) with a column for the value
at the location, the IDW interpolation, the
spatial variability indexes obtained with \code{\link{terrainIndex}},
and the time variability index.
}

\details{
\code{predVarsLocal} is a wrapper around \code{extractForecast} using
\code{\link{mclapply}} for building a time series for each variable
included in \code{vrbls}. 
}

\author{
Marcelo Pinho Almeida and Oscar Perpiñán Lamigueiro
}

\seealso{
  \code{\link{terrainIndex}}
  \code{\link{getRasterDays}}
  \code{\link{getPointRuns}}
}

\examples{}
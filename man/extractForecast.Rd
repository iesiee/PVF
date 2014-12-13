\name{extractForecast}
\alias{extractForecast}
\alias{predVarsLocal}

\encoding{UTF-8}

\title{
Extract NWP variables forecast with variability indexes
}

\description{
Build a time series with one or more Numerical Weather Prediction variables from a specified remote source for a time period at a defined location. The variable is quantified with the value at the location, a spatial interpolation using nearby points, three spatial variability indexes, and one time variability index.
}

\usage{
extractForecast(point, seqDays, vrbl,
                src='meteogalicia',
                remote=TRUE, dataDir='.',
                sun = FALSE, ...)

predVarsLocal(point, seqDays, vrbls,
              mc.cores = 1,
              sun = TRUE, ...)
}


\arguments{
  \item{point}{point represented by a \code{SpatialPoints*} object, or a
  numeric vector of length 2 (longitude, latitude), in degrees.
  }
  
  \item{seqDays}{Sequence of days defined by a \code{Date} object, or
    a character that can be coerced to a \code{Date}.
  }
  
  \item{vrbl, vrbls}{Character. The name(s) of the variable(s) to retrieve. Inspect
    the help page of \code{\link{grepVar}} to know what variables are available in each service.
  }

  \item{sun}{Logical, if \code{TRUE} sun geometry is computed (solar altitude, azimuth, and extraterrestrial irradiance) and included as additional variables.}

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
  \code{\link{terrainIndex}}, \code{\link{getRasterDays}}, \code{\link{getPointRuns}}, \code{\link{calcSol}}
}

\examples{}
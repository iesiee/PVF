\name{radarDiagram}
\alias{radarDiagram}

\title{
Radar Diagram
}
\description{
Radar Diagram
}
\usage{
radarDiagram(formula, data, ...)
}

\arguments{
  \item{formula}{}
  \item{data}{}


  \item{\dots}{}
}
\details{

}
\value{
 trellis object
}

\author{
Oscar Perpiñán Lamigueiro
}


\examples{
dats <- data.frame(Q1 = runif(12, min = 0.1, max = 0.2),
                   Q5 = runif(12, min = 0.2, max = 0.6),
                   Q9 = runif(12, min = 0.6, max = 1))
dats$var <- c('nMBE','nMAE','nRMSE','R2-1','tStone',
              'difSUM','difMED','uQ1num','uQ1med',
              'uQ9num','uQ9med','var')
dats <- melt(dats, id = 'var', variable.name = 'Q')

radarDiagram(value ~ var|Q, data = dats)

}


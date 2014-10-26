\name{roseDiagram}
\alias{roseDiagram}

\title{
Rose Diagram
}
\description{
Rose Diagram
}
\usage{
roseDiagram(dats, ...)
}

\arguments{
  \item{dats}{}


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

rownames(dats) <- c('nMBE','nMAE','nRMSE','R2-1','tStone',
                    'difSUM','difMED','uQ1num','uQ1med',
                    'uQ9num','uQ9med','var')

roseDiagram(dats)
}


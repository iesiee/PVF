\name{radarDiagram}
\alias{radarDiagram}

\title{
Radar Diagrams
}
\description{
Summary diagrams
}
\usage{

radarDiagram(formula, data,
             auto.key = list(space = "right",
                             points = FALSE,
                             lines = TRUE),
             ...) 

}
\arguments{
  \item{data}{A \code{data.frame}
}
  \item{auto.key}{}
  \item{\dots}{
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

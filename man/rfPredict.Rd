\name{rfPredict}
\alias{rfPredict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rfPredict
}
\description{
rfPredict
}
\usage{
rfPredict(test, history, nDays)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{history}{
%%     ~~Describe \code{history} here~~
}
  \item{nDays}{
%%     ~~Describe \code{nDays} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (test, history, nDays) 
{
    n <- as.character(nDays)
    switch(n, `0` = {
        train <- history[which(!(index(history) \%in\% index(test))), 
            ]
    }, all = {
        train <- history
    }, {
        d <- unique(as.Date(index(test)))[1]
        train <- window(history, start = as.POSIXct(d - nDays) + 
            3600, end = as.POSIXct(d))
    })
    qrfModelH <- quantregForest(train[, -1], as.numeric(train[, 
        1]))
    qrfPredH <- predict(qrfModelH, test)
    colnames(qrfPredH) <- c("q1", "q5", "q9")
    qrfPredH <- zoo(qrfPredH, index(test))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

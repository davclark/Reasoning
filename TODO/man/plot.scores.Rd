\name{plot.scores}
\alias{plot.scores}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.scores(results, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (results, ...) 
{
    "Plot barcharts grouped both ways (by the x and also the y axis of results)"
    dev.new()
    print(barchart(results, stack = F, horizontal = F, auto.key = T, 
        ylab = "Mean Rating", ...))
    dev.new()
    print(barchart(t(results), stack = F, horizontal = F, auto.key = T, 
        ylab = "Mean Rating", ...))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

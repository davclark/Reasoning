\name{fisher.transform.test}
\alias{fisher.transform.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fisher.transform.test(r1, n1, r2 = NULL, n2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r1}{
%%     ~~Describe \code{r1} here~~
}
  \item{n1}{
%%     ~~Describe \code{n1} here~~
}
  \item{r2}{
%%     ~~Describe \code{r2} here~~
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
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
function (r1, n1, r2 = NULL, n2 = NULL) 
{
    "This is a pretty simple test, maybe modify to return more info?"
    z <- 0.5 * log((1 + r1)/(1 - r1))
    if (!is.null(r2)) {
        z <- z - 0.5 * log((1 + r2)/(1 - r2))
        denom <- sqrt((1/(n1 - 3)) + (1/(n2 - 3)))
    }
    else denom <- sqrt(1/(n1 - 3))
    p = pnorm(z/denom)
    p[p > 0.5] <- 1 - p[p > 0.5]
    p <- p * 2
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

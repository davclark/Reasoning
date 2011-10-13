\name{simple.scatter}
\alias{simple.scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simple.scatter(df, groups = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
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
function (df, groups = NULL, ...) 
{
    "splom the way I like it!"
    splom(df, jitter.x = TRUE, jitter.y = TRUE, factor = 3, amount = 0, 
        alpha = 0.5, upper.panel = function(x, y, ...) {
            curr.cor <- cor.test(x, y)
            cor.txt <- sprintf("\%.2f", curr.cor$estimate)
            p.value <- sprintf("\%.2f", curr.cor$p.value)
            n <- curr.cor$parameter + 2
            grid.text(bquote(r == .(cor.txt)), gp = gpar(cex = 0.7), 
                y = unit(0.5, "npc") + unit(1, "lines"))
            grid.text(bquote(p == .(p.value)), gp = gpar(cex = 0.7), 
                y = unit(0.5, "npc"))
            grid.text(bquote(N == .(n)), gp = gpar(cex = 0.7), 
                y = unit(0.5, "npc") - unit(1, "lines"))
        }, groups = groups, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

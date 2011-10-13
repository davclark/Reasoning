\name{main}
\alias{main}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
main(plots = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plots}{
%%     ~~Describe \code{plots} here~~
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
function (plots = FALSE) 
{
    "A sample analysis session"
    resp.df <- read.data("responses.csv")
    good.df <- subset(resp.df, complete \%in\% c("t", "y", "n - 1st q", 
        "complete"))
    cn <- colnames(good.df)
    gw <- c(grep("gw", cn, value = TRUE), "lifestyle")
    evo <- grep("evo", cn, value = TRUE)
    relig <- c("deity1_4", "after1_5", "crea1_6")
    the.rest <- c("knowledgeable", "nat1_3", relig)
    good.df$total.evo <- rowMeans(good.df[evo])
    good.df$total.gw <- rowMeans(good.df[gw])
    good.df$total.relig <- rowMeans(good.df[relig])
    simple.cols <- c("total.evo", "total.gw", "total.relig", 
        "knowledgeable", "nat1_3")
    ranney.cols <- c("total.evo", "total.gw", relig, "knowledgeable", 
        "nat1_3")
    good.df$cond <- paste(good.df$n_s, good.df$pre_post, sep = "_")
    s_pre.df <- subset(good.df, cond == "s_pre")
    if (plots) {
        gw.results <- compute.scores(good.df, gw)
        plot.scores(gw.results, ylim = c(4, 8.5))
        for (s in list(gw, evo, the.rest)) {
            dev.new()
            print(simple.scatter(good.df[, s]))
        }
    }
    return(list(good.df = good.df, gw = gw, evo = evo, the.rest = the.rest))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

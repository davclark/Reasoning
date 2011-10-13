\name{omnibus.consistency.plot}
\alias{omnibus.consistency.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
omnibus.consistency.plot(df, columns, col = gray.colors, fill = "red", other.df = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{columns}{
%%     ~~Describe \code{columns} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{fill}{
%%     ~~Describe \code{fill} here~~
}
  \item{other.df}{
%%     ~~Describe \code{other.df} here~~
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
function (df, columns, col = gray.colors, fill = "red", other.df = NULL, 
    ...) 
{
    "Sort by factor mean and subject mean, plot and return ordered data frame"
    odf <- df[, columns]
    factor.mean <- apply(odf, 2, mean, na.rm = TRUE)
    subj.mean <- apply(odf, 1, mean, na.rm = TRUE)
    odf <- odf[order(subj.mean), order(factor.mean)]
    if (!is.null(other.df)) {
        factor.mean <- apply(other.df, 2, mean, na.rm = TRUE)
        print(order(factor.mean))
        odf <- cbind(odf, other.df[order(subj.mean), order(factor.mean)])
    }
    plotmat <- t(as.matrix(odf))
    na.inds <- which(is.na(plotmat), arr.ind = TRUE)
    print(levelplot(plotmat, col.regions = col, at = seq(0.5, 
        9.5), scales = list(y = list(draw = FALSE), x = list(rot = 60)), 
        xlab = "Sorted Items", ylab = "Sorted Subjects", aspect = "fill", 
        panel = function(...) {
            if (length(na.inds) > 0) 
                panel.rect(0.5, 0.5, ncol(odf) + 0.5, nrow(odf) + 
                  0.5, col = fill, density = 10)
            panel.levelplot(...)
            if (length(na.inds) > 0) 
                panel.points(na.inds[, "row"], na.inds[, "col"], 
                  pch = 8)
        }, ...))
    odf$survey_number <- df[rownames(odf), "survey_number"]
    return(invisible(odf))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

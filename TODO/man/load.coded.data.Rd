\name{load.coded.data}
\alias{load.coded.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load.coded.data(fname = "../coded-responses.csv")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
%%     ~~Describe \code{fname} here~~
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
function (fname = "../coded-responses.csv") 
{
    resp.df <- read.csv(fname, row.names = 1, as.is = c("light_in", 
        "light_out", "know1", "know2", "know3", "surprise_text", 
        "inconvenient_truth_text", "comments", "feedback", "notes"))
    resp.df$pre_post <- ordered(resp.df$pre_post, levels = c("pre", 
        "post"))
    cn <- colnames(resp.df)
    gw <- grep("gw", cn, value = TRUE)
    evo <- grep("evo", cn, value = TRUE)
    resp.df$total.evo <- rowMeans(resp.df[evo])
    resp.df$total.gw <- rowMeans(resp.df[gw])
    s.pre.df <- subset(resp.df, pre_post == "pre")
    all.post.df <- subset(resp.df, pre_post == "post")
    s.post.df <- subset(all.post.df, n_s == "s")
    n.post.df <- subset(all.post.df, n_s == "n")
    post.surprise <- c()
    for (sn in s.pre.df$survey_number) {
        curr <- subset(all.post.df, survey_number == sn, surprise, 
            drop = TRUE)
        if (length(curr) == 1) 
            post.surprise <- c(post.surprise, curr)
        else post.surprise <- c(post.surprise, NA)
    }
    s.pre.df$surprise <- post.surprise
    s.post.df$pre.gw <- subset(s.pre.df, survey_number \%in\% s.post.df$survey_number, 
        select = total.gw, drop = TRUE)
    scores <- c("ghg", "light", "energy", "total.score", "knowledgeable", 
        "total.gw", "lifestyle")
    scores <- intersect(scores, colnames(s.post.df))
    diff.df <- subset(s.pre.df, survey_number \%in\% s.post.df$survey_number, 
        scores)
    for (score in scores) {
        diff.name <- paste(score, "diff", sep = ".")
        s.post.df[, diff.name] <- s.post.df[, score] - diff.df[, 
            score]
    }
    return(list(s.pre = s.pre.df, s.post = s.post.df, n.post = n.post.df, 
        all.post = all.post.df, all = resp.df))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

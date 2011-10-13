\name{read.data}
\alias{read.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.data(fname)
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
function (fname) 
{
    "read the contents of fname, preserve all information apart from case, etc."
    resp.df <- read.csv(fname, stringsAsFactors = FALSE, na.strings = c("NA", 
        "N/A", "\"N/A\"", "n/a", "Bad Data", "see note", "1,5,9", 
        "2&6", "2 & 5"))
    col.patterns <- paste("surprise$", "knowledgeable", "gw", 
        "evo", "nat", "deity", "after", "crea", "lifestyle", 
        "conservative", "careful", "reread", sep = "|")
    likert.cols <- grep(col.patterns, names(resp.df))
    for (n in likert.cols) {
        resp.df[[n]] <- as.double(resp.df[[n]])
    }
    reverse <- c("gw2_1", "evo2_5", "evo2_6")
    for (n in reverse) {
        resp.df[[n]] <- 10 - resp.df[[n]]
    }
    parties <- c("None", "Democrat", "Green", "Independent", 
        "Libertarian", "Republican", "Other", "Decline to state", 
        "")
    faiths <- c("Atheist", "Agnostic", "Buddhist", "Christian", 
        "Hindu", "Jewish", "Muslim", "Spiritual but not religious", 
        "Other", "Decline to state", "")
    ii <- !resp.df$party \%in\% parties
    resp.df$party[ii] <- parties[as.numeric(resp.df$party[ii])]
    ii <- !resp.df$faith \%in\% faiths
    resp.df$faith[ii] <- faiths[as.numeric(resp.df$faith[ii])]
    for (i in colnames(resp.df)) {
        if (is.character(resp.df[[i]])) {
            resp.df[[i]] <- tolower(resp.df[[i]])
        }
    }
    for (n in c("survey_number", "n_s", "pre_post")) {
        resp.df[[n]] <- factor(resp.df[[n]])
    }
    for (n in c("gender", "citizen", "us_born", "english", "party", 
        "faith")) {
        blanks <- which(!is.na(resp.df[[n]]) & resp.df[[n]] == 
            "")
        resp.df[[n]][blanks] <- resp.df[[n]][blanks + 1]
        resp.df[[n]] <- factor(resp.df[[n]])
    }
    resp.df$us_years <- sub("2 years as a student", "2", resp.df$us_years, 
        fixed = TRUE)
    resp.df$us_years <- sub(">40 years", "40", resp.df$us_years, 
        fixed = TRUE)
    resp.df$us_years <- as.numeric(resp.df$us_years)
    return(resp.df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

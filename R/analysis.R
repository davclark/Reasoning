# analysis.R - read in data from responses.csv and perform some data analysis
# Dav Clark - 28 Nov 2010

## Some general hints for other RTMD users - please edit to make more useful!

# I tend to use some libraries that are not part of the standard R distribution.
# Ask me (Dav) if you don't know how to install these libraries. The R gui
# includes a pakcage manager.

# To use this file from R, do something like the following (make sure R is in
# the same directory first):
# source('analysis.R')
# main()
# More generally, main() is just meant to provide an example - but this gets
# you started

# A BETTER way to learn how to use this file would be to look at one of the .Rnw
# files (that should be in the directory with this)

# use edit(resp.df) to view, or even resp.ed.df <- edit(resp.df) to modify the
# data.frame. If you don't like the printing when you close the edit() window,
# try: invisible(edit(resp.df))

# In an ideal world, try to avoid automatic coercion to NA. This allows us to
# catch some mistakes

options(warn=TRUE)

slide.plots <- function() {
    trellis.par.set(fontsize=list(text=20))
}

compute.scores <- function(df, cols, fun=mean) {
    results <- list() # We'll upgrade to a data.frame, then matrix shortly
    for(n in cols) {
        results[[n]] <- tapply(df[[n]], df$cond, fun, na.rm=TRUE)
    }

    # First convert to a data.frame ('cause it'll handle the list properly)
    results <- data.frame(results)

    # Then re-order our rows and coerce to a matrix (better for plotting, etc.)
    return(as.matrix(results[c('s_pre', 's_post', 'n_post'),]))
}

plot.scores <- function(results, ...) {
    'Plot barcharts grouped both ways (by the x and also the y axis of results)'
    dev.new()
    print(barchart(results, stack=F, horizontal=F, auto.key=T, 
                   ylab='Mean Rating', ...))
    dev.new()
    print(barchart(t(results), stack=F, horizontal=F, auto.key=T, 
                   ylab='Mean Rating', ...))
}

fancy.dotplot <- function(df, ...) {
    'Currently works more as an example than a re-usable function'
    dev.new()
    print(xyplot(jitter(knowledgeable) ~ jitter(surprise), df, 
                 type=c('smooth', 'p'), alpha=0.5, groups=n_s, 
                 xlab='jittered surprise rating',
                 ylab='jittered knowledgeable rating', auto.key=T))
}

# XXX - Not sure this is right...
factor.consistency.plot <- function(df) {
    'df should contain only columns for factors we are testing for consistency'
    factor.mean <- apply(df, 1, mean, na.rm=T)
    df <- df[order(factor.mean),]
    
    for(i in 1:length(df)) {
        dev.new()
        y <- df[,i]
        print(
          xyplot(1:length(y) ~ y, jitter.x=T)
        )
    }
}

omnibus.consistency.plot <- function(df, columns, col=gray.colors, fill='red', other.df=NULL, ...) {
    'Sort by factor mean and subject mean, plot and return ordered data frame'
    # We're using gray.colors on order from the boss!
    odf <- df[,columns]

    factor.mean <- apply(odf, 2, mean, na.rm=TRUE)
    subj.mean <- apply(odf, 1, mean, na.rm=TRUE)
    odf <- odf[order(subj.mean), order(factor.mean)]
    if(!is.null(other.df)) {
        # We keep the subject ordering from above, but still sort factors
        factor.mean <- apply(other.df, 2, mean, na.rm=TRUE)
        print(order(factor.mean))
        odf <- cbind(odf, other.df[order(subj.mean),order(factor.mean)])
    }

    plotmat <- t(as.matrix(odf))
    na.inds <- which(is.na(plotmat), arr.ind=TRUE)
    print(
      levelplot(plotmat, col.regions=col, at=seq(0.5,9.5), 
                scales=list(y=list(draw=FALSE),x=list(rot=60)), 
                xlab='Sorted Items', ylab='Sorted Subjects', aspect='fill',
                panel=function (...) {
                    if(length(na.inds) > 0)
                        panel.rect(0.5,0.5,ncol(odf)+0.5,nrow(odf)+0.5,
                                   col=fill, density=10)
                    panel.levelplot(...)
                    if(length(na.inds) > 0)
                        panel.points(na.inds[,'row'], na.inds[,'col'], pch=8)
                },
                ...)
    )

    odf$survey_number <- df[rownames(odf),'survey_number']

    return(invisible(odf))
}

# for some reason, groups seems to mess something up if we don't set it
# explicitly
simple.scatter <- function(df, groups=NULL, ...) {
    'splom the way I like it!'
    # For some reason, jitter.data doesn't work here
    splom(df, jitter.x=TRUE, jitter.y=TRUE, factor=3, amount=0, alpha=0.5,
          # splom isn't terribly friendly. I figured this out mostly
          # by reading the code in panel.pairs...
          upper.panel = function(x, y, ...) {
              # panel.splom(x, y,  ...)
              # Compute cor(relation) and plot it 
              curr.cor <- cor.test(x,y)
              cor.txt <- sprintf('%.2f', curr.cor$estimate)
			  p.value <- sprintf('%.2f', curr.cor$p.value)
              # parameter is df, which is N - 2
              n <- curr.cor$parameter + 2
              # We use grid.text here to be independent of our actual x and y
              # ranges.
              grid.text(bquote(r==.(cor.txt)), gp=gpar(cex=0.7),
                        y=unit(0.5, 'npc') + unit(1, 'lines') )
			  grid.text(bquote(p==.(p.value)), gp=gpar(cex=0.7),
			            y=unit(0.5, 'npc') )
			  grid.text(bquote(N==.(n)), gp=gpar(cex=0.7),
			            y=unit(0.5, 'npc') - unit(1, 'lines') )
          },
          groups=groups,
          ...
         )
}

fisher.transform.test <- function(r1, n1, r2=NULL, n2=NULL) {
    'This is a pretty simple test, maybe modify to return more info?'
    z <- 0.5 * log((1 + r1)/(1 - r1))
    if(!is.null(r2)) {
        z <- z - 0.5 * log((1 + r2)/(1 - r2))
        denom <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
    }
    else
        denom <- sqrt(1 / (n1 - 3))

    p = pnorm(z / denom)

    # Currently we only use a 2-tailed null hyopthesis
    p[p > 0.5] <- 1 - p[p > 0.5]
    p <- p * 2


    return(p)
}

main <- function(plots=FALSE) {
    'A sample analysis session'

    resp.df <- read.data('responses.csv')

    # We can do a more nuanced treatment later, but for now I am simply marking
    # most bad data for non-use (except for the guy missing only one question
    good.df <- subset(resp.df, 
                      complete %in% c('t', 'y', 'n - 1st q', 'complete') )

    # Would be nice to hoist this with some exposition to the preprocessing
    # Rnw doc

    cn <- colnames(good.df)
    gw <- c(grep('gw', cn, value=TRUE), "lifestyle")
    evo <- grep('evo', cn, value=TRUE)
    relig <- c("deity1_4", "after1_5", "crea1_6")
    the.rest <- c("knowledgeable", "nat1_3", relig)

    good.df$total.evo <- rowMeans(good.df[evo])
    good.df$total.gw <- rowMeans(good.df[gw])
    # Note - Michael doesn't much like these!
    good.df$total.relig <- rowMeans(good.df[relig])

    simple.cols <- c('total.evo', 'total.gw', 'total.relig', 'knowledgeable', 'nat1_3')

    # Michael likes religion expanded here
    ranney.cols <- c('total.evo', 'total.gw', relig, 'knowledgeable', 'nat1_3')
    

    good.df$cond <- paste(good.df$n_s, good.df$pre_post, sep='_')

    # This is a general pattern to do with various columns and subsets
    s_pre.df <- subset(good.df, cond == 's_pre')
    # cor(s_pre.df[,ranney.cols], use='pairwise.complete')

    if(plots) {
        gw.results <- compute.scores(good.df, gw)
        plot.scores(gw.results, ylim=c(4,8.5))

        # Plot each global warming, evo, etc. variable against others
        for(s in list(gw, evo, the.rest)) {
            dev.new()
            print(
              simple.scatter(good.df[,s])
            )
        }
    }

    return(list(good.df=good.df, gw=gw, evo=evo, the.rest=the.rest))
}

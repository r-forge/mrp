# TODO: Add comment
# 
# Author: Statistics
###############################################################################

## for ... use: colorFunction=colorblind, lo=-0.25, hi=0.25, alternative="White", minimumProportion=0.01

visualizeThreewayDataset <- function(
        mlm, theta.hat, proportionOfVoters, 
        var1, var2, var3, 
        var2Labels=c("All", levels(var2)), var3Labels=levels(var3),
        outputFilename="maps",
        var1PlotFunction=statemaps,
        caption=paste("2000: State-level support for spending on healthcare, relative to national average of ", round (100*nationalAverage), sep=""),
        ...
) {
    # Make the map  
    ## pdf (paste (outputFilename, ".pdf", sep=""), height=, width=15)
    png (paste (outputFilename, ".png", sep=""), height=1000, width=1000)
    par (mar=c(0,0,2,0), oma=c(0,0,3,0))
    
#===to see the layout, use layout.show()=================
    total.mat <- createLayoutMatrix (nlevels(var2), nlevels(var3))
    layout(total.mat, width=c(3, rep(4, nlevels(var3))), height=c(1,rep(4,nlevels(var2)+1),1.5))
#=====================

    proportionOfVoters[is.na(proportionOfVoters)] <- 0
    nationalAverage <- weighted.mean(theta.hat, proportionOfVoters, na.rm=TRUE)
    for (k in var2Labels){
        blankplot (k, cex=2)
    }
    for(j in var3Labels){
        blankplot(j, cex=2)
    }
    ## For All Voters
    theta.hat.2way <- array (NA, c (nlevels(var1), nlevels(var3)), dimnames=list(levels(var1), levels(var3)))
    for (i in levels(var1)) {
        for (k in levels(var3)) {
            theta.hat.2way[i,k] <- weighted.mean (theta.hat[i,,k], proportionOfVoters[i,,k], na.rm=TRUE)
        }
    }
    
    for (k in levels(var3)){
        #blankplot ("No two way data.", cex=2)
        var1PlotFunction (theta.hat.2way[,k] - nationalAverage, apply (proportionOfVoters[,,k], 1, sum), ...)
    }
    
    for (j in 1:nlevels(var2)){
        for (k in 1:nlevels(var3)){
            var1PlotFunction (theta.hat[,j,k] - nationalAverage, proportionOfVoters[,j,k], ...)
        }
    }
    #par(mar=c(2,0,1,0))
    par(mar=c(3.5,0,1,0))
    pal( diverge_hcl(1001, h = c(60, 190), c = 200, l = c(10, 90)))
    #text(1,   -1, "Yes", xpd=TRUE, cex=2)
    additionalArgs <- list(...)
    hi <- max (0, additionalArgs$hi, na.rm=TRUE)
    lo <- min (0, additionalArgs$lo, na.rm=TRUE)
    text(1,   -1, paste (round ((nationalAverage + hi)*100), "%", sep=""), xpd=TRUE, cex=2)
    text(0.5, -1, paste (round(nationalAverage*100), "%", sep=""), xpd=TRUE, cex=2)
    text(0,   -1, paste (round ((nationalAverage + lo)*100), "%", sep=""), xpd=TRUE, cex=2)
    
#    text(1,   -1, paste (round ((hi)*100), "%", sep=""), xpd=TRUE, cex=2)
#    text(0.5, -1, "national average", xpd=TRUE, cex=2)
#    text(0,   -1, paste (round ((lo)*100), "%", sep=""), xpd=TRUE, cex=2)
    
    
    #text(0,   -1, "No",  xpd=TRUE, cex=2)
    par(mar=c(0,0,0,0))
    blankplot (paste ("\nThe state is left blank where a category represents less than 1% of the voters of a state.", sep=""), cex=1.5)
    mtext(caption, side=3, line=0.5, xpd=TRUE, cex=2, outer=TRUE)
    dev.off()       
}

graphsFor50States <- function (
        theta.hat, ybar.weighted, n.effective, proportionOfVoters, 
        var1Levels, var2Levels, var3Levels,
        var1Labels=var1Levels, var2Labels=c("All", var2Levels), var3Labels=var3Levels,
        outputFilename="maps",
        caption="2000: Raw and estimated support for school vouchers within each state among") {
    
    ## State graphs
    for (k in 1:length(var3Levels)) {
        png (paste (outputFilename, "_", var3Labels[k], ".png", sep=""), height=1000,width=1000)
        par (mar=c(2,2,1,0), tck=0, mgp=c(1.5,.5,0), oma=c(0,0,4,0))
        graph.dims <- c(7,7)
        par (mfrow=graph.dims)
        #sort.state <- rev (order (rvote08))
        count <- 0
        for (i in 1:length(var1Levels)){
            
            count <- count + 1
            if (sum (proportionOfVoters[var1Levels[i],,var3Levels[k]], na.rm=TRUE) > 0.1){
                plot (c(1,5), c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", type="n",
                        yaxs="i")
                if (count%%graph.dims[2]==1)
                    axis (2, c(.02,.5,.95), c("0","50%","100%"), cex.axis=1.1)
                if (count > (48-graph.dims[2]))
                    axis (1, c(1.2,3,4.8), c("poor","mid","rich"), cex.axis=1.2)
                abline (.5, 0, col="gray", lwd=.5)
                fit <- theta.hat[var1Levels[i],,var3Levels[k]]
                pts <- ybar.weighted[var1Levels[i],,var3Levels[k]]
                neff <- n.effective[var1Levels[i],,var3Levels[k]]
                se <- sqrt (fit*(1-fit)/neff)
                #text (3, .9, state.name.long[i], cex=1.3)
                text (3, .9, var1Labels[i], cex=1.3)
                for (j in 1:length(var2Levels)){
                    lines (rep (j, 2), pts[j] + c(-1,1)*se[j], lwd=.5)
                    points (j, min (.98, max (.02, pts[j])), pch=20, cex=1.5)
                }
                lines (1:length(var2Levels), fit, lwd=2)
            }
            else {
                plot (c(1,5), c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", yaxs="i", type="n")
                #text (3, .9, state.name.long[i], cex=1.3)
                text (3, .9, var1Labels[i], cex=1.3)
            }
        }
        mtext (paste (caption, var3Labels[k]), outer=TRUE, cex=1, side=3, line=1)
        dev.off()
    }
}


colorblind <- function (a, lo=min(a,na.rm=TRUE), hi=max(a,na.rm=TRUE),
        n.shades=1001){
    require(colorspace)
    a <- pmin (hi, pmax (lo, a))
    cols <-  diverge_hcl(n.shades, h = c(190, 60), c = 200, l = c(10, 90))
    return (cols[floor (1 + (n.shades-1)*(a-lo)/(hi-lo))])
}

pal <- function(col, border = "light gray"){
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = NA)
}


# Function for a blank plot
blankplot <- function (words, cex=1){
    plot (c(0,1), c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n",type="n")
    text (.5, .5, words, cex=cex)
}

statemaps <- function (a, proportionOfVoters=rep(1, length(a)), colorFunction=c, grayscale=FALSE, alternative="White", minimumProportion=0.01, ...){
    library (maps)
    stopifnot (length (proportionOfVoters) == length(a))
    if (length(a)==51){
        no.dc <- c(1:8,10:51)
        a <- a[no.dc]
        proportionOfVoters <- proportionOfVoters[no.dc]
    }
    if (length(a)==50){
        lower48 <- state.abb!="AK" & state.abb!="HI"
        a <- a[lower48]
        proportionOfVoters <- proportionOfVoters[lower48]
    }
    else if (length(a)!=48) stop ("wrong number of states")
    a <- replace (a, proportionOfVoters < 0.01, NA)
    a <- a * -1
    
    mapping <- list (1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20:22,23:24,25,26,27,28,29,30,31,32,33,34:37,38:40,41,42,43,44,45,46,47,48,49,50,51,52,53:55,56:60,61,62,63)
    # for (i in 1:length(mapping)) print(regions[mapping[[i]]])
    a.long <- rep (NA, 63)
    projection <- "bonne"
    
    for (i in 1:48){
        a.long[mapping[[i]]] <- a[i]
    }
    if (grayscale){
        a.long.scaled <- .95*(a.long-min(a,na.rm=TRUE))/(max(a,na.rm=TRUE)-min(a,na.rm=TRUE))
        shades <- a.long.scaled
        not.dc <- !is.na(a.long.scaled)
        shades[not.dc] <- gray (shades[not.dc])
        map('state', proj=projection, param=25, lty=0, ...)
        m <- map('state', proj=projection, param=25, fill=TRUE, plot=FALSE)
        polygon(m$x,m$y, col=shades, lwd=0.5, border="gray85")
    }
    else {
        colors <- colorFunction (a.long, ...)
        colors <- replace (colors, is.na (colors), alternative)
        
        additionalArgs <- list (...)
        additionalArgs[["lo"]] <- NULL
        additionalArgs[["hi"]] <- NULL
        
        map('state', proj=projection, param=25, lty=0, additionalArgs)
        m <- map('state', proj=projection, param=25, fill=TRUE, plot=FALSE)
        polygon(m$x,m$y, col=colors, lwd=0.5, border="gray85")
    }
}


createLayoutMatrix <- function(nrows, ncols, includeAllRow=TRUE, includeAllCol=FALSE) {
    if (includeAllRow) {
        nrows <- nrows + 1
    }
    if (includeAllCol) {
        ncols <- ncols + 1
    }
    v.idx <- seq(1, nrows)  ## the labels for each row
    h.idx <- c(0, seq(nrows+1, nrows + ncols))
    main.idx <- t(matrix(seq(nrows+ncols+1, nrows + ncols + nrows*ncols), ncols, nrows))
    pal.idx <- c(0,rep(nrows + ncols + nrows*ncols + 1, ncols))
    foot.idx <- c(0, rep(nrows + ncols + nrows*ncols + 2, ncols))
    total.mat <- rbind(c(h.idx), cbind(v.idx, main.idx), pal.idx, foot.idx)
    return (total.mat)
}




visualizeThreewayDatasetWithAll <- function(
        mlm, theta.hat, proportionOfVoters, 
        var1, var2, var3, 
        var2Labels=c("All", levels(var2)), var3Labels=c("All", levels(var3)),
        outputFilename="maps",
        var1PlotFunction=statemaps,
        caption=paste("2000: State-level support for spending on healthcare, relative to national average of ", round (100*nationalAverage), sep=""),
        ...
) {
    # Make the map  
    png (paste (outputFilename, ".png", sep=""), height=1000, width=1000)
    par (mar=c(0,0,2,0), oma=c(0,0,3,0))

#===to see the layout, use layout.show()=================
    total.mat <- createLayoutMatrix (nlevels(var2), nlevels(var3), includeAllCol=TRUE)
    layout(total.mat, width=c(3, rep(4, nlevels(var3)+1)), height=c(1,rep(4,nlevels(var2)+1),1.5))
#=====================
    
    proportionOfVoters[is.na(proportionOfVoters)] <- 0
    nationalAverage <- weighted.mean(theta.hat, proportionOfVoters, na.rm=TRUE)
    
    ## For All Voters
    theta.hat.2way <- array (NA, c (nlevels(var1), nlevels(var3)), dimnames=list(levels(var1), levels(var3)))
    for (i in levels(var1)) {
        for (k in levels(var3)) {
            theta.hat.2way[i,k] <- weighted.mean (theta.hat[i,,k], proportionOfVoters[i,,k], na.rm=TRUE)
        }
    }
    
    theta.hat.2way.2 <- array (NA, c (nlevels(var1), nlevels(var2)), dimnames=list(levels(var1), levels(var2)))
    for (i in levels(var1)) {
        for (j in levels(var2)) {
            theta.hat.2way.2[i,j] <- weighted.mean (theta.hat[i,j,], proportionOfVoters[i,j,], na.rm=TRUE)
        }
    }
    
    
    for (k in var2Labels){
        blankplot (k, cex=2)
    }
    for(j in var3Labels){
        blankplot(j, cex=2)
    }
    
    blankplot ("")
    for (k in levels(var3)){
        var1PlotFunction (theta.hat.2way[,k] - nationalAverage, apply (proportionOfVoters[,,k], 1, sum), ...)
    }
    
    for (j in 1:nlevels(var2)){
        for (k in 0:nlevels(var3)){
            if (k == 0) {
                var1PlotFunction (theta.hat.2way.2[,j] - nationalAverage, apply (proportionOfVoters[,j,], 1, sum), ...)
            } else {
                var1PlotFunction (theta.hat[,j,k] - nationalAverage, proportionOfVoters[,j,k], ...)
            }
        }
    }
    par(mar=c(3.5,0,1,0))
    pal( diverge_hcl(1001, h = c(60, 190), c = 200, l = c(10, 90)))
    additionalArgs <- list(...)
    hi <- max (0, additionalArgs$hi, na.rm=TRUE)
    lo <- min (0, additionalArgs$lo, na.rm=TRUE)
    text(1,   -1, paste (round ((nationalAverage + hi)*100), "%", sep=""), xpd=TRUE, cex=2)
    text(0.5, -1, paste (round(nationalAverage*100), "%", sep=""), xpd=TRUE, cex=2)
    text(0,   -1, paste (round ((nationalAverage + lo)*100), "%", sep=""), xpd=TRUE, cex=2)
    
    par(mar=c(0,0,0,0))
    blankplot (paste ("\nThe state is left blank where a category represents less than 1% of the voters of a state.", sep=""), cex=1.5)
    mtext(caption, side=3, line=0.5, xpd=TRUE, cex=2, outer=TRUE)
    dev.off()       
}

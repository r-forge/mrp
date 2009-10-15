# TODO: Add comment
# 
# Author: Statistics
###############################################################################

source ("MRP/createThreeWayDataset.R")
source ("MRP/visualizeThreewayDataset.R")


generateMap <- function (response, var1, var2, var3, positiveResponse, 
        fitModelFunction, stateLevel, 
        visualizeFunction, proportions, outputFilename, 
        var2Labels, var3Labels, caption, caption2="",
        midpointFunction=weighted.mean,
        lo=-0.25, hi=0.25)
{
    print ("Creating threeway dataset")
    threewayDataset <- createThreeWayDataset (response, var1, var2, var3, positiveResponse=positiveResponse)
    
    print ("Fitting model")
    mlm <- fitModelFunction (var1, var2, var3, threewayDataset$ybar.weighted, threewayDataset$n.effective, stateLevel$income, stateLevel$republicanVote04)
    
    print ("Visualizing three way map")
    visualizeFunction (mlm$mlm, mlm$theta.hat, proportions, 
            var1, var2, var3,
            outputFilename=outputFilename, 
            var1PlotFunction=statemaps, 
            var2Labels=var2Labels,
            var3Labels=var3Labels,
            caption=caption,
            caption2=caption2,
            colorFunction=colorblind, lo=lo, hi=hi, alternative="White", minimumProportion=0.01,
            midpointFunction=midpointFunction)
    
    ## print ("Visualizing post stratified maps")
    ## generateTwoWayMaps (mlm$mlm, mlm$theta.hat, proportions, var1, var2, var3,
    ##         var2Labels, var3Labels,
    ##         outputFilename=outputFilename, caption)
    ## 
    return (list (mlm=mlm, threewayDataset=threewayDataset))
}

generateTwoWayMaps <- function (mlm, theta.hat, proportions, var1, var2, var3,
        var2Labels, var3Labels, caption,
        outputFilename) {
    
    nationalAverage <- weighted.mean(theta.hat, proportions, na.rm=TRUE)
    
    theta.hat.2way <- apply (theta.hat, c(1,3), weighted.mean, na.rm=TRUE)
    png (paste (outputFilename, ".poststratified1", ".png", sep=""), height=250, width=250*nlevels(var2))
    par (mfrow=c(2, nlevels(var3)+1))
    blankplot ("")
    for (label in var2Labels) {
        blankplot (label)
    }
    
    blankplot ("All Voters")
    for (index in levels(var3)) {
        statemaps (theta.hat.2way[,index] - nationalAverage, 
                apply (proportions[,,index], 1, sum), colorFunction=colorblind)
    }
    
    mtext (caption)
    dev.off()
    
    
    theta.hat.2way <- apply (theta.hat, c(1,2), weighted.mean, na.rm=TRUE)
    png (paste (outputFilename, ".poststratified2", ".png", sep=""), height=250, width=250*nlevels(var3))
    par (mfrow=c(1, nlevels(var2)))
    blankplot ("")
    for (label in var3Labels) {
        blankplot (label)
    }
    
    blankplot ("All Voters")
    for (index in levels(var2)) {
        statemaps (theta.hat.2way[,index] - nationalAverage, 
                apply (proportions[,index,], 1, sum), colorFunction=colorblind)
    }
    
    mtext (caption)
    dev.off()
}



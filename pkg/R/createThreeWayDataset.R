# TODO: Add comment
# 
# Author: Statistics
###############################################################################


createThreeWayDataset <- function (y, var1, var2, var3, pop.weight=rep(1, length(y)), positiveResponse=levels(y)[2]) {
    # check data
    stopifnot (length(y) == length(var1), 
               length(y) == length(var2), 
               length(y) == length(var3),
               nlevels(y) == 2)
    
    
    ## Want to generalize: state=var1, income=vaWr2, religionAndEthnicity=var3
    # set up empty arrays
    arrayDimensions <- c(nlevels(var1), nlevels(var2), nlevels(var3))
    arrayDimnames   <- list(levels(var1), levels(var2), levels(var3)) 
    
    # reduce data set to remove NAs
    completeDataSubset <- !is.na (y) & !is.na(var1) & !is.na(var2) & !is.na (var3)
    pop.weight <- pop.weight[completeDataSubset]
    
    y <- y[completeDataSubset]
    var1 <- var1[completeDataSubset]
    var2 <- var2[completeDataSubset]
    var3 <- var3[completeDataSubset]
    
       
    # change y to a 1 / 0 variable with 1 for Yes, 0 for No
    y <- relevel (factor(y, ordered=FALSE), positiveResponse)
    y <- 2-as.numeric(y) 
    
    # set up three way data
    ybar.weighted         <- array (NA, arrayDimensions, dimnames=arrayDimnames) 
    n                     <- array (NA, arrayDimensions, dimnames=arrayDimnames)
    design.effect.by.cell <- array (NA, arrayDimensions, dimnames=arrayDimnames)
    
    for (i in levels(var1)) {
        if (!(i %in% c("AK", "HI", "DC"))) {
            for (j in levels(var2)) {
                for (k in levels(var3)) {
                    subset <- var1==i & var2==j & var3==k
                    ybar.weighted[i,j,k] <- weighted.mean (y[subset], pop.weight[subset])
                    n[i,j,k] <- sum (subset)
                    design.effect.by.cell[i,j,k] <- 1 + var( pop.weight[subset] / mean(pop.weight[subset]))
                }
            }   
        }
    }
    ybar.weighted[is.nan(ybar.weighted)] <- NA
    design.effect <- weighted.mean (design.effect.by.cell[n > 1], n [n > 1], na.rm=TRUE)
    n.effective <- n / design.effect
    
    return (list (ybar.weighted=ybar.weighted, n=n, n.effective=n.effective, design.effect=design.effect, design.effect.by.cell=design.effect.by.cell, pop.weight=pop.weight, dataLength=sum(completeDataSubset)))
}

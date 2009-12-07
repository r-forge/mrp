setClass(Class="ThreeWayData",
    representation=representation(
                numberWays="numeric",
                ybarWeighted = "array", 
                n="array",
                designEffectByCell="array", 
                dataLength="numeric"))

setGeneric ("getNumberWays", function (object) { standardGeneric ("getNumberWays")})
setMethod (f="getNumberWays",
        signature="ThreeWayData",
        definition=function (object) {
            return (object@numberWays)
        })

setGeneric ("getYbarWeighted", function (object) { standardGeneric ("getYbarWeighted")})
setMethod (f="getYbarWeighted",
        signature="ThreeWayData",
        definition=function (object) {
            return (object@ybarWeighted)
        })

setGeneric ("getN", function (object) { standardGeneric ("getN")})
setMethod (f="getN",
        signature="ThreeWayData",
        definition=function (object) {
            return (object@n)
        })


setGeneric ("getDesignEffectByCell", function (object) { standardGeneric ("getDesignEffectByCell")})
setMethod (f="getDesignEffectByCell",
        signature="ThreeWayData",
        definition=function (object) {
            return (object@designEffectByCell)
        })

setGeneric ("getDataLength", function (object) { standardGeneric ("getDataLength")})
setMethod (f="getDataLength",
        signature="ThreeWayData",
        definition=function (object) {
            return (object@dataLength)
        })


setGeneric ("getNEffective", function (object) { standardGeneric ("getNEffective")})
setMethod (f="getNEffective",
        signature="ThreeWayData",
        definition=function (object) {
            return (getN (object) / getDesignEffect (object))
        })

setGeneric ("getDesignEffect", function (object) { standardGeneric ("getDesignEffect")})
setMethod (f="getDesignEffect",
        signature="ThreeWayData",
        definition=function (object) {
            return (weighted.mean (getDesignEffectByCell (object), getN(object), na.rm=TRUE))
        })



newThreeWayData <- function (numberWays, mrp.data) {
    stopifnot (numberWays==3)
    
    dims <- c (nlevels (mrp.data$var1), nlevels (mrp.data$var2), nlevels (mrp.data$var3))
    dimnames   <- list(levels(mrp.data$var1), levels(mrp.data$var2), levels(mrp.data$var3))
    
    ## reduce data set to remove NAs
    completeCases <- complete.cases (mrp.data)
    mrp.data <- mrp.data[completeCases,]
    
    ## change the response variable to a binary variable with 1 for Yes and 0 for No: 
    ## as.numeric() should make the positive response 1 and the negative response 2.
    y <- 2-as.numeric(mrp.data$response) 
    
    ybarWeighted       <- array (NA, dim=dims, dimnames=dimnames) 
    n                  <- array (0, dim=dims, dimnames=dimnames)
    designEffectByCell <- array (0, dim=dims, dimnames=dimnames)

    ## i <- gl (n=nlevels (var1), k=nlevels(var2) * nlevels(var3), labels=levels (var1))
    ## j <- gl (n=nlevels (var2), k=nlevels(var3), )
    ## 
    
    for (i in levels(mrp.data$var1)) {
        for (j in levels(mrp.data$var2)) {
            for (k in levels(mrp.data$var3)) {
                subset <- mrp.data$var1==i & mrp.data$var2==j & mrp.data$var3==k
                ybarWeighted[i,j,k] <- weighted.mean (y[subset], mrp.data$weight[subset])
                n[i,j,k] <- sum (subset)
                designEffectByCell[i,j,k] <- 
                        ifelse (n[i,j,k]==1, 1,
                                1 + var (mrp.data$weight[subset] / mean(mrp.data$weight[subset]))) 
            }
        }
    }
    ybarWeighted[is.nan(ybarWeighted)] <- NA
    
    return (new ("ThreeWayData", 
            numberWays=numberWays,
            ybarWeighted = ybarWeighted, 
            n=n,
            designEffectByCell=designEffectByCell, 
            dataLength=sum(completeCases)))
}

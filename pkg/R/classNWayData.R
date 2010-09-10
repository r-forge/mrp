setClass(Class="NWayData",
            representation=representation(
                numberWays="numeric",
                ybarWeighted = "array", 
                n="array",
                designEffectByCell="array", 
                dataLength="numeric",
                data="data.frame"))


setGeneric ("dataRescale", function (object, ...) { standardGeneric ("dataRescale")})
setMethod (f="dataRescale",
        signature="NWayData",
        definition=function (object, colName, newColName, ...) {
            object@data[newColName] <- rescale (object@data[colName], ...)
            return (object)
        })

## probably kill, merge/join.
setGeneric ("dataLookup", function (object, ...) { standardGeneric ("dataLookup")})
setMethod (f="dataLookup",
        signature=signature(object="NWayData"),
        definition=function (object, colName, newColName, lookupTable, byValue=FALSE) {
            object@data[newColName] <- lookupTable[object@data[[colName]],]
            return (object)           
        })



#### Probably kill this, just a merge() / join.
setGeneric ("dataGenericAugment", function (object, ...) {standardGeneric ("dataGenericAugment")})
setMethod (f="dataGenericAugment",
        signature=signature(object="NWayData"),
        definition=function (object, colNames, newColName, func, ...) {
            object@data[newColName] <- func (object@data[, colNames], ...)
            return (object)           
        })


setGeneric ("getNumberWays", function (object) { standardGeneric ("getNumberWays")})
setMethod (f="getNumberWays",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (object@numberWays)
        })

setGeneric ("getYbarWeighted", function (object) { standardGeneric ("getYbarWeighted")})
setMethod (f="getYbarWeighted",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (object@ybarWeighted)
        })

setGeneric ("getN", function (object) { standardGeneric ("getN")})
setMethod (f="getN",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (object@n)
        })


setGeneric ("getDesignEffectByCell", function (object) { standardGeneric ("getDesignEffectByCell")})
setMethod (f="getDesignEffectByCell",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (object@designEffectByCell)
        })

setGeneric ("getDataLength", function (object) { standardGeneric ("getDataLength")})
setMethod (f="getDataLength",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (object@dataLength)
        })


setGeneric ("getNEffective", function (object) { standardGeneric ("getNEffective")})
setMethod (f="getNEffective",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (getN (object) / getDesignEffect (object))
        })

setGeneric ("getDesignEffect", function (object) { standardGeneric ("getDesignEffect")})
setMethod (f="getDesignEffect",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (weighted.mean (getDesignEffectByCell (object), getN(object), na.rm=TRUE))
        })

setGeneric ("getData", function (object) { standardGeneric ("getData")})
setMethod (f="getData",
        signature=signature(object="NWayData"),
        definition=function (object) {
            return (object@data)
        })

setGeneric ("flattenData", function (object) { standardGeneric ("flattenData")})
setMethod (f="flattenData",
        signature=signature(object="NWayData"),
        definition=function(object) {
            dimLevels <- dimnames (object@ybarWeighted)
            vars <- data.frame(array (NA, dim=c (length(object@ybarWeighted), object@numberWays), dimnames=list(list(), names(dimLevels))))
            carry <- 1
            for (ii in 1:object@numberWays) {
                vars[, ii] <- gl (n=length(dimLevels[[ii]]), k=carry, length=length(object@ybarWeighted), labels=dimLevels[[ii]])
                carry <- carry * length(dimLevels[[ii]])
            }
            
            ybarWeighted <- as.vector (replace (object@ybarWeighted, getNEffective(object)==0, 0.5))
            nEffective <- as.vector (getNEffective (object))
            
            object@data <- data.frame (response.yes=ybarWeighted*nEffective, response.no=(1-ybarWeighted)*nEffective, vars)
            return (object)
        })

newNWayData <- function (numberWays, mrp.data) {
    if ("data.frame" != class (mrp.data)) {
        stop ("Correct usage (data.frame): <response> <var 1> <var 2> ... <var n> <weights>\n")
    } 
    if (numberWays + 2 != ncol(mrp.data)) {
        stop (cat ("mrp.data must have", numberWays+2, "columns. Found: ", length(mrp.data), "\n",
                                           "Correct usage (data.frame): <response> <var 1> <var 2> ... <var n> <weights>\n"))
    }
    dims <- lapply (mrp.data[,2:(1+numberWays)], nlevels)
    dimLevels <- lapply (mrp.data[,2:(1+numberWays)], levels)
    varNames <- names (mrp.data)
    
    ## reduce data set to remove NAs
    completeCases <- complete.cases (mrp.data)
    mrp.data <- mrp.data[completeCases,]
    
    matchingSubsets <- list()
    for (i in 1:numberWays) {
        matchingSubsets[[varNames[i+1]]] <- lapply (dimLevels[[i]], "==", mrp.data[ , i+1])
    }
    
    ## change the response variable to a binary variable with 1 for Yes and 0 for No: 
    ## as.numeric() should make the positive response 1 and the negative response 2.
    y <- 2-as.numeric(mrp.data[ , varNames[1]]) 
    
    ybarWeighted       <- array (NA, dim=dims, dimnames=dimLevels) 
    n                  <- array (0, dim=dims, dimnames=dimLevels)
    designEffectByCell <- array (0, dim=dims, dimnames=dimLevels)
    
    subset <- rep (TRUE, nrow(mrp.data))
    for (ii in 1:length(ybarWeighted)) {
        subset <- rep (TRUE, nrow(mrp.data))
        carry <- 1
        for (jj in 1:numberWays) {
            ind <- ((ii-1) %/% carry) %% dims[[jj]] + 1
            carry <- carry * dims[[jj]]
            subset <- subset & matchingSubsets[[jj]][[ind]] 
        }
        ybarWeighted[ii] <- weighted.mean (y[subset], mrp.data[subset , numberWays + 2])
        n[ii] <- sum (subset)
        designEffectByCell[ii] <- 
                ifelse (n[ii]==1, 1,
                        1 + var (mrp.data[subset, numberWays+2] / mean(mrp.data[subset, numberWays+2])))
    }
    ybarWeighted[is.nan(ybarWeighted)] <- NA
    
    nWayData <- new ("NWayData", 
            numberWays=numberWays,
            ybarWeighted = ybarWeighted, 
            n=n,
            designEffectByCell=designEffectByCell, 
            dataLength=sum(completeCases))
    nWayData <- flattenData (nWayData) 
    return (nWayData)
}

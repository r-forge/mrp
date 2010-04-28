setClass(Class="mrp",
        representation=representation(
                data = "NWayData", 
                formula = "character",
                multilevelModel = "mer",
                theta.hat = "array",
                population = "array"),
        prototype=prototype (
                formula="cbind(response.yes, response.no) ~ 1 + (1 | var1) + (1 | var2) + (1 | var3) + (1 | var1:var2) + (1 | var1:var3) + (1 | var2:var3)"),
        validity=function (object) {
            if (is.null (object@data)) { stop ("[mrp: validation] data must not be null") }
            return(TRUE)
        }
)

## Definining Methods

## Getters and Setters
setMethod (f="getData",
        signature=signature(object="mrp"),
        definition=function(object) {
            return (object@data)   
        })


#setGeneric ("getNumberWays", function (object) { standardGeneric ("getNumberWays") })
setMethod (f="getNumberWays",
        signature=signature(object="mrp"),
        definition=function(object) {
            return (getNumberWays(object@data))   
        })

setGeneric ("setPopulation", function (object, population) { standardGeneric ("setPopulation")})
setMethod (f="setPopulation",
        signature=signature(object="mrp"),
        definition=function(object, population) {
            stopifnot (class (population) == "array")
            #stopifnot (dim (population) == dim (object@theta.hat))

            ## Need more checks here.
            object@population <- replace (population, is.na (population), 0)
            return (object)
        })

setGeneric ("setFormula", function (object, formula) { standardGeneric ("setFormula")})
setMethod (f="setFormula",
        signature=signature(object="mrp"),
        definition=function (object, formula) {
            object@formula <- formula
            return (object)
        })

setGeneric ("getFormula", function (object) { standardGeneric ("getFormula")})
setMethod (f="getFormula",
        signature=signature(object="mrp"),
        definition=function (object) {
            return (object@formula)
        })

setGeneric ("mr", function (object) { standardGeneric ("mr")})
#setGeneric ("multilevelRegression", function (object) { standardGeneric ("multilevelRegression")})
setMethod (f="mr",
        signature=signature(object="mrp"),
        definition=function(object) {
            if (hasMultilevelModel(object) == FALSE) {
                object <- fitMultilevelModel(object)
            }   
            return (object)
        })

setGeneric ("p", function (object, poststratification.specification=rep(FALSE, getNumberWays (object@data))) { standardGeneric ("p")})
#setGeneric ("poststratify", function (object) { standardGeneric ("poststratify")})
setMethod (f="p",
        signature=signature(object="mrp"),
        definition=function (object, poststratification.specification) {
            stopifnot (object@population != numeric(0))
            stopifnot (hasMultilevelModel(object))
            
            poststratified <- object@theta.hat * object@population
            groups <- which (poststratification.specification == TRUE)
            if (length(groups) == 0) {
                return (sum (poststratified, na.rm=TRUE) / sum (object@population)) 
            } else {
                return (apply (poststratified, groups, sum, na.rm=TRUE) / apply (object@population, groups, sum)) ## population should never have NAs
            }
        })


setGeneric ("fitMultilevelModel", function(object) { standardGeneric ("fitMultilevelModel")})
setMethod (f="fitMultilevelModel",
        signature=signature(object="mrp"),
        definition=function (object) {
            object@multilevelModel <- 
                    glmer (formula (object@formula), data=getData(object@data), family=quasibinomial(link="logit"))  
                           
            
            theta.hat <- rep (NA, length (getYbarWeighted (object@data)))
            theta.hat[complete.cases(object@data@data)] <- fitted(object@multilevelModel)
            object@theta.hat <- array (theta.hat, dim (getYbarWeighted(object@data)),
                    dimnames=dimnames (getYbarWeighted(object@data)))
            
            return (object)
        })

setGeneric ("hasMultilevelModel", function (object) { standardGeneric ("hasMultilevelModel")})
setMethod (f="hasMultilevelModel",
        signature=signature(object="mrp"),
        definition=function (object) {
            return (length (object@theta.hat) != 0)
        })


## Data augmentation
setMethod (f="dataRescale",
        signature=signature(object="mrp"),
        definition=function (object, colName, newColName, ...) {
            object@data <- rescaleData (object@data, colName, newColName, ...)
            return (object)
        })

setMethod (f="dataLookup",
        signature=signature(object="mrp"),
        definition=function (object, colName, newColName, lookupTable, byValue=FALSE) {
            object@data <- dataLookup (object@data, colName, newColName, lookupTable, byValue)
            return (object)           
        })

setMethod (f="dataGenericAugment",
        signature=signature(object="mrp"),
        definition=function (object, colNames, newColName, func, ...) {
            object@data <- dataGenericAugment (object@data, colNames, newColName, func, ...)
            return (object)           
        })

newMrp <- function (response, vars, population, weight=rep(1, length(response))) {
    # check inputs
    if ("data.frame" != class(vars)) {
        stop ("vars must be a data.frame.")
    }
    if (length(response) != nrow(vars)) {
        stop ("response must have the same length as the rows as the vars.")
    }
    if (nlevels(response) != 2) {
        stop (paste ("response must have 2 levels, found:", nlevels(response)))
    }
    # make inputs factors
    response <- factor (response)
    for (i in 1:length (vars)) {
        vars[,i] <- factor(vars[,i])
    }

    data <- newNWayData (ncol(vars), data.frame (response, vars, weight))
    if (missing(population)) {
        population <- array (1, dim(data@ybarWeighted)) 
    }
    
    if (all (dim(data@ybarWeighted) == dim (population)) == FALSE) {
        stop (paste ("dim (population) must match dim (data@ybarWeighted).\n\tExpected:", dim (data@ybarWeighted)," but found: ", dim (population)))
    }
    
    return (new(Class="mrp", data=data, population=population))
}

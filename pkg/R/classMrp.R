#source ("MRP/MRP/R/classThreeWayData.R")
#require (lme4)
setClass(Class="mrp",
        representation=representation(
                data = "data.frame",
                numberWays = "integer",
                data.nWay = "ThreeWayData", ## This is not general for now. Once this is up and running, think about how to refactor this into a general n-way dataset
                formula = "character",
                multilevelModel = "mer",
                theta.hat = "array",
                population = "array",
                .debug = "logical"),
        prototype=prototype (
                formula="cbind(response.yes, response.no) ~ 1 + (1 | var1) + (1 | var2) + (1 | var3) + (1 | var1:var2) + (1 | var1:var3) + (1 | var2:var3)"),
        validity=function (object) {
            cat("~~~ mrp: inspector ~~~ \n")
            if (is.null (object@data)) { stop ("[mrp: validation] data must not be null") }
            if (object@numberWays!=3)  { stop (paste ("[mrp: validation] number ways supported for now is 3; found:", object@numberWays)) }
            return(TRUE)
        }
)

## Definining Methods

## Getters and Setters
## setGeneric ("getData", function (object) { standardGeneric ("getData") })
## setGeneric ("setData", function (object) { standardGeneric ("setData") })
## 
## setGeneric ("getNumberWays", function (object) { standardGeneric ("getNumberWays") })
## ## setGeneric ("setNumberWays", function (object) { standardGeneric ("setNumberWays") })
## 
## setGeneric ("getDebug", function (object) { standardGeneric ("getDebug") })
## setGeneric ("setDebug", function (object) { standardGeneric ("setDebug") })

setGeneric ("setPopulation", function (object, population) { standardGeneric ("setPopulation")})
setMethod (f="setPopulation",
        signature="mrp",
        definition=function(object, population) {
            stopifnot (class (population) == "array")
            stopifnot (dim (population) == dim (object@theta.hat))

            ## Need more checks here.
            object@population <- replace (population, is.na (population), 0)
            return (object)
        })

setGeneric ("setFormula", function (object, formula) { standardGeneric ("setFormula")})
setMethod (f="setFormula",
        signature="mrp",
        definition=function (object, formula) {
            object@formula <- formula
            return (object)
        })

setGeneric ("getFormula", function (object) { standardGeneric ("getFormula")})
setMethod (f="getFormula",
        signature="mrp",
        definition=function (object) {
            return (object@formula)
        })

setGeneric ("mr", function (object) { standardGeneric ("mr")})
#setGeneric ("multilevelRegression", function (object) { standardGeneric ("multilevelRegression")})
setMethod (f="mr",
        signature="mrp",
        definition=function(object) {
            if (hasNWayData(object) == FALSE) {
                object <- createNWayData (object)
            }
            if (hasMultilevelModel(object) == FALSE) {
                object <- fitMultilevelModel(object)
            }   
            return (object)
        })

setGeneric ("p", function (object, poststratification.specification=rep(FALSE, object@numberWays)) { standardGeneric ("p")})
#setGeneric ("poststratify", function (object) { standardGeneric ("poststratify")})
setMethod (f="p",
        signature="mrp",
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

setGeneric ("createNWayData", function (object) { standardGeneric ("createNWayData")})
setMethod (f="createNWayData",
        signature="mrp",
        definition=function (object) {
            object@data.nWay <- newThreeWayData (object@numberWays, object@data)
            return (object)
        })

setGeneric ("hasNWayData", function (object) { standardGeneric ("hasNWayData")})
setMethod (f="hasNWayData",
        signature="mrp",
        definition=function (object) {
            return (length(object@data.nWay@numberWays) != 0)
        })

setGeneric ("fitMultilevelModel", function(object) { standardGeneric ("fitMultilevelModel")})
setMethod (f="fitMultilevelModel",
        signature="mrp",
        definition=function (object) {
            # unwrap each of the indexes 
            # (there should be a much better way of doing this by combining
            #  this with the ThreeWayData class, but save it for another day)
            ## var1Levels <- dimnames (getYbarWeighted (object@data.nWay))[[1]]
            ## var2Levels <- dimnames (getYbarWeighted (object@data.nWay))[[2]]
            ## var3Levels <- dimnames (getYbarWeighted (object@data.nWay))[[3]]
            ## 
            ## var1 <- gl (n=length(var1Levels), k=1, length=length(getYbarWeighted(object@data.nWay)), labels=var1Levels)
            ## var2 <- gl (n=length(var2Levels), k=length(var1Levels), length=length(getYbarWeighted(object@data.nWay)), labels=var2Levels)
            ## var3 <- gl (n=length(var3Levels), k=length(var1Levels)*length(var2Levels), length=length(getYbarWeighted(object@data.nWay)), labels=var3Levels)
            
            #prepare data
            ## TODO: What does this mean exactly? 
            # I think we could get the same effect by replacing all NA's 
            # adjust for having 0 effective n
            ## ybarWeighted <- as.vector (replace (getYbarWeighted(object@data.nWay), getNEffective(object@data.nWay)==0, 0.5))
            ## nEffective <- as.vector (getNEffective (object@data.nWay))
            ## 
            ## response <- cbind (ybarWeighted * nEffective, (1-ybarWeighted)*nEffective)
            ## renormalize data that can be renormalized
            #require (arm)
            ## if (is.ordered (object@data$var1)) {
            ##     z.var1 <- rescale (var1)  
            ## }
            ## if (is.ordered (object@data$var2)) {
            ##     z.var2 <- rescale (var2)  
            ## }
            ## if (is.ordered (object@data$var3)) {
            ##     z.var3 <- rescale (var3)  
            ## }
            ##
            object@multilevelModel <- 
                    glmer (formula (object@formula), data=getData(object@data.nWay), 
                           family=quasibinomial(link="logit")) 
            
            theta.hat <- rep (NA, length (ybarWeighted))
            theta.hat[complete.cases(ybarWeighted)] <- fitted(object@multilevelModel)
            object@theta.hat <- array (theta.hat, dim (getYbarWeighted(object@data.nWay)),
                    dimnames=dimnames (getYbarWeighted(object@data.nWay)))
            
            return (object)
        })

setGeneric ("hasMultilevelModel", function (object) { standardGeneric ("hasMultilevelModel")})
setMethod (f="hasMultilevelModel",
        signature="mrp",
        definition=function (object) {
            return (length (object@theta.hat) != 0)
        })

## Constructor
newMrp <- function (response, var1, var2, var3, weight=rep(1, length(response)), positiveResponse=levels(response)[1], formula=NULL) {
    stopifnot (length (response) == length(var1), length (response) == length (var2), length(response) == length (var3))
    stopifnot (is.factor (response), is.factor (var1), is.factor (var2), is.factor (var3))
    stopifnot (levels(response) != 2)
    response <- relevel (response, positiveResponse)
    
    if (is.null (formula)) {
        return (new(Class="mrp", data=data.frame (response=response, var1=var1, var2=var2, var3=var3, weight=weight), numberWays=as.integer(3)))    
    }
    else {
        return (new(Class="mrp", data=data.frame (response=response, var1=var1, var2=var2, var3=var3, weight=weight), numberWays=as.integer(3), formula=formula))
    }
}

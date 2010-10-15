setClass(Class="mrp",
         representation=representation(
           poll = "NWayData", 
           data = "data.frame",
           formula = "formula",
           multilevelModel = "mer",
           population = "NWayData"),
         prototype=prototype (
           formula=as.formula("cbind(response.yes, response.no) ~ 1 + (1 | var1) + (1 | var2) + (1 | var3) + (1 | var1:var2) + (1 | var1:var3) + (1 | var2:var3)")),
         validity=function (object) {
            if (is.null (object@data)) {
              stop ("[mrp: validation] flattened data is missing")
            }
            if (is.null (object@poll)) {
              stop("[mrp: validation] poll NWayData is missing.")
            }
            if (is.null (object@population)) {
              stop("[mrp: validation] pop NWayData is missing.")
            }
            if (is.null (object@formula)) {
              stop("[mrp: validation] formula is missing.")
            }
            return(TRUE)
          }
)

mrp <- function(formula,
                poll, poll.weights=1,
                population=NULL, use=NULL,
                population.formula=formula,
                add=NULL, mr.formula=NULL,
                ...) {
   pop <- population
  mrp.formula <- as.formula(formula)
  mrp.terms <- terms(mrp.formula)  
  mrp.varnames <- attr(mrp.terms,"term.labels")
  population.formula <- update(mrp.formula,population.formula)
  population.terms <- terms(population.formula)
  population.varnames <- attr(terms(population.formula),"term.labels")
  population.varnames <- reorder.popterms(mrp.varnames,population.varnames)
  
  allvars <- all.vars(mrp.formula)
  poll <- na.omit(poll[,allvars])
  ## Set up and store poll NWayData
  cat("\nMaking NWay poll data:\n")
  if (sum(mrp.varnames %in% names(poll)) != length(mrp.varnames) ) {
       stop(paste("\nVariable ",sQuote(mrp.varnames[!(mrp.varnames %in% names(poll))])," not found in poll data."))
     }
  poll.nway <- daply(poll, .variables=mrp.varnames, pop=FALSE,
             .fun=makeNWay, .progress="text",
             response=as.character(mrp.formula[[2]]), weights=poll.weights)
  poll.nway <- new("NWayData",poll.nway,type="poll",
                   levels=saveNWayLevels(poll))
  data <- adply(poll.nway, .margins=1:getNumberWays(poll.nway), 
             flattenNWay,
             design.effect=getDesignEffect(poll.nway))
  data <- restoreNWayLevels(df=data,nway=poll.nway)
  ## Do merges and eval expressions on the data
  data.expressions <- add[sapply(add, is.expression)]
  data.merges <- add[sapply(add, is.data.frame)]
  data$finalrow <- 1:nrow(data)
  if(length(data.expressions)>0){
    data <- within(data,sapply(data.expressions, eval.parent, n=2))
  }
  ## Attempt merges. 
  if(length(data.merges)>0){
    for(d in 1:length(data.merges)){
      data <- join(data,data.merges[[d]],type="left")
    }
  }

  ## ## These are an obnoxious hack but we *really* don't want it
  ## ## to get confused about the order of the data.
    ### UPDATE: should be fixed by using plyr::join instead of base::merge
  ## data <- data[order(data$finalrow),]
  ## rownames(data) <- data$finalrow
  
  if (!is.null(pop)) { ## set up and store population NWayData
    if(is.data.frame(pop)) {

      ## construct the population array based on population formula
      ## next, repeat it across any extra dimensions in poll
      
      na.omit(pop[, {names(pop) %in% c(population.varnames$inpop, use)}])
      cat("\nMaking NWay population data:\n")    
      if (sum(population.varnames$inpop %in% names(pop)) != length(population.varnames$inpop) ) {
        stop(paste("\nVariable ",sQuote(population.varnames$inpop[!(population.varnames$inpop %in% names(pop))])," not found in population."))
      }
      if(!(identical(sapply(poll[,population.varnames$inpop], levels), 
                     sapply(pop[,population.varnames$inpop], levels)) )) {
        sapply(population.varnames$inpop, function(x) {
          if(length(levels(poll[,x])) != length(levels(pop[,x]))){
            warning("Non-conformable population array. Poststratification will not work unless factor levels are identical. You can still get raw estimates.",call.=FALSE)
            warning(paste("For",sQuote(x),
                          "poll has", length(levels(poll[,x])),
                          "; pop has",length(levels(pop[,x]))),call.=FALSE)
          }
        })}
      pop.nway <- daply(pop, .variables=unlist(population.varnames$inpop),
                        .fun=makeNWay,pop=TRUE,weights=use,
                        .progress="text"
                        )
      pop.nway <- array(rep(pop.nway, length(poll.nway)),
                        dim(getNEffective(poll.nway)), dimnames(getNEffective(poll.nway)))
      
      pop.nway <- new("NWayData",pop.nway,type="pop",
                      levels=saveNWayLevels(pop))
    }
    ## if (is.NWayData(pop)) {
    ##   if(!identical(getNumberWays(pop), getNumberWays(poll))){
    ##     warning(paste("Population (",
    ##                   paste(attr(getNumberWays(pop),"ways"),collapse=" + "),
    ##                   ")\nis different from poll (",
    ##                   paste(attr(getNumberWays(poll),"ways"),collapse=" + "),")",sep=""))
    ##   }
    ##   pop.nway <- pop
    ## }
  } else { ## No population supplied
    pop.nway <- makeOnesNWay(poll.nway)
  }
  ## build the default formula unless one has been supplied
  mr.f <- formula(paste("response ~",
                              paste(paste("(1|",
                                          mrp.varnames,")"),
                                    collapse="+"))
                        )
  if (!is.null(mr.formula)){ 
    mr.f <- update.formula(mr.f, mr.formula)
  }
  mrp <- new("mrp",
             poll=poll.nway,
             data=data,
             formula=mr.f,
             population=pop.nway
             )
  cat("\nRunning Multilevel Regression step.\n")
  response <- as.matrix(getResponse(mrp))
  mrp <- mr(mrp, ...)
  return(mrp)
  
}

  ## For population array, if there are "ways" present in poll but constant
  ## in population, move those terms to the end. Will become constant (1s).
  reorder.popterms <- function(poll,pop){
    inpop <- poll[poll%in%pop]
    notinpop <- poll[!{poll%in%pop}]
    
    return(list(inpop=inpop,out=notinpop))
  }
  
## Definining Methods

## Getters and Setters
setGeneric("getData", function(object) {standardGeneric("getData")})
setMethod (f="getData",
        signature=signature(object="mrp"),
        definition=function(object) {
            return (object@data)   
        })

setGeneric("getResponse", function(object) {standardGeneric("getResponse")})
setMethod( f="getResponse",
          signature=signature(object="mrp"),
          definition=function(object) {
            return(as.matrix(object@data[,c("response.yes","response.no")]))
          })
          
setMethod (f="getNumberWays",
        signature=signature(object="mrp"),
        definition=function(object) {
          poll <- getNumberWays(object@poll)
          pop <-  getNumberWays(object@population)
          return (c(poll=poll,pop=pop))   
        })

setGeneric ("getPopulation", function (object) { standardGeneric ("getPopulation")})
setMethod (f="getPopulation",
		signature=signature(object="mrp"),
		definition=function(object) {
			stopifnot (class (object) == "mrp")
		
			return (object@population)
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

setGeneric ("setPopOnes", function (object) { standardGeneric ("setPopOnes")})
setMethod (f="setPopOnes",
        signature=signature(object="mrp"),
           definition=function(object) {
             if(object@population@type=="pop") {
               warning("Population appears to contain real data. Replacing with ones!")
             }
             object@population <- makeOnesNWay(object@poll)
             return(object)

           } )

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

setGeneric ("getThetaHat", function (object) { standardGeneric ("getThetaHat")})
setMethod(f="getThetaHat",signature(object="mrp"),
          definition=function(object) {
            theta.hat <- rep (NA, length (getYbarWeighted (object@poll)))
#           theta.hat[complete.cases(object@data)] <- fitted(object@multilevelModel)
            theta.hat <- fitted(object@multilevelModel)
            theta.hat <- array (theta.hat,
                                       dim (getYbarWeighted(object@poll)),
                                       dimnames=dimnames (getYbarWeighted(object@poll)))
            return(theta.hat)
          })

setGeneric ("mr", function (object,formula,...) { standardGeneric ("mr")})
                                        #setGeneric ("multilevelRegression", function (object) { standardGeneric ("multilevelRegression")})
setMethod (f="mr",
signature=signature(object="mrp"),
definition=function(object,mr.formula=NULL,...) {
  if(is.null(mr.formula)) {
    fm <- object@formula
  } else {
    fm <- update.formula(object@formula, mr.formula)
    object@formula <- fm
  }
  response <- as.matrix(getResponse(object))
  object@multilevelModel <- glmer(fm,
                                  data=object@data,
                                  family=quasibinomial(link="logit"),...)
  return (object)
})


setGeneric ("poststratify", function (object, formula=NULL) { standardGeneric ("poststratify")})
#setGeneric ("poststratify", function (object) { standardGeneric ("poststratify")})
setMethod (f="poststratify",
           signature=signature(object="mrp"),
           definition=function (object, formula=NULL) {
             spec <- formula
             if(is.null(object@population)) {
               warning("Object does not contain population data;\nestimates returned instead.")
               return(getThetaHat(object));
             }
             if(is.null(spec)){
               spec <- rep(FALSE,getNumberWays(object@poll))
             }
             if(is.formula(spec)){
               spec <- attr(terms(spec),"term.labels")
             }
             stopifnot (object@population != numeric(0)) 
             stopifnot (hasMultilevelModel(object))
             
             poststratified <- getThetaHat(object) * object@population

             if(!is.logical(spec)){
               groups <- match(spec,
                               attr(getNumberWays(object@poll),"ways"))
             } else {
               groups <- which (spec == TRUE)
             }
             if (length(groups) == 0) {
               return (sum (poststratified, na.rm=TRUE) / sum (object@population)) 
             } else {
               ans <- (apply (poststratified, groups, sum, na.rm=TRUE) /
                       apply(object@population, groups, sum))
               ans[is.nan(ans)] <- NA
               return(ans)
             }
           })


setGeneric ("hasMultilevelModel", function (object) { standardGeneric ("hasMultilevelModel")})
setMethod (f="hasMultilevelModel",
        signature=signature(object="mrp"),
        definition=function (object) {
            return (length (fitted(object@multilevelModel)) != 0)
        })



## newMrp <- function (response, vars, population, weight=rep(1, length(response))) {
##     # check inputs
##     if ("data.frame" != class(vars)) {
##         stop ("vars must be a data.frame.")
##     }
##     if (length(response) != nrow(vars)) {
##         stop ("response must have the same length as the vars.")
##     }
##     if (nlevels(response) != 2) {
##         stop (paste ("response must have 2 levels, found:", nlevels(response)))
##     }
##     # make inputs factors
##     response <- factor (response)
##     for (i in 1:length (vars)) {
## 		if (is.factor (vars[,i]) == FALSE) {
## 			vars[,i] <- factor(vars[,i])	
## 		}
##     }

##     data <- newNWayData (ncol(vars), data.frame (response, vars, weight))
##     if (missing(population)) {
##         population <- array (1, dim(data@ybarWeighted)) 
##     }
    
##     if (all (dim(data@ybarWeighted) == dim (population)) == FALSE) {
##       stop (paste ("dim (population) must match dim (data@ybarWeighted).\n\tExpected:", dim (data@ybarWeighted)," but found: ", dim (population)))
##     }
    
##     formula <- paste ("cbind (response.yes, response.no) ~ 1 +",
##                       paste ("(1 | ", names (vars), ")", sep="", collapse=" + "))
##     return (new(Class="mrp", data=data, population=population, formula=formula))
##   }

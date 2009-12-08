#library (RUnit)
#source ("MRP/MRP/R/classMrpVisualizer.R")

createFakeData <- function (n=100) {
    #library (maps)
    response <- factor (rep (c("Yes", "No"), length.out=n))
    response <- relevel (response, "Yes")
    var1 <- factor (rep (state.abb, length.out=n))
    var2 <- factor (rep (1:3, length.out=n))
    var3 <- factor (rep (1:5, length.out=n))
    weight <- rep (1, length.out=n)
    
    return (data.frame (response=response, var1=var1, var2=var2, var3=var3, weight=weight))
}

createMrp <- function (n=100) {
    fakeData <- createFakeData()
    mrp <- newMrp (fakeData$response, fakeData$var1, fakeData$var2, fakeData$var3, fakeData$weight)
    mrp <- mr (mrp)
    
    population <- array (1, dim=dim (mrp@theta.hat), dimnames=dimnames (mrp@theta.hat))
    mrp <- setPopulation (mrp, population)
    
    return (mrp)
}

statemaps <- function (data, average, proportionOfVoters=rep(1, length(data)), colorFunction=c, grayscale=TRUE, alternative="White", minimumProportion=0.01, border="gray85", ...){
    library (maps)
    stopifnot (length (proportionOfVoters) == length(data))
    data <- data - average
    if (length(data)==51){
        no.dc <- c(1:8,10:51)
        data <- data[no.dc]
        proportionOfVoters <- proportionOfVoters[no.dc]
    }
    if (length(data)==50){
        lower48 <- state.abb!="AK" & state.abb!="HI"
        data <- data[lower48]
        proportionOfVoters <- proportionOfVoters[lower48]
    }
    else if (length(data)!=48) stop ("wrong number of states")
    data <- replace (data, proportionOfVoters < 0.01, NA)
    data <- data * -1
    
    mapping <- list (1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20:22,23:24,25,26,27,28,29,30,31,32,33,34:37,38:40,41,42,43,44,45,46,47,48,49,50,51,52,53:55,56:60,61,62,63)
    # for (i in 1:length(mapping)) print(regions[mapping[[i]]])
    data.long <- rep (NA, 63)
    projection <- "bonne"
    
    for (i in 1:48){
        data.long[mapping[[i]]] <- a[i]
    }
    if (grayscale){
        data.long.scaled <- .95*(data.long-min(a,na.rm=TRUE))/(max(a,na.rm=TRUE)-min(a,na.rm=TRUE))
        shades <- data.long.scaled
        not.dc <- !is.na(data.long.scaled)
        shades[not.dc] <- gray (shades[not.dc])
        map('state', proj=projection, param=25, lty=0, ...)
        m <- map('state', proj=projection, param=25, fill=TRUE, plot=FALSE)
        polygon(m$x,m$y, col=shades, lwd=0.5, border=border)
    }
    else {
        colors <- colorFunction (data.long, ...)
        colors <- replace (colors, is.na (colors), alternative)
        
        additionalArgs <- list (...)
        additionalArgs[["lo"]] <- NULL
        additionalArgs[["hi"]] <- NULL
        
        map('state', proj=projection, param=25, lty=0, additionalArgs)
        m <- map('state', proj=projection, param=25, fill=TRUE, plot=FALSE)
        polygon(m$x,m$y, col=colors, lwd=0.5, border=border)
    }
}


test.mrp <- createMrp()

test.creation <- function () {
    mrpVisualizer <- newMrpVisualizer ()
}

test.visualize <- function () {
    mrpVisualizer <- newMrpVisualizer ()
    visualize (mrpVisualizer, test.mrp)
}




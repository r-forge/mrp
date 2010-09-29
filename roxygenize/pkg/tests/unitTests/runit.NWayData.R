createFakeData <- function (n=3, length=100) {
    response <- factor (rep (c("Yes", "No"), length.out=length))
    response <- relevel (response, "Yes")
    var1 <- factor (rep (state.abb, length.out=length))
    var2 <- factor (rep (1:3, length.out=length))
    var3 <- factor (rep (1:5, length.out=length))
    weight <- rep (1, length.out=length)
    
    return (data.frame (response=response, var1=var1, var2=var2, var3=var3, weight=weight))
}

test.creation <- function () {
    fakeData <- createFakeData ()
    nWayData <- newNWayData (3, fakeData)
    checkEquals (3, nWayData@numberWays)
    checkTrue (is.null (nWayData@ybarWeighted) == FALSE)
    checkTrue (is.null (nWayData@n) == FALSE)
    checkTrue (is.null (nWayData@designEffectByCell) == FALSE)
    checkTrue (is.null (nWayData@n) == FALSE)
    checkTrue (is.null (nWayData@dataLength) == FALSE)
}

test.creation.fail <- function () {
    mrp.data <- createFakeData() 
    checkException (newNWayData (4, mrp.data))
}

test.getters <- function () {
    rawData <- createFakeData()
    nWayData <- newNWayData (3, rawData)
    
    checkEquals (3, getNumberWays (nWayData))
    checkEquals (c(length(state.abb), 3, 5), dim (getYbarWeighted (nWayData)))

    indexToCheck <- round (runif(5, 1, 100))
    for (i in indexToCheck) {
        expectedResponse <- ifelse (rawData[i,"response"]=="Yes", 1, 0)
        checkEquals (expectedResponse * rawData[i,"weight"], 
                getYbarWeighted(nWayData)[rawData[i,"var1"], rawData[i,"var2"], rawData[i,"var3"]])
    }
    
    for (i in indexToCheck) {
        checkEquals (1, getN(nWayData)[rawData[i,"var1"], rawData[i,"var2"], rawData[i,"var3"]])
    }
    
    for (i in indexToCheck) {
       checkEquals (1, getDesignEffectByCell(nWayData)[rawData[i,"var1"], rawData[i,"var2"], rawData[i,"var3"]])
    }
    
    checkEquals (100, getDataLength(nWayData))
    
    checkEquals (1, getDesignEffect(nWayData))

    ## getNEffective
}

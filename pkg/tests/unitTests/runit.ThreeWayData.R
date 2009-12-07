library (RUnit)
source ("MRP/MRP/R/classThreeWayData.R")

createFakeData <- function (n=100) {
    library (maps)
    response <- factor (rep (c("Yes", "No"), length.out=n))
    response <- relevel (response, "Yes")
    var1 <- factor (rep (state.abb, length.out=n))
    var2 <- factor (rep (1:3, length.out=n))
    var3 <- factor (rep (1:5, length.out=n))
    weight <- rep (1, length.out=n)
    
    return (data.frame (response=response, var1=var1, var2=var2, var3=var3, weight=weight))
}

test.creation <- function () {
    threeWayData <- newThreeWayData (3, createFakeData())
    checkEquals (3, threeWayData@numberWays)
    checkTrue (is.null (threeWayData@ybarWeighted) == FALSE)
    checkTrue (is.null (threeWayData@n) == FALSE)
    checkTrue (is.null (threeWayData@designEffectByCell) == FALSE)
    checkTrue (is.null (threeWayData@n) == FALSE)
    checkTrue (is.null (threeWayData@dataLength) == FALSE)
}

test.creation.fail <- function () {
    mrp.data <- createFakeData() 
    checkException (newThreeWayData (4, mrp.data))
}

test.getters <- function () {
    rawData <- createFakeData()
    threeWayData <- newThreeWayData (3, rawData)
    
    checkEquals (3, getNumberWays (threeWayData))
    checkEquals (c(length(state.abb), 3, 5), dim (getYbarWeighted (threeWayData)))

    indexToCheck <- round (runif(5, 1, 100))
    for (i in indexToCheck) {
        expectedResponse <- ifelse (rawData[i,"response"]=="Yes", 1, 0)
        checkEquals (expectedResponse * rawData[i,"weight"], 
                getYbarWeighted(threeWayData)[rawData[i,"var1"], rawData[i,"var2"], rawData[i,"var3"]])
    }
    
    for (i in indexToCheck) {
        checkEquals (1, getN(threeWayData)[rawData[i,"var1"], rawData[i,"var2"], rawData[i,"var3"]])
    }
    
    for (i in indexToCheck) {
       checkEquals (1, getDesignEffectByCell(threeWayData)[rawData[i,"var1"], rawData[i,"var2"], rawData[i,"var3"]])
    }
    
    checkEquals (100, getDataLength(threeWayData))
    
    checkEquals (1, getDesignEffect(threeWayData))

    ## getNEffective
}

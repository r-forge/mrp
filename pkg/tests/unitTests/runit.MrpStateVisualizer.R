#library (RUnit)
#source ("MRP/MRP/R/classMrpStateVisualizer.R")

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

#test.mrp <- createMrp()

test.creation <- function () {
    mrpStateVisualizer <- newMrpStateVisualizer ()
}

test.visualize <- function () {
    mrpStateVisualizer <- newMrpStateVisualizer ()
    visualize (mrpStateVisualizer, test.mrp)
}


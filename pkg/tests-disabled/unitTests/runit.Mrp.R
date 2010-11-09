## Use case: happy path

## 1) create object with data.
##    - implicit is the validity check
## 2) run multilevel regression (mr)
##    - check that multilevel regression is done correctly
##    - this means running the "standard" model
## 3) poststratify
##    - check that average values match up
##    - verify that we can graph every thing properly.
##
createFakeData <- function (n=100) {
    response <- factor (rep (c("Yes", "No"), length.out=n))
    response <- relevel (response, "Yes")
    var1 <- factor (rep (state.abb, length.out=n))
    var2 <- factor (rep (1:3, length.out=n))
    var3 <- factor (rep (1:5, length.out=n))
    #var2 <- factor (rep (letters[1:3], length.out=n))
    #var3 <- factor (rep (letters[1:5], length.out=n))
    weight <- rep (1, length.out=n)
    
    return (data.frame (response=response, var1=var1, var2=var2, var3=var3, weight=weight))
}

createFakePopulation <- function () {
    population <- expand.grid (state.abb, factor (1:3), factor(1:5))
    population <- cbind (population, rep(1, nrow(population))) 
    names (population) <- c("var1", "var2", "var3", "population")
    
    return (population)
}


test.creation <- function () {
    fakeData <- createFakeData()
    fakePopulation <- createFakePopulation()

    fakeData$response <- as.numeric (as.character (factor (fakeData$response, labels=c(1,0))))
    
    obj <- mrp (formula = response ~ var1 + var2 + var3,
                poll=fakeData,
                poll.weights="weight",
                population=fakePopulation,
                use="population")
    
    checkEqualsNumeric (3, getNumberWays(obj)["poll"])
    checkTrue (all (getPopulation (obj)@.Data == 1))
}

test.multilevelRegression <- function () {
    fakeData <- createFakeData()
    mrp <- newMrp (fakeData$response, fakeData[,2:4], createFakePopulation(), weight=fakeData$weight)
    mrp <- mr (mrp)
    
    ## TODO: add some asserts here.
}


test.poststratify <- function () {
    fakeData <- createFakeData()
    mrp <- newMrp (fakeData$response, fakeData[,2:4], weight=fakeData$weight)
    mrp <- mr (mrp)
    
    population <- array (1, dim=dim (mrp@theta.hat), dimnames=dimnames (mrp@theta.hat))
    mrp <- setPopulation (mrp, population)
    
    reference.dim <- dim (mrp@theta.hat)
    
    estimate <- p (mrp) # should return a single number indicating the whole estimate
    checkEquals (1, length(estimate))
    checkTrue (estimate > 0 & estimate < 1)
    
    estimate2 <- p (mrp, c(FALSE, FALSE, FALSE)) # should return the same number
    checkEquals (estimate, estimate2)
    
    estimate3 <- p (mrp, c(TRUE, FALSE, FALSE)) # should return a vector with an estimate for each state
    checkEquals (reference.dim[1], length (estimate3))
    checkEquals (estimate, mean (estimate3))
    
    estimate4 <- p (mrp, c(TRUE, TRUE, FALSE)) # should return a vector with an estimate for each state
    checkEquals (reference.dim[1:2], dim (estimate4))
    checkEquals (estimate, mean (estimate4))
    
    estimate5 <- p (mrp, c(TRUE, TRUE, TRUE)) # should return a vector with an estimate for each state
    checkEquals (reference.dim, dim (estimate5))
    checkEquals (estimate, mean(estimate5))
    
    
    population <- array (1/length (mrp@theta.hat), dim=dim (mrp@theta.hat), dimnames=dimnames (mrp@theta.hat))
    mrp <- setPopulation (mrp, population)
    new.estimate <- p (mrp) # should return a single number indicating the whole estimate
    checkEquals (estimate, new.estimate)
    
    new.estimate2 <- p (mrp, c(FALSE, FALSE, FALSE)) # should return the same number
    checkEquals (estimate2, new.estimate2)
    
    new.estimate3 <- p (mrp, c(TRUE, FALSE, FALSE)) # should return a vector with an estimate for each state
    checkEquals (estimate3, new.estimate3)
    
    new.estimate4 <- p (mrp, c(TRUE, TRUE, FALSE)) # should return a vector with an estimate for each state
    checkEquals (estimate4, new.estimate4)
    
    new.estimate5 <- p (mrp, c(TRUE, TRUE, TRUE)) # should return a vector with an estimate for each state
    checkEquals (estimate5, new.estimate5)
}

## mrp.ex1 <- new(Class="mrp")
## mrp.ex1
## 

## Alternative:
## 2) run multilevel regression with different model
## test.multilevelRegressionWithCustomModel <- function () {
##     ## checkEquals (0, 1)
## }


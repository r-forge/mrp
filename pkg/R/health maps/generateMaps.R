rm (list=ls())

### Load data
##### Load All Data
## Annenberg 2000 Data
source ("Annenberg/load.naes2000.R")
source ("Annenberg/load.naes2004.R")

## Census Data
source ("Census Data/loadCensusData.R")
censusStateAgeIncome <- createThreewayCensusData (census, "state", "age", "income")
propStateAgeIncome <- array (NA, dim=dim(censusStateAgeIncome), dimnames=dimnames(censusStateAgeIncome))
dimnames(propStateAgeIncome) <- list(years=dimnames(censusStateAgeIncome)[[1]], state=levels(naes00$state), age=levels(naes00$age), income=levels(naes00$income))
for (y in dimnames (censusStateAgeIncome)[[1]]){
    for (i in dimnames (censusStateAgeIncome) [[2]]) {
        propStateAgeIncome[y,i,,] <- censusStateAgeIncome[y,i,,] / sum (censusStateAgeIncome[y,i,,]) 
    }
}

## State Level Data
# TODO: load Statelevel
source ("MRP/example - favor school voucher/loadStateLevelData.R")
stateLevel <- loadStateLevelData ()


###### load all files

source ("MRP/example - healthcare/healthcare2000FitModel.R")
source ("MRP/health maps/generateMap.R")


################ healthcare 2000, 2004
healthcare2000 <-
    generateMap (naes00$healthFavorSpendingOnUninsured, naes00$state, naes00$age, naes00$income, positiveResponse="Yes",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2000",,,],
            outputFilename="MRP/health maps/maps/healthcare2000-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Should federal gov't spend more money on health care for the uninsured (2000 survey)?")
            #midpointFunction=function(x,y,na.rm){ return (0.5)})

healthcare2004 <-
    generateMap (naes04$healthFavorSpending, naes04$stateAbbreviation, naes04$age, naes04$income, positiveResponse="Yes",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2004",,,],
            outputFilename="MRP/health maps/maps/healthcare2004-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Should federal gov't spend more money on health care for the uninsured (2004 survey)?")
            #midpointFunction=function(x,y,na.rm){ return (0.5)})

#Can you just do one-way graphs of each of the variables (D-vote, liberal, Democrat) vs. age and vs. income? 

##########################
# different looks for 2000
## lo <- max (abs (range ((healthcare2000$mlm)$theta.hat, na.rm=TRUE) - 0.5)) * -1; hi <- lo * -1
## visualizeThreewayDatasetWithoutPoststratification ((healthcare2000$mlm)$mlm, (healthcare2000$mlm)$theta.hat, propStateAgeIncome["2000",,,], 
##         naes00$state, naes00$age, naes00$income,
##         outputFilename="MRP/health maps/maps/health2000-1", 
##         var1PlotFunction=statemaps, 
##         var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),
##         var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
##         caption="Should federal gov't spend more money on health care for the uninsured?",
##         colorFunction=colorblind, lo=lo, hi=hi, alternative="White", minimumProportion=0.01)
##         #midpointFunction=function(x,y,na.rm){ return (0.5)})
## 
## lo <- max (abs (range ((healthcare2000$mlm)$theta.hat, na.rm=TRUE) - 0.5)) * -0.8; hi <- lo * -1
## visualizeThreewayDatasetWithoutPoststratification ((healthcare2000$mlm)$mlm, (healthcare2000$mlm)$theta.hat, propStateAgeIncome["2000",,,], 
##         naes00$state, naes00$age, naes00$income,
##         outputFilename="MRP/health maps/maps/health2000-2", 
##         var1PlotFunction=statemaps, 
##         var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),
##         var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
##         caption="Should federal gov't spend more money on health care for the uninsured?",
##         colorFunction=colorblind, lo=lo, hi=hi, alternative="White", minimumProportion=0.01)
##         #midpointFunction=function(x,y,na.rm){ return (0.5)})
## 
## lo <- max (abs (range ((healthcare2000$mlm)$theta.hat, na.rm=TRUE) - 0.5)) * -0.75; hi <- lo * -1
## visualizeThreewayDatasetWithoutPoststratification ((healthcare2000$mlm)$mlm, (healthcare2000$mlm)$theta.hat, propStateAgeIncome["2000",,,], 
##         naes00$state, naes00$age, naes00$income,
##         outputFilename="MRP/health maps/maps/health2000-3", 
##         var1PlotFunction=statemaps, 
##         var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),
##         var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
##         caption="Should federal gov't spend more money on health care for the uninsured?",
##         colorFunction=colorblind, lo=lo, hi=hi, alternative="White", minimumProportion=0.01)
##         #midpointFunction=function(x,y,na.rm){ return (0.5)})

##########################
# different looks for 2004
## lo <- max (abs (range ((healthcare2004$mlm)$theta.hat, na.rm=TRUE) - 0.5)) * -1; hi <- lo * -1
## source ("MRP/health maps/generateMap.R")
## source ("MRP/visualizeThreewayDataset.R")
## visualizeThreewayDatasetWithoutPoststratification ((healthcare2004$mlm)$mlm, (healthcare2004$mlm)$theta.hat, propStateAgeIncome["2004",,,], 
##         naes04$stateAbbreviation, naes04$age, naes04$income,
##         outputFilename="MRP/health maps/maps/health2004-1", 
##         var1PlotFunction=statemaps, 
##         var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),
##         var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
##         caption="Should federal gov't spend more money on health care for the uninsured?",
##         colorFunction=colorblind, lo=lo, hi=hi, alternative="White", minimumProportion=0.01)
##         #midpointFunction=function(x,y,na.rm){ return (0.5)})
## 
## lo <- max (abs (range ((healthcare2004$mlm)$theta.hat, na.rm=TRUE) - 0.5)) * -0.8; hi <- lo * -1
## visualizeThreewayDatasetWithoutPoststratification ((healthcare2004$mlm)$mlm, (healthcare2004$mlm)$theta.hat, propStateAgeIncome["2004",,,], 
##         naes04$stateAbbreviation, naes04$age, naes04$income,
##         outputFilename="MRP/health maps/maps/health2004-2", 
##         var1PlotFunction=statemaps, 
##         var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),
##         var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
##         caption="Should federal gov't spend more money on health care for the uninsured?",
##         colorFunction=colorblind, lo=lo, hi=hi, alternative="White", minimumProportion=0.01)
##         #midpointFunction=function(x,y,na.rm){ return (0.5)})
## 
## lo <- max (abs (range ((healthcare2004$mlm)$theta.hat, na.rm=TRUE) - 0.5)) * -0.75; hi <- lo * -1
## visualizeThreewayDatasetWithoutPoststratification ((healthcare2004$mlm)$mlm, (healthcare2004$mlm)$theta.hat, propStateAgeIncome["2004",,,], 
##         naes04$stateAbbreviation, naes04$age, naes04$income,
##         outputFilename="MRP/health maps/maps/health2004-3", 
##         var1PlotFunction=statemaps, 
##         var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),
##         var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
##         caption="Should federal gov't spend more money on health care for the uninsured?",
##         colorFunction=colorblind, lo=lo, hi=hi, alternative="White", minimumProportion=0.01)
##         #midpointFunction=function(x,y,na.rm){ return (0.5)})




################ democratic vote: 2000, 2004
##  Gore / (Bush + Gore)
y <- rep (NA, length (naes00$votedGeneralElection))
y <- replace (y, naes00$votedGeneralElection=="Bush", "Bush")
y <- replace (y, naes00$votedGeneralElection=="Gore", "Gore")
y <- factor (y)

dvote2000 <- 
    generateMap (y, naes00$state, naes00$age, naes00$income, positiveResponse="Gore",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2000",,,],
            outputFilename="MRP/health maps/maps/dvote2000-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Did you vote for Al Gore?", 
            caption2="Analysis only included respondents who expressed a preference for Gore or for Bush.\n")

##  Kerry / (Bush + Kerry)
y <- rep (NA, length(naes04$votedBushKerryNader))
y <- replace(y, naes04$votedBushKerryNader=="Bush", "Bush")
y <- replace(y, naes04$votedBushKerryNader=="Kerry", "Kerry")
y <- factor (y)

dvote2004 <- 
    generateMap (y, naes04$stateAbbreviation, naes04$age, naes04$income, positiveResponse="Kerry",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2004",,,],
            outputFilename="MRP/health maps/maps/dvote2004-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Did you vote for John Kerry?",
            caption2="Analysis only included respondents who expressed a preference for Kerry or for Bush.\n")




## intend: Gore / (Bush + Gore)
y <- rep (NA, length(naes00$intendGeneralElection))
y <- replace(y, naes00$intendGeneralElection=="Bush", "Bush")
y <- replace(y, naes00$intendGeneralElection=="Gore", "Gore")
y <- factor (y)


intendDvote2000 <- 
            generateMap (y, naes00$state, naes00$age, naes00$income, positiveResponse="Gore",
                            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2000",,,],
                            outputFilename="MRP/health maps/maps/intendDvote2000-StateAgeIncome",
                            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
                            caption="Do you plan to vote for Al Gore?",
                            caption2="Analysis only included respondents who expressed a preference for Gore or for Bush.\n")


## intend: Kerry / (Bush + Kerry)
y <- rep (NA, length(naes04$intendBushKerryNader))
y <- replace(y, naes04$intendBushKerryNader=="Bush", "Bush")
y <- replace(y, naes04$intendBushKerryNader=="Kerry", "Kerry")
y <- factor (y)

intendDvote2004 <- 
            generateMap (y, naes04$stateAbbreviation, naes04$age, naes04$income, positiveResponse="Kerry",
                            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2004",,,],
                            outputFilename="MRP/health maps/maps/intendDvote2004-StateAgeIncome",
                            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
                            caption="Do you plan to vote for John Kerry?",
                            caption2="Analysis only included respondents who expressed a preference for Kerry or for Bush.\n")


################ liberal: 2000, 2004
### Liberal / (Liberal + Conservative): 2000
y <- rep (NA, length(naes00$pidConservativeOrLiberal))
y <- replace (y, naes00$pidConservativeOrLiberal=="Very conservative", "Conservative")
y <- replace (y, naes00$pidConservativeOrLiberal=="Conservative", "Conservative")
y <- replace (y, naes00$pidConservativeOrLiberal=="Very liberal", "Liberal")
y <- replace (y, naes00$pidConservativeOrLiberal=="Liberal", "Liberal")
y <- factor (y, ordered=TRUE)

liberal2000 <- 
    generateMap (y, naes00$state, naes00$age, naes00$income, positiveResponse="Liberal",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2000",,,],
            outputFilename="MRP/health maps/maps/liberal2000-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Do you identify yourself as liberal (2000 survey)?",
            caption2="Analysis only includes respondents who identified as liberal or conservative.\n")

### Liberal / (Liberal + Conservative)
y <- rep (NA, length(naes04$pidConservativeOrLiberal))
y <- replace (y, naes04$pidConservativeOrLiberal=="Very conservative", "Conservative")
y <- replace (y, naes04$pidConservativeOrLiberal=="Conservative", "Conservative")
y <- replace (y, naes04$pidConservativeOrLiberal=="Very liberal", "Liberal")
y <- replace (y, naes04$pidConservativeOrLiberal=="Liberal", "Liberal")
y <- factor (y, ordered=TRUE)

liberal2004 <-
    generateMap (y, naes04$stateAbbreviation, naes04$age, naes04$income, positiveResponse="Liberal",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2004",,,],
            outputFilename="MRP/health maps/maps/liberal2004-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Do you identify yourself as liberal (2004 survey)?",
            caption2="Analysis only includes respondents who identified as liberal or conservative.\n")


################ democrat: 2000, 2004
### Democrat / (Democrat + Republican)
y <- naes00$pidParty
y <- replace (y, naes00$pidParty=="Independent", NA)
y <- replace (y, naes00$pidParty=="Verbatim", NA)
y <- factor (y)

democrat2000 <- 
    generateMap (y, naes00$state, naes00$age, naes00$income, positiveResponse="Democrat",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2000",,,],
            outputFilename="MRP/health maps/maps/democrat2000-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Do you identify yourself as a Democrat (2000 survey)?",
            caption2="Analysis only includes respondents who identified with the Democratic or Republican Party.\n")


### Deomcrat / (Democrat + Republican)
y <- naes04$pidParty
y <- replace (y, naes04$pidParty=="Independent", NA)
y <- replace (y, naes04$pidParty=="Something else", NA)
y <- factor (y)

democrat2004 <-
    generateMap (y, naes04$stateAbbreviation, naes04$age, naes04$income, positiveResponse="Democrat",
            fitModelStateAgeIncome, stateLevel, visualizeThreewayDatasetWithoutPoststratification, propStateAgeIncome["2004",,,],
            outputFilename="MRP/health maps/maps/democrat2004-StateAgeIncome",
            var2Labels=c("Age\n18-29", "30-44", "45-64", "65+"),var3Labels=c("Income under $20,000", "$20-40,000", "$40-75,000", "$75-150,000", "Over $150,000"),
            caption="Do you identify yourself as a Democrat (2004 survey)?",
            caption2="Analysis only includes respondents who identified with the Democratic or Republican Party.\n")


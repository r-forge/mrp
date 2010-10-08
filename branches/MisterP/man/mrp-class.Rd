\name{mrp}
\docType{class}
\alias{mrp}
\alias{mrp-class}

\alias{getData,mrp-method}
\alias{getResponse,mrp-method}
\alias{getNumberWays,mrp-method}
\alias{getThetaHat,mrp-method}

\alias{getPopulation,mrp-method}
\alias{setPopulation,mrp-method}
\alias{setPopOnes,mrp-method}
\alias{getFormula,mrp-method}
\alias{setFormula,mrp-method}
\alias{hasMultilevelModel,mrp-method}


\title{Multilevel regression and poststratification}
\description{
  The \code{mrp} class holds N-dimensional cross-classified arrays for a survey
  and a population, a 2-dimensional summary of survey data used to fit a
  multilevel model, and a fitted model. Methods described here operate
  on \code{mrp} objects which are typically created by calling the
  \code{\link{mrp}} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{ a \code{data.frame} used to run the
      multilevel model.}
    \item{\code{formula}:}{ The formula used in the multilevel
      regression. The left-hand side is always \squote{response}.}
    \item{\code{multilevelModel}:}{ The multilevel regression model 
      (class \code{mer} created by \code{\link[lme4]{glmer}})}
    \item{\code{poll}:}{ An N-way cross-classified array (class
  \code{\link{NWayData}}) constructed from a survey.}
    \item{\code{population}:}{ The population distribution, also of
  class \code{\link{NWayData}}, of dimension matching that of
  \code{poll}. 
  This is intended to be a probability 
      mass table (all entries sum to 1), but the package will 
      also work with unnormalized population distributions. 
      The population is not used for the multilevel regression step. 
      It is used only in poststratification.}
  }
}
\usage{
\S4method{mr}{mrp}(object,formula)
\S4method{poststratify}{mrp}(object,formula)
\S4method{getNumberWays}{mrp}(object)
}
\arguments{
  \item{object}{A \code{mrp} object.}
  \item{formula}{For \code{mr} method, to change the model formula and re-fit the multilevel
    regression, provide a formula specification that will be used to
    \link{update.formula}. The left-hand side should always be
    \sQuote{response}. For convenience use \code{.} to indicate
    current formula contents on either side of the \code{~}, e.g.,
    \code{. ~ . + newVar}.
    
    For \code{poststratify} method, may be a formula with variable names
    corresponding to the \dQuote{ways} by which the data are stratified
    (e.g., \code{~age+sex}), or a character vector of such names, or a
    logical vector of the same length. By default, all \dQuote{ways} are
    collapsed to a single poststratified predicted value.}
}
\section{Methods}{
  \describe{
    \item{mr}{\code{signature(object = "mrp")}: update the formula and
    re-run the multilevel regression in the object.}
    \item{poststratify}{\code{signature(object = "mrp")}: perform
    poststratification using the fitted values of the multilevel model
    slot and the population frequencies contained in the
    population slot. When used with a
    \code{poststratification.specification} (formula, character, or
    logical; see above), gives values in each of the given cells of the
    N-way array.}
    \item{getThetaHat}{\code{signature(object = "mrp")}: model fitted
    values in an N-way array corresponding to the dimension of the
    NWayData contained in slot \code{poll}.}
    \item{setPopulation}{\code{signature(object = "mrp")}: set
    population frequency data used for poststratification. This method
    checks that the dimension and names of the array correspond to those
    of slot \code{poll}.}
    \item{setPopOnes}{\code{signature(object = "mrp")}: To display model
    results without adjusting to a population frequency table, all
    dimensions can be set to 1.}
    \item{getNumberWays}{\code{signature(object = "mrp")}: returns the
    dimension of both \code{poll} and \code{population}. When a
    \dQuote{real} population array is not set (all=1), value for pop
    shows 0 here for.}
  }
}



\author{Andrew Gelman <gelman@stat.columbia.edu>,
  Daniel Lee <bearlee@alum.mit.edu>,
  Yu-Sung Su <ys463@columbia.edu>,
  Michael Malecki <malecki@wustl.edu>
}


\examples{
\dontrun{
    # Get data from BetaDataFerrett.
    # From within BetaDataFerrett (after login) -- Step 1: Select Dataset & Variable
    #   Left Pane:
    #     -> American Community Survey
    #     -> 3-Year Estimates - Public Use Microdata Sample
    #     -> 2006-2008
    #     -> View Variables
    #   Right Pane:
    #     -> Choose:
    #       - Population - Age (AGEP)
    #       - Housing - Household income (HINCP)
    #       - Housing - Vehicles (VEH)
    #       - Housing - Telephone in Unit (TEL)
    #       - Population - Sex (SEX) (1 = Male, 2 = Female)
    #       - Population - Recoded detailed race code (RAC1P) (1 = white, 2 = black, 3 = american india, 4= alaska native..., 6 = asian, ...)
    #       - FIPS State Code.
    #  Step 2: 
    #       Download data as comma delimited
    #       Will take a while.
    #       Saved as "dataferret.csv" in current workspace (getwd())
    # Load Data
    acs2008 <- read.csv ("dataferrett.csv")
    nrow (acs2008) # 8965060
    
    # Preprocess data
    # names (acs2008)
    # "SERIALNO" "AGEP"     "HINCP"    "VEH"      "TEL"      "SEX"      "RAC1P"    "ST"
    
    # Create 5 age categories
    acs2008$age <- cut (x=acs2008$AGEP, breaks=c (0, 18, 30, 45, 65, 99), labels=c("0-17", "18-29", "30-44", "45-64", "65+"))
    table (acs2008$age)
    
    # Create 5 income categories
    acs2008$income <- cut (x=acs2008$HINCP, breaks=c (min (acs2008$HINCP), 50000, 100000, 150000, 200000, max (acs2008$HINCP)), labels=c("< $50K", "$50-100K", "$100-150K", "$150-200K", "> $200K"))
    table (acs2008$income)
    
    # State: recode states
    library (datasets)
    states <- rep (NA, 72)
    states[1] <- "AL"
    states[2] <- "AK"
    states[4] <- "AZ"
    states[5] <- "AR"
    states[6] <- "CA"
    states[8] <- "CO"
    states[9] <- "CT"
    states[10] <- "DE"
    states[11] <- "DC"
    states[12] <- "FL"
    states[13] <- "GA"
    states[15] <- "HI"
    states[16] <- "ID"
    states[17] <- "IL"
    states[18] <- "IN"
    states[19] <- "IA"
    states[20] <- "KS"
    states[21] <- "KY"
    states[22] <- "LA"
    states[23] <- "ME"
    states[24] <- "MD"
    states[25] <- "MA"
    states[26] <- "MI"
    states[27] <- "MN"
    states[28] <- "MS"
    states[29] <- "MO"
    states[30] <- "MT"
    states[31] <- "NE"
    states[32] <- "NV"
    states[33] <- "NH"
    states[34] <- "NJ"
    states[35] <- "NM"
    states[36] <- "NY"
    states[37] <- "NC"
    states[38] <- "ND"
    states[39] <- "OH"
    states[40] <- "OK"
    states[41] <- "OR"
    states[42] <- "PA"
    states[44] <- "RI"
    states[45] <- "SC"
    states[46] <- "SD"
    states[47] <- "TN"
    states[48] <- "TX"
    states[49] <- "UT"
    states[50] <- "VT"
    states[51] <- "VA"
    states[53] <- "WA"
    states[54] <- "WV"
    states[55] <- "WI"
    states[56] <- "WY"
    states[72] <- NA # Puerto Rico
    acs2008$state <- factor (states[acs2008$ST], levels=states)
    
    # Sex
    acs2008$sex <- factor (acs2008$SEX, levels=c(1,2), labels=c("Male", "Female"))
    
    # Outcome vars
    # have access to a vehicle: change to binary variable
    acs2008$vehicle <- factor (acs2008$VEH > 0, levels=c(TRUE, FALSE), labels=c("Yes", "No"))
    # phone in unit
    acs2008$phone <- factor (acs2008$TEL, levels=c(1,2), labels=c("Yes", "No"))
    
    acs2008 <- acs2008 [, c("vehicle", "state", "age", "income", "phone")]
    acsMrpVehicle <- newMrp (acs2008$vehicle, acs2008[, c("state", "age", "income")])
    
    subset <- rbinom (nrow(acs2008), 1, 0.3)==1
    acsMrpVehicle <- newMrp (acs2008$vehicle[subset], acs2008[subset, c("state", "age", "income")])
    acsMrpVehicle <- setFormula (acsMrpVehicle, "cbind(response.yes, response.no) ~ 1 + (1 | state) + (1 | age) + (1 | income) + (1 | state:age) + (1 | state:income) + (1 | age:income)")
    acsMrpVehicle <- mr (acsMrpVehicle)
    
    (vehicle <- p (acsMrpVehicle))
    (vehicle.byState <- p (acsMrpVehicle, c(TRUE, FALSE, FALSE))) 
    (vehicle.byAge <- p (acsMrpVehicle, c(FALSE, TRUE, FALSE)))
    (vehicle.byIncome <- p (acsMrpVehicle, c(FALSE, FALSE, TRUE)))
    
    stateVisualizer <- newMrpStateVisualizer()
    visualize (stateVisualizer, acsMrpVehicle)
    
}
  
## 1) Republican vote
  ## load data
  data(pew)
  ## Run 3-way analysis of pew data
  ## response: rvote - republican vote
  ## variables: stnum - state
  ##            age2 - age
  ##            inc - income
  pewRepublicanMrp <- newMrp (pew$rvote, pew[, c("stnum", "age2", "inc")])
  pewRepublicanMrp <- setFormula (pewRepublicanMrp, "cbind(response.yes, response.no) ~ 1 + (1 | stnum) + (1 | age2) + (1 | inc) + (1 | stnum:age2) + (1 | stnum:inc) + (1 | age2:inc)")
  
  pewRepublicanMrp <- mr (pewRepublicanMrp)
  
  (republicanSupport <- p (pewRepublicanMrp))
  (republicanSupport.byState <- p (pewRepublicanMrp, c(TRUE, FALSE, FALSE))) 
  
  stateVisualizer <- newMrpStateVisualizer()
  visualize (stateVisualizer, pewRepublicanMrp)

\dontrun{
  ## 2) School Vouchers: simple model
  ## load data: 2000
  data (naes00)
  data (proportionOfVoters.schoolVouchers)

  ## Run 3-way analysis of annenberg data
  ## response: favorVoucher - favor school vouchers
  ## variables: stateAbbreviation - state
  ##            income - categorical income variable (4 categories)
  ##            religionAndEthnicity - categorical religion and ethnicity variable (7 categories)
  favorVoucher2000Mrp.simpleModel <- newMrp (naes00$favorVoucher, naes00[, c("stateAbbreviation", "religionAndEthnicity", "income")])
  favorVoucher2000Mrp.simpleModel <- setFormula (favorVoucher2000Mrp.simpleModel, 
      paste ("cbind(response.yes, response.no) ~", 
      "1 + (1 | stateAbbreviation) + (1 | income)",
      "+ (1 | religionAndEthnicity) + (1 | income:religionAndEthnicity)",
      "+ (1 | income:stateAbbreviation) + (1 | religionAndEthnicity:stateAbbreviation)"))
  
  favorVoucher2000Mrp.simpleModel <- mr (favorVoucher2000Mrp.simpleModel)
  favorVoucher2000Mrp.simpleModel <- setPopulation (favorVoucher2000Mrp.simpleModel, proportionOfVoters.schoolVouchers)
  
  (favorVoucher2000 <- p (favorVoucher2000Mrp.simpleModel))
  (favorVoucher2000.byState <- p (favorVoucher2000Mrp.simpleModel, c(TRUE, FALSE, FALSE))) 
  
  #stateVisualizer <- newMrpStateVisualizer()
  visualize (stateVisualizer, favorVoucher2000Mrp.simpleModel)
  
  
  ## 3) School Vouchers: full model
  #data (naes00)
  #data (proportionOfVoters.schoolVouchers)
  favorVoucher2000Mrp.fullModel <- newMrp (naes00$favorVoucher, naes00[, c("stateAbbreviation", "religionAndEthnicity", "income")])
  
  ## hierarchical data:
  data (stateLevel)
  
  ## augment data:
  ##    z.income - standardized household income variable
  ##    region - categorical variable indicating region
  ##    z.stateIncome - standardized state income variable
  ##    z.stateRepublicanVote2004 - support for Republican vote (in 2004)
  favorVoucher2000Mrp.fullModel@data@data[, "z.income"] <- rescale (favorVoucher2000Mrp.fullModel@data@data[, "income"])
  
  stateToRegion <- function () {
    state.abb.long <- factor(rep(NA, 51), levels=c(state.abb, "DC"))
    state.abb.long[1:8] <- state.abb[1:8]
    state.abb.long[9] <- "DC"
    state.abb.long[10:51] <- state.abb[9:50]
  
      state.region.long <- factor(rep(NA, 51), levels=c(levels(state.region), "DC"))
      state.region.long[1:8]   <- state.region[1:8]
      state.region.long[9]     <- "DC"
      state.region.long[10:51] <- state.region[9:50]  
    
      ## modifications to the regions:
      ## DE, MD, and WV in the South
      state.region.long <- replace (state.region.long,
              state.abb.long == "DE" | state.abb.long == "MD" | state.abb.long == "WV",
              "South")
    
      ## rename the levels so they match common convention ("Midwest" instead of "North Central")
      levels (state.region.long) <- c ("Northeast", "South", "Midwest", "West", "DC")
    
      return (state.region.long)
  }  
  favorVoucher2000Mrp.fullModel@data@data[, "region"] <- stateToRegion()[favorVoucher2000Mrp.fullModel@data@data[, "stateAbbreviation"]] 
  favorVoucher2000Mrp.fullModel@data@data[, "z.stateIncome"] <- rescale (stateLevel$income[favorVoucher2000Mrp.fullModel@data@data[, "stateAbbreviation"]])
  favorVoucher2000Mrp.fullModel@data@data[, "z.stateRepublicanVote2004"] <- rescale (stateLevel$republicanVote04[favorVoucher2000Mrp.fullModel@data@data[, "stateAbbreviation"]])
  
  favorVoucher2000Mrp.fullModel <- setFormula (favorVoucher2000Mrp.fullModel, 
      paste ("cbind(response.yes, response.no) ~", 
      "z.income*z.stateIncome + z.income*z.stateRepublicanVote2004",
      "+ (1 + z.income | region)",
      "+ (1 + z.income | stateAbbreviation)",
      "+ (1 | income:region)",
      "+ (1 + z.income | religionAndEthnicity)",
      "+ (1 | income:religionAndEthnicity)",
      "+ (1 | income:stateAbbreviation)",
      "+ (1 | religionAndEthnicity:region)",
      "+ (1 | religionAndEthnicity:stateAbbreviation)",
      "+ (1 | income)"))

  favorVoucher2000Mrp.fullModel <- mr (favorVoucher2000Mrp.fullModel)
  favorVoucher2000Mrp.fullModel <- setPopulation (favorVoucher2000Mrp.fullModel, proportionOfVoters.schoolVouchers)
  ## Note: This will take a lot longer than the simple model.
  
  (favorVoucher2000 <- p (favorVoucher2000Mrp.fullModel))
  (favorVoucher2000.byState <- p (favorVoucher2000Mrp.fullModel, c(TRUE, FALSE, FALSE))) 
  
  #stateVisualizer <- newMrpStateVisualizer()
  visualize (stateVisualizer, favorVoucher2000Mrp.fullModel)


  #4) Health care : 2000
  data (naes00)
  supportHealthcare2000Mrp <- newMrp (naes00$healthFavorSpendingOnUninsured, naes00[, c("stateAbbreviation", "age", "income")])
  
  ## hierarchical data:
  data (stateLevel)

  supportHealthcare2000Mrp@data@data[, "z.income"] <- rescale (supportHealthcare2000Mrp@data@data[, "income"])
  supportHealthcare2000Mrp@data@data[, "z.age"] <- rescale (supportHealthcare2000Mrp@data@data[, "age"])
  
  stateToRegion <- function () {
    state.abb.long <- factor(rep(NA, 51), levels=c(state.abb, "DC"))
    state.abb.long[1:8] <- state.abb[1:8]
    state.abb.long[9] <- "DC"
    state.abb.long[10:51] <- state.abb[9:50]
  
      state.region.long <- factor(rep(NA, 51), levels=c(levels(state.region), "DC"))
      state.region.long[1:8]   <- state.region[1:8]
      state.region.long[9]     <- "DC"
      state.region.long[10:51] <- state.region[9:50]  
    
      ## modifications to the regions:
      ## DE, MD, and WV in the South
      state.region.long <- replace (state.region.long,
              state.abb.long == "DE" | state.abb.long == "MD" | state.abb.long == "WV",
              "South")
    
      ## rename the levels so they match common convention ("Midwest" instead of "North Central")
      levels (state.region.long) <- c ("Northeast", "South", "Midwest", "West", "DC")
    
      return (state.region.long)
  }  
  supportHealthcare2000Mrp@data@data[, "region"] <- stateToRegion()[supportHealthcare2000Mrp@data@data[, "stateAbbreviation"]] 
  supportHealthcare2000Mrp@data@data[, "z.stateIncome"] <- rescale (stateLevel$income[supportHealthcare2000Mrp@data@data[, "stateAbbreviation"]])
  supportHealthcare2000Mrp@data@data[, "z.stateRepublicanVote2004"] <- rescale (stateLevel$republicanVote04[supportHealthcare2000Mrp@data@data[, "stateAbbreviation"]])
  
  supportHealthcare2000Mrp <- setFormula (supportHealthcare2000Mrp, 
      paste ("cbind(response.yes, response.no) ~", 
      "z.age*z.stateIncome + z.age*z.stateRepublicanVote2004",
      "+ (1 + z.age | region)",
      "+ (1 + z.age | stateAbbreviation)",
      "+ (1 | age:region)",
      "+ (1 + z.age | income)",
      "+ (1 | age:income)",
      "+ (1 | age:stateAbbreviation)",
      "+ (1 | income:region)",
      "+ (1 | income:stateAbbreviation)",
      "+ (1 | age)"))

  supportHealthcare2000Mrp <- mr (supportHealthcare2000Mrp)
  
  data (propStateAgeIncome)
  missingStates <- which (dimnames (propStateAgeIncome)[[2]] \%in\% c("AK", "HI"))
  supportHealthcare2000Mrp <- setPopulation (supportHealthcare2000Mrp, propStateAgeIncome["2000", -missingStates, , ])

  (supportHealthcare2000 <- p (supportHealthcare2000Mrp))
  (supportHealthcare2000.byState <- p (supportHealthcare2000Mrp, c(TRUE, FALSE, FALSE))) 
  
  #stateVisualizer <- newMrpStateVisualizer()
  visualize (stateVisualizer, supportHealthcare2000Mrp)


  
  #5) Health care : 2004
  data (naes04)
  supportHealthcare2004Mrp <- newMrp (naes04$healthFavorSpending, naes04[, c("stateAbbreviation", "age", "income")])
  
  ## hierarchical data:
  data (stateLevel)

  supportHealthcare2004Mrp@data@data[, "z.income"] <- rescale (supportHealthcare2004Mrp@data@data[, "income"])
  supportHealthcare2004Mrp@data@data[, "z.age"] <- rescale (supportHealthcare2004Mrp@data@data[, "age"])
  
  stateToRegion <- function () {
    state.abb.long <- factor(rep(NA, 51), levels=c(state.abb, "DC"))
    state.abb.long[1:8] <- state.abb[1:8]
    state.abb.long[9] <- "DC"
    state.abb.long[10:51] <- state.abb[9:50]
  
      state.region.long <- factor(rep(NA, 51), levels=c(levels(state.region), "DC"))
      state.region.long[1:8]   <- state.region[1:8]
      state.region.long[9]     <- "DC"
      state.region.long[10:51] <- state.region[9:50]  
    
      ## modifications to the regions:
      ## DE, MD, and WV in the South
      state.region.long <- replace (state.region.long,
              state.abb.long == "DE" | state.abb.long == "MD" | state.abb.long == "WV",
              "South")
    
      ## rename the levels so they match common convention ("Midwest" instead of "North Central")
      levels (state.region.long) <- c ("Northeast", "South", "Midwest", "West", "DC")
    
      return (state.region.long)
  }  
  supportHealthcare2004Mrp@data@data[, "region"] <- stateToRegion()[supportHealthcare2004Mrp@data@data[, "stateAbbreviation"]] 
  supportHealthcare2004Mrp@data@data[, "z.stateIncome"] <- rescale (stateLevel$income[supportHealthcare2004Mrp@data@data[, "stateAbbreviation"]])
  supportHealthcare2004Mrp@data@data[, "z.stateRepublicanVote2004"] <- rescale (stateLevel$republicanVote04[supportHealthcare2004Mrp@data@data[, "stateAbbreviation"]])
  
  supportHealthcare2004Mrp <- setFormula (supportHealthcare2004Mrp, 
      paste ("cbind(response.yes, response.no) ~", 
      "z.age*z.stateIncome + z.age*z.stateRepublicanVote2004",
      "+ (1 + z.age | region)",
      "+ (1 + z.age | stateAbbreviation)",
      "+ (1 | age:region)",
      "+ (1 + z.age | income)",
      "+ (1 | age:income)",
      "+ (1 | age:stateAbbreviation)",
      "+ (1 | income:region)",
      "+ (1 | income:stateAbbreviation)",
      "+ (1 | age)"))

  supportHealthcare2004Mrp <- mr (supportHealthcare2004Mrp)
  
  data (propStateAgeIncome)
  missingStates <- which (dimnames (propStateAgeIncome)[[2]] \%in\% c("AK", "HI"))
  supportHealthcare2004Mrp <- setPopulation (supportHealthcare2004Mrp, propStateAgeIncome["2004", -missingStates, , ])

  (supportHealthcare2004 <- p (supportHealthcare2004Mrp))
  (supportHealthcare2004.byState <- p (supportHealthcare2004Mrp, c(TRUE, FALSE, FALSE))) 
  
  #stateVisualizer <- newMrpStateVisualizer()
  visualize (stateVisualizer, supportHealthcare2004Mrp)
}
}
\keyword{classes}

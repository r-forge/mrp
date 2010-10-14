\name{mrp}
\alias{mrp}
\alias{mr,mrp-method}
\alias{poststratify,mrp-method}
\alias{mr}
\alias{poststratify}

\title{Multilevel regression and poststratification}
\description{
  Set up survey and population data, and a multilevel regression model
  used for poststratifying by an arbitrary number of strata or \dQuote{ways}.
}

\usage{mrp (formula,
            poll, poll.weights=1,
            pop=NULL, use=NULL,
            add=NULL, mr.formula=NULL,
            \dots)
\S4method{mr}{mrp}(object, mr.formula, \dots)
\S4method{poststratify}{mrp}(object,formula)
}
\arguments{
    \item{object}{A \code{mrp} object.}
    \item{formula}{A formula representation of the binary outcome variable
      and the desired eventual
      poststratification; i.e., the \dQuote{ways} by which to
      break down the poll and population data, given as factor
      variables with matching names and levels in both \code{poll}
      and \code{population}. By default, this formula will also be
      used to construct the multilevel model, with an intercept
      estimated for each named stratum. See \code{mr.formula}
      below to easily fit more complex models.

      In \code{poststratify}, the formula is null on the left-hand
      side and right-hand-side variable names are dimensions of
      the population data by which to poststratify. See the
      example below. For \code{poststratify} method, may be a formula with variable names
      corresponding to the \dQuote{ways} by which the data are stratified
      (e.g., \code{~age+sex}), or a character vector of such names, or a
      logical vector of the same length. By default, all \dQuote{ways} are
      collapsed to a single poststratified predicted value.}
    \item{poll}{A \code{data.frame} representing a survey, containing (at
      least) the named variables in \code{formula}. The LHS response
      is expected to be dichotomous, and will be coerced to
      binary-logical (if factor, 1 for \sQuote{yes}, 0 for
      \sQuote{no}).}
    \item{poll.weights}{Supplied survey weights for respondents, used to
      compute the effective \eqn{N}, weighted \eqn{\bar{Y}}, and Design Effect.}
    \item{pop}{A \code{data.frame} containing population (e.g., census)
      data with variable names and factor levels matching those in
      \code{poll} and specified in \code{formula}.}
    \item{use}{The column of the \code{pop} data to use for
      poststratification.}
    \item{add}{A \code{list} of additional variables to add to
      the prepared \code{data.frame} used in the multilevel
      regression step. The list may contain items of two types of
      objects: an \bold{expression} to be evaluated \sQuote{within} the
      prepared data, such as a transformation of another variable; or a
      \bold{data.frame} to be left-joined onto the data via
      \code{merge}. An example of the latter would be a data.frame with
      a column containing the \sQuote{region} for every \sQuote{state}
      in the data.}
    \item{mr.formula}{A formula specification for the multilevel model
      to run in the prepared data. The left-hand side should always be
    \sQuote{response}. For convenience, the formula is handled by
      \code{update.formula} so that \code{.} indicates the 
    current formula contents on either side of the \code{~}, e.g.,
    \code{.~.+newVar}. The initial default formula is constructed as
      just an intercept term for each of the variables in the main
      formula specification (\code{(1|way1)+(1|way2)} etc.)}
    \item{\dots}{Additional arguments to be passed to the multilevel
      regression \code{\link[lme4]{glmer}} step.}
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
    \item{getFormula}{\code{signature(object = "mrp")}: returns the
      formula used to fit the multilevel regression.}
    \item{setFormula}{\code{signature(object = "mrp")}: set the formula; but
      you really should use the \code{mr} method instead.}
    \item{getPopulation}{\code{signature(object = "mrp")}: returns the
      \code{NWayData} population array.}
    \item{getData}{\code{signature(object = "mrp")}: return the
      \code{data.frame} used in the multilevel regression step.}
    \item{getResponse}{\code{signature(object = "mrp")}: returns a
      matrix containing the two-column \dQuote{yes/no} response used to
      fit the multilevel regression.}
  }
}
\seealso{\code{\link{mrp-class}} for other
    methods on the objects produced by \code{mrp()}.}


\author{Andrew Gelman <gelman@stat.columbia.edu>,
  Daniel Lee <bearlee@alum.mit.edu>,
  Yu-Sung Su <ys463@columbia.edu>,
  Michael Malecki <malecki@wustl.edu>
}


\examples{
\dontrun{
# library("foreign")

## #read in megapoll and attach
# marriage.data <- read.dta("http://www.princeton.edu/~jkastell/MRP_primer/gay_marriage_megapoll.dta", convert.underscore = TRUE) 

## #read in state-level dataset
## Statelevel <-
## read.dta("http://www.princeton.edu/~jkastell/MRP_primer/state_level_update.dta",
## convert.underscore = TRUE)

## #read in Census data
## Census <- read.dta("http://www.princeton.edu/~jkastell/MRP_primer/poststratification%202000.dta",convert.underscore = TRUE)
## Census <- Census[order(Census$cstate),]

## #Create index variables

##   #At level of megapoll
##   marriage.data <- within(marriage.data, {
##     state <- factor(state,exclude=NA)
##     poll <- factor(poll,exclude=NA)
##     age <- factor(age.cat,exclude=NA,labels=c("young","middleage",
##                                        "older","old"))
##     edu <- factor(edu.cat,exclude=NA,labels=c"< High School",
##                   "High School",
##                   "Some College",
##                   "Graduated College"))
##                         })
## marriage.data <- subset(marriage.data,!is.na(state) & state!="" )
## marriage.data$state <- factor(marriage.data$state)

## Statelevel <- within(Statelevel, {
##   state <- sstate
##   state.initnum <- sstate.initnum
## p.evang.full<- p.evang[state.initnum]# proportion of evangelicals in respondent's state
## p.mormon.full <- p.mormon[state.initnum]# proportion of mormon's in respondent's state
## p.relig.full <- p.evang.full + p.mormon.full# combined evangelical + mormom proportions
## p.kerry.full <- kerry.04[state.initnum]# kerry's % of 2-party vote in respondent's state in 2004
## })

library(MisterP,lib="~/R")

load("../data/us_noaa_map.Rdata")
load("../data/kastellec_ex.Rdata")

  marriage.data <- within(marriage.data, {
    state <- factor(state,exclude=NA)
    poll <- factor(poll,exclude=NA)
    age <- factor(age.cat,exclude=NA,labels=c("young","middleage",
                                       "older","old"))
    edu <- factor(edu.cat,exclude=NA,labels=c("< High School",
                  "High School",
                  "Some College",
                  "Graduated College"))
                        })
marriage.data <- subset(marriage.data,!is.na(state) & state!="" )
marriage.data$state <- factor(marriage.data$state)



##   #At census level (same coding as above for all variables)
census <- na.omit(census[census$state %in% marriage.data$state,
                         c("age","state","education",
                           "weighted2000","weighted2004","weighted2008")])

Census <- within(census,{
   age <- factor(age,exclude=NA,labels=c("young","middleage",
                                  "older","old"))
   education[education=="postgraduate"] <- "college graduate"
   edu <- factor(education,exclude=NA,labels=c("< High School",
                                        "High School",
                                        "Some College",
                                        "Graduated College"))
   state <- factor(state,exclude=NA)
})

regions <- data.frame(state=levels(marriage.data$state),region=NA)
  regions[regions$state %in% c("CT","ME","MA",
                               "NH","RI","VT",
                               "NJ","NY","PA"),"region"] <- "Northeast"
regions[regions$state %in% c("IN","IL","MI",
                             "OH","WI","IA",
                             "KS","MO","MN",
                             "NE","ND","SD"),"region"] <- "Midwest"
regions[regions$state %in% c("DE","DC","FL",
                             "GA","MD","NC",
                             "SC","VA","WV",
                             "AL","KY","MS",
                             "TN","AR","LA",
                             "OK","TX"),"region"] <- "South"
regions[regions$state %in% c("AZ","CO","ID",
                             "NM","MT","UT",
                             "NV","WY","AK",
                             "CA","HI","OR","WA"),"region"] <- "West"
regions$region <- factor(regions$region)




mrp.simple <- mrp(yes.of.all ~ state +age+edu+poll,
    poll=marriage.data)

quartzFonts(gotham=quartzFont(c(
              "Gotham-Book","Gotham-Bold",
              "Gotham-BookItalic","Gotham-BoldItalic")))

spplot (mrp.simple, state ~ poll + age ,
        spmap.states, "STATE", exclude=c("AK","DC","HI"),
        add.settings=list(
          add.text=list(cex=.7,fontface="italic",
            fontfamily="gotham")))


mrp.statelevel <- mrp(yes.of.all~state+age+edu,
                      poll=marriage.data,
                      pop=Census,use="weighted2004",
                      add=list(Statelevel,regions,
                        expression(z.age <- rescale(age))),
                      mr=.~.+z.age+p.relig.full+p.kerry.full+(1|region)
                      )
mrp.statelevel <- mr(mrp.statelevel, .~.+z.age+p.relig.full+p.kerry.full+(1|region)+(1|poll))
spplot(mrp.statelevel, state~age+edu, spmap.states, "STATE", exclude=c("AK","DC","HI"),add.settings=list())

## #run individual-level opinion model

## individual.model <- glmer(formula = yes.of.all ~ (1|race.female) + (1|age.cat) 
##   + (1|edu.cat) + (1|age.edu.cat) + (1|state) + (1|region) + (1|poll) + p.relig.full 
##         + p.kerry.full, family=binomial(link="logit"))
## display(individual.model)

## #examine random effects and standard errors for race-female
## ranef(individual.model)$race.female
## se.ranef(individual.model)$race.female

## #create vector of state ranefs and then fill in missing ones
## state.ranefs <- array(NA,c(51,1))
## dimnames(state.ranefs) <- list(c(sstate),"effect")
## for(i in sstate){
##     state.ranefs[i,1] <- ranef(individual.model)$state[i,1]
## }
## state.ranefs[,1][is.na(state.ranefs[,1])] <- 0


## #create a prediction for each cell in Census data
## cellpred <- invlogit(fixef(individual.model)["(Intercept)"]
##             +ranef(individual.model)$race.female[crace.female,1]
##             +ranef(individual.model)$age.cat[cage.cat,1]
##             +ranef(individual.model)$edu.cat[cedu.cat,1]
##             +ranef(individual.model)$age.edu.cat[cage.edu.cat,1]
##             +ranef(individual.model)$age.edu.cat[cage.edu.cat,1]
##             +state.ranefs[cstate,1]
##             +ranef(individual.model)$region[cregion,1]   
##             +(fixef(individual.model)["p.relig.full"] *cp.relig.full)
##             +(fixef(individual.model)["p.kerry.full"] *cp.kerry.full)
##                )

## #weights the prediction by the freq of cell                                       
## cellpredweighted <- cellpred * cpercent.state

## #calculates the percent within each state (weighted average of responses)
## statepred <- 100* as.vector(tapply(cellpredweighted,cstate,sum))
## statepred
}
}

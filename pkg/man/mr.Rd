\name{mr}
\alias{mr}
\alias{mr,mrp-method}

\title{Run Multilevel Regression step of MRP Analysis}
\description{
  Run a (binomial) multilevel regression in survey data for later
  poststratification.
}

\usage{
\S4method{mr}{mrp}(object, mr.formula, \dots)
}
\arguments{
    \item{object}{A \code{mrp} object.}
  
    \item{mr.formula}{A formula specification for the multilevel model
      to run in the prepared data. The left-hand side should always be
    \sQuote{response}. For convenience, the formula is handled by
      \code{update.formula} so that \code{.} indicates the 
    current formula contents on either side of the \code{~}, e.g.,
    \code{.~.+newVar}. The initial default formula is constructed as
      just an intercept term for each of the variables in the main
      formula specification (\code{(1|way1)+(1|way2)} etc.)}
    \item{\dots}{Additional arguments to be passed to the multilevel
      regression step, which uses \code{\link[lme4]{glmer}} by default. }
  }
  %%%%%%%% THIS IS DUPLICATED IN mrp-class : PICK ONE.
  %%%%%%%%
% \section{Methods}{
%   \describe{
%     \item{mr}{\code{signature(object = "mrp")}: update the formula and
%       re-run the multilevel regression in the object.}
%     \item{poststratify}{\code{signature(object = "mrp")}: perform
%       poststratification using the fitted values of the multilevel model
%       slot and the population frequencies contained in the
%       population slot. When used with a
%       \code{poststratification.specification} (formula, character, or
%       logical; see above), gives values in each of the given cells of the
%       N-way array.}
%     \item{getThetaHat}{\code{signature(object = "mrp")}: model fitted
%       values in an N-way array corresponding to the dimension of the
%       NWayData contained in slot \code{poll}.}
%     \item{getEstimates}{\code{signature(object = "mrp")}: an alias for \code{getThetaHat()}}
%     \item{setPopulation}{\code{signature(object = "mrp")}: set
%       population frequency data used for poststratification. This method
%       checks that the dimension and names of the array correspond to those
%       of slot \code{poll}.}
%     \item{setPopOnes}{\code{signature(object = "mrp")}: To display model
%       results without adjusting to a population frequency table, all
%       dimensions can be set to 1.}
%     \item{getNumberWays}{\code{signature(object = "mrp")}: returns the
%       dimension of both \code{poll} and \code{population}. When a
%       \dQuote{real} population array is not set (all=1), value for pop
%       shows 0 here.}
%     \item{getFormula}{\code{signature(object = "mrp")}: returns the
%       formula used to fit the multilevel regression.}
%     \item{setFormula}{\code{signature(object = "mrp")}: set the formula; but
%       you really should use the \code{mr} method instead.}
%     \item{getPopulation}{\code{signature(object = "mrp")}: returns the
%       \code{NWayData} population array.}
%     \item{getData}{\code{signature(object = "mrp")}: return the
%       \code{data.frame} used in the multilevel regression step.}
%     \item{getResponse}{\code{signature(object = "mrp")}: returns a
%       matrix containing the two-column \dQuote{yes/no} response used to
%       fit the multilevel regression.}
%   }
% }
\seealso{\code{\link{mrp-class}} for other
    methods on the objects produced by \code{mrp()};
      \code{\link{plotmrp}} for how to plot poststratified results  onto maps.}


\author{Andrew Gelman <gelman@stat.columbia.edu>,
  Michael Malecki <malecki@wustl.edu>,
  Daniel Lee <bearlee@alum.mit.edu>,
  Yu-Sung Su <ys463@columbia.edu>
}


\examples{
  library(MRP)
  ## Load example data and helper datasets.
  data(samesexmarriage)
  ## Helper datasets for other US applications of MRP:
  data(spmap.states) # projected US state map
  data(mrp.census)   # census with common demo strata
  data(mrp.regions)  # regions data.frame with DC separate

  ## To ensure matching of strata between poll and population,
  ## both should be factors with identical names and levels.
  marriage.data <- within(marriage.data, {
    state <- factor(state,exclude=c(NA, ""))
    poll <- factor(poll,exclude=NA)
    age <- factor(age.cat,exclude=NA,
                  labels=c("18-29","30-44","45-64","65+"))
    edu <- factor(edu.cat,exclude=NA,labels=c("< High School",
                                       "High School",
                                       "Some College",
                                       "Graduated College"))
    ## Code interaction here, first fixing levels
    female <- factor(female,levels=c(0,1),
                     labels=c("Male","Female"))
    race.wbh <- factor(race.wbh, levels=c(1,3,2),
                   labels=c("White","Black","Hispanic"))
    f.race <- interaction(female,race.wbh)
  })
  
  ## Poll has four levels of education, so we need to combine
  ## the top two levels in the census data. We'll also go ahead
  ## and trim it down to just the variables used here.
  mrp.census <- na.omit(mrp.census[mrp.census$state \%in\% marriage.data$state,])
  mrp.census <- within(mrp.census,{
    age <- factor(age,exclude=NA,labels=c("18-29","30-44","45-64","65+"))
    education[education=="postgraduate"] <- "college graduate"
    edu <- factor(education,exclude=NA,labels=c("< High School",
                                         "High School",
                                         "Some College",
                                         "Graduated College"))
    state <- factor(state,exclude=NA)
    race[race=="Other"] <- NA
    race <- factor(race,exclude=NA)
    f.race <- interaction(sex,race)
  })
  mrp.census <- na.omit(mrp.census)
    

  ## Ready to run simple mrp with poll and population:
  
  mrp.simple <- mrp(yes.of.all ~ state+age+edu+poll,
                    poll=marriage.data,poll.weights="weight"
                    #,pop=mrp.census,use="weighted2004",
                    #population.formula= .~.-poll
                    )

  print(100*poststratify(mrp.simple, ~ edu+age), digits=2)

\dontrun{
  ## Fit a fuller model, adding state-level predictors:
  ## This model is also used in the not-run example
  ## for plotting maps.
  
  mrp.statelevel <- mrp(yes.of.all~
                        state+f.race+age+edu+poll,

                        poll=marriage.data,
                        population=mrp.census,use="weighted2008",
                        population.formula=.~.-poll,
                        add=list(Statelevel,
                          mrp.regions,
                          expression(age.edu <- interaction(age,edu)),
                          expression(z.age <- rescale(age)))
                        )
  
  ## Update the mr.formula on already-prepared mrp object and re-fit:
  mrp.statelevel <- mr(mrp.statelevel, .~.+(1|region)+ (1|age.edu)+
                       z.age+p.relig.full+p.kerry.full)

  ## Fine plot control is shown with this example in plotmrp documentation!
}
}

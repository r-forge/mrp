createTestPoll <- function () {
    data (samesexmarriage)
    
    poll <- within (marriage.data, {
                state <- factor (state,exclude=c(NA, ""))
                age <- factor (age.cat, exclude=NA, labels=c("18-29","30-44", "45-64","65+"))
                edu <- factor (edu.cat,exclude=NA,labels=c("< High School","High School","Some College", "Graduated College"))
                weight <- ifelse(is.na(weight), 1, weight)
            })
    poll <- na.omit (poll[, c("state", "age", "edu", "weight", "yes.of.all")])
        
    return (poll)
}

createTestPopulation <- function (poll) {
    data (mrp.census)
    
    population <- na.omit(mrp.census[mrp.census$state %in% poll$state,c("weighted2004","state", "age", "education")])
    population <- within(population, {
                    age <- factor(age,exclude=NA,labels=c("18-29","30-44","45-64","65+"))
                    education[education=="postgraduate"] <- "college graduate"
                    edu <- factor(education,exclude=NA,labels=c("< High School",
                                "High School",
                                "Some College",
                                "Graduated College"))
                    state <- factor(state,exclude=NA)
              })
    population <- na.omit(population)    
    return (population)
}


test.create.mrp <- function () {
    obj <- mrp (formula=yes.of.all ~ state+age+edu,
                poll=createTestPoll(),
                poll.weights="weight",
                population=createTestPopulation(createTestPoll()),
                use="weighted2004")

    
}

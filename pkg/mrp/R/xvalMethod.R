setGeneric("xval", function(object, formula, folds, loss.type) {standardGeneric("xval")})

setMethod(f="xval",
          signature=signature(object="mrp"),
          definition=function(object, formula, folds, loss.type){
            ## create a list of length folds that holds different partitions
            K <- folds
            M <- object
            if(missing(formula)) {
              fm <- M@formula
              }
            else {
              fm <- update.formula(M@formula, formula)
            }

            if(missing(loss.type)) loss.type <- "log"
            require(doMC, quietly=T)
            registerDoMC()
            response <- M@data[,c("response.yes", "response.no")];
            response <- ceiling(response) # annoying floating point rounding errors
            partition <- array(0, dim=c(2, 2, nrow(response), K))
            for(i in 1:nrow(response)) {
              part <- sample(1:K, sum(response[i, ]), replace=T);
              full <- rep(c(1,0), response[i,])
              for(j in 1:K){
                partition[,1,i,j] <- c(sum(full[part!=j]==1), sum(full[part!=j]==0));
                partition[,2,i,j] <- c(sum(full[part==j]==1), sum(full[part==j]==0));
              }
            }
            
            listofpartition <- lapply(1:K, function(k){
              newdata <- M@data
              newdata$response.yes <- partition[1,1,,k];
              newdata$response.no <- partition[2,1,,k];
              testdata <- data.frame(t(partition[,2,,k]));
              colnames(testdata) <- c("response.yes", "response.no")
              list(training=newdata, testing=testdata)
            }) 
            
            ## nopool.loss <- foreach(k = 1:K, .verbose=FALSE) %dopar% {
            ##   foo <- glm(cbind(response.yes,response.no) ~ stt*inc,
            ##              data=listofpartition[[k]]$training,family=quasibinomial(link="logit")
            ##              )
            ##   yhat <- try(subset(fitted(foo), rowSums(listofpartition[[k]]$testing)>0),
            ##               silent=TRUE)
              
            ##   S <- listofpartition[[k]]$testing
            ##   S <- subset(S, rowSums(S)>0)
            ##   loss <- subset(listofpartition[[k]]$training, rowSums(listofpartition[[k]]$testing)>0)
            ##                             #    loss <- na.omit(loss)
            ##   loss$pred <- yhat
            ##   loss$logloss <- -(S$response.yes*log(yhat) + S$response.no*log(1-yhat))
            ##   loss$n <- rowSums(S)
            ##   return(loss)
            ## }
  ##            browser()
            if(loss.type=="log") {
              loss <- foreach(k = 1:K, .verbose=FALSE) %dopar% {
                response <- as.matrix((listofpartition[[k]]$training)[, c("response.yes", "response.no")])
                attr(fm, ".Environment") <- environment() ## crucial!! Environment of formula!!
                foo <- blmer(fm, data=listofpartition[[k]]$training, family=quasibinomial)
                yhat <- try(subset(fitted(foo), rowSums(listofpartition[[k]]$testing)>0),
                            silent=TRUE)
                S <- listofpartition[[k]]$testing
                S <- subset(S, rowSums(S)>0)
                                        #    loss <- na.omit(loss)
                pred <- yhat
                logloss <- -(S$response.yes*log(yhat) + S$response.no*log(1-yhat))
                n <- rowSums(S)
                loss <- data.frame(pred, logloss, n)
                return(loss)
              }
            }
            ##            browser()
            ## comppool.loss <- foreach(k = 1:K, .verbose=FALSE) %dopar% {
            ##   foo <- glm(cbind(response.yes,response.no) ~ stt+inc,
            ##              data=listofpartition[[k]]$training,family=quasibinomial(link="logit")
            ##              )
            ##   yhat <- try(subset(fitted(foo), rowSums(listofpartition[[k]]$testing)>0),
            ##               silent=TRUE)
            ##   S <- listofpartition[[k]]$testing
            ##   S <- subset(S, rowSums(S)>0)
            ##   loss <- subset(listofpartition[[k]]$training, rowSums(listofpartition[[k]]$testing)>0)
            ##                             #    loss <- na.omit(loss)
            ##   loss$pred <- yhat
            ##   loss$logloss <- -(S$response.yes*log(yhat) + S$response.no*log(1-yhat))
            ##   loss$n <- rowSums(S)
            ##   return(loss)
            ## }

            mat <- 
              sapply(loss, function(l){ # fold
                sum(l$logloss)
              })


            ## find Error Lower Bound
            aa <- sum(mat)
            ## resp <- ddply(M@data, .(inc, stt), function(x) apply(cbind(x$response.yes, x$response.no), 2, sum))
            ## resp1 <- ddply(resp, .(inc, stt), function(x) c(x$V1/(x$V1+x$V2), (x$V1+x$V2)))
            ## aa <- c(aa, with(resp1, {-sum(((V1*log(V1)+(1-V1)*log(1-V1)))*V2)}))
            ## names(aa) <- c("loss", "lb")
            
            ## stt.name <- c("CA",  "DE", "TN", "CT", "VA", "AK", "CO", "RI", "UT", "MN")
            ## loglossbycell <- function(x) {apply(sapply(x, function(y)resp1$V1*(log(resp1$V1)-log(y$pred))+(1-resp1$V1)*(log(1-resp1$V1)-log(1-y$pred))), 1, mean)}
            ## comploglossbycell <- loglossbycell(comppool.loss)
            ## nologlossbycell <- loglossbycell(nopool.loss)
            ## parloglossbycell <- loglossbycell(parpool.loss)
            ## data1 <- data.frame(comp=comploglossbycell, no=nologlossbycell, partial=parloglossbycell)
            ## data1$state <- rep(stt.name, 5)
            ## data1$inc <- rep(1:5, each=10)
            
            ## data12 <- melt(data1, id=c("state", "inc"))
            ## library(ggplot2)
            ## p <- ggplot(data12, aes(x=variable, y=value)) + facet_grid(inc~state) + geom_point(aes(color=variable)) + scale_y_log10()

            return(aa)
            ## fit the model and calculate the 
          }
          
          )


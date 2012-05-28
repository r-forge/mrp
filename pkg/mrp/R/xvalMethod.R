setGeneric("xval", function(object, formula, folds, loss.type, ...) {standardGeneric("xval")})

setMethod(f="xval",
          signature=signature(object="mrp"),
          definition=function(object, formula, folds, loss.type, ...){
            ## create a list of length folds that holds different partitions
            K <- folds
            M <- object
            if(missing(formula)) {
              fm <- M@formula
              }
            else {
              fm <- update.formula(M@formula, formula)
            }
           
            fm.terms <- terms(fm)
            cl.labels <- attr(fm.terms, "term.labels")
            cl.labels <- gsub("1 \\| ", "", cl.labels)
            cl.fm <- paste("response~", cl.labels[1], sep="")
            for(i in 2:length(cl.labels))
              cl.fm <- paste(cl.fm, "+", cl.labels[i], sep="")
            cl.fm <- as.formula(cl.fm)

            if(missing(loss.type)) loss.type <- "log"
            ##            require(doMC, quietly=T)
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
 
            if(loss.type=="log") {
              loss <- foreach(k = 1:K, .verbose=FALSE) %dopar% {
                response <- as.matrix((listofpartition[[k]]$training)[, c("response.yes", "response.no")])
                attr(fm, ".Environment") <- environment() ## crucial!! Environment of formula!!
                foo <- blmer(fm, data=listofpartition[[k]]$training, family=quasibinomial, ...)               
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
              cl.loss <-  foreach(k = 1:K, .verbose=FALSE) %dopar% {
                response <- as.matrix((listofpartition[[k]]$training)[, c("response.yes", "response.no")])
                attr(cl.fm, ".Environment") <- environment()
                foo <- glm(cl.fm, data=listofpartition[[k]]$training, family=quasibinomial)               
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

            mat <- 
              sapply(loss, function(l){ # fold
                sum(l$logloss)
              })
            aa <- sum(mat)

            cl.mat <- 
              sapply(cl.loss, function(l){ # fold
                sum(l$logloss)
              })            
            cl.aa <- sum(cl.mat)

            response <- as.matrix(M@data[, c("response.yes", "response.no")])
            attr(cl.fm, ".Environment") <- environment()
            insampleM <- glm(cl.fm, data=M@data,  family=binomial)
            yhat <- fitted(insampleM)
            LB <- -sum(M@data$response.yes*log(yhat) + M@data$response.no*log(1-yhat))
            return(list(MulRes=aa, MulFormula=fm, ClaRes=cl.aa,  ClaFormula=cl.fm, LB=LB))
          }
          
          )

## lb <- function(object){
##   M <- object@data
##   resp <- ddply(M, .(state, income), function(x) {
##     if(is.null(dim(x)))
##       return(c(0,0))
##     else
##       apply(cbind(x$response.yes, x$response.no), 2, sum)
##   }
##                 )
##   resp1 <- ddply(resp, .(state, income), function(x) c(x$V1/(x$V1+x$V2+1), (x$V1+x$V2)))
##   with(resp1, {-sum(((V1*log(V1+.0001)+(1-V1)*log(1-V1+0.0001)))*V2)})  
## }

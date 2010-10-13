setMethod("spplot", signature("mrp"),
          definition=function(obj, formula, spmap=NULL, FID="state", exclude=NULL, ...) {
            obj.p <- melt(poststratify(obj, all.vars(formula)))
            obj.p <- MisterP:::restoreNWayLevels(obj.p, obj@poll)
            plot.terms <- terms(formula,keep.order=TRUE)
            
            plotdf <- dcast(obj.p, formula)
            names(plotdf) <- make.names(names(plotdf))
            
            plotdf[,1] <- as.character(levels(plotdf[,1])[plotdf[,1]])
            spmap@data[,FID] <- as.character(levels(spmap@data[,FID])[spmap@data[,FID]])
            
            
            ## remove excludes list from fitted model, and then pare the sp obj to match.
            if(!is.null(exclude)) {
              plotdf <- subset(plotdf, 
                               !(plotdf[,1] %in% exclude))
            }
            spmap <- spmap[spmap@data[,FID] %in% plotdf[,1],]
            ## set feature ids and rownames to match 
            
            spmap <- spChFIDs(spmap, 
                              spmap@data[,FID])
            rownames(spmap@data) <- spmap@data[,FID]
            rownames(plotdf) <- plotdf[,1]
            
            startcol <- ncol(spmap@data)+2
            spmap@data <- cbind(spmap@data,
                                plotdf[rownames(spmap@data),])
            endcol <- ncol(spmap@data)
            
            dimlabels <- lapply(obj@poll@levels[attr(plot.terms,"term.labels")],function(x) { return(x$levels) })
            if(length(dimlabels)==1) {
              dimlabels[[2]] <- 1 }
            theplot <- spplot(spmap,
                              startcol:endcol,
                              layout=c(length(dimlabels[[2]]), length(dimlabels[[1]])),
                              panel=panel.polygonsplot,
                              strip=strip.custom(
                                factor.levels=rep(dimlabels[[2]],
                                  length(dimlabels[[1]]))),
                              strip.left=strip.custom(horizontal=FALSE,
                                factor.levels=rep(dimlabels[[1]],
                                  each=length(dimlabels[[2]]))),
                              between=list(x=.25,y=.25),
                              ## Need to do a 'theme' and roll together user changes
                              ## with our defaults.
                              par.settings=list(
                                par.main.text=list(fontfamily="gotham"),
                                add.line=list(col="#00000000",lwd=0), # state borders
                                add.text=list(cex=.7,fontface="italic",
                                  fontfamily="gotham"),
                                axis.line=list(lwd=0),
                                ## Here we are going to do some
                                ## strip and strip.left magic.
                                layout.heights=list(strip = 
                                  c(rep(0, length(dimlabels[[1]])-1), 
                                    1)), # should by dynamic for linebreaks
                                layout.widths=list(strip.left=
                                  c(1, rep(0,length(dimlabels[[2]])-1))),
                                strip.border=list(col="transparent"),
                                regions=list(col=blue2green2red(100))
                                ),...
                              )

            
            
            return(theplot)
          } )

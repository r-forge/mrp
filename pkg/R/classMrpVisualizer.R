setClass(Class="MrpVisualizer",
        representation=representation(
                filename="character",
                height="numeric",
                width="numeric",
                shrinkVertical="numeric",
                parameters="list",
                plot.var1="function",
                graphics.open="function",
                graphics.close="function"),
        prototype=prototype(
                height=1,
                width=1,
                shrinkVertical=0.9,
                parameters=list(mar=c(1,1,1,1), oma=c(0,0,0.3,0)),
                plot.var1=function (data, average, proportions) { return (plot (data))},
                ## TODO: Make this a true class: gives it structure
                graphics.open=c,
                graphics.close=c)        
)

setGeneric ("visualize", function (object, ...) { standardGeneric ("visualize")})
setMethod (f="visualize",
        signature="MrpVisualizer",
        definition=function (object, mrp) {
            object@graphics.open (object@filename, object@height, object@width)
            
            par (object@parameters)
            layoutParameters <- prepareLayout (object, mrp)
            layout(mat=layoutParameters$mat, widths=layoutParameters$widths, heights=layoutParameters$heights, respect=FALSE)
            
            placeLabels (object, mrp)
            placeCharts (object, mrp)
            placeMisc (object, mrp)            
            
            object@graphics.close()
            
            return (TRUE)
        })

setGeneric ("placeMisc", function (object, ...) { standardGeneric ("placeMisc")})
setMethod (f="placeMisc",
        signature="MrpVisualizer",
        definition=function (object, mrp) {
            
        })


setGeneric ("placeCharts", function (object, mrp) { standardGeneric ("placeCharts")})
setMethod (f="placeCharts",
        signature="MrpVisualizer",
        definition=function (object, mrp) {
            poststratified <- p (mrp, rep (TRUE, 3))
            average <- p (mrp)
            for (i in 1:dim(mrp@data@ybarWeighted)[2]) {
                for (j in 1:dim(mrp@data@ybarWeighted)[3]) {
                    object@plot.var1 (poststratified[, i, j], average, mrp@population[,i,j])
                }
            }
        })

setGeneric ("placeLabels", function (object, mrp) { standardGeneric ("placeLabels")})
setMethod (f="placeLabels",
        signature="MrpVisualizer",
        definition=function (object, mrp) {
            blankplot <- function (words, cex=1){
                plot (c(0,1), c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n",type="n")
                text (.5, .5, words, cex=cex)
            }
            for (i in dimnames(mrp@data@ybarWeighted)[[2]]) {
                blankplot (i)   
            }
            for (i in dimnames(mrp@data@ybarWeighted)[[3]]) {
                blankplot (i)   
            }
        })

setGeneric ("prepareLayout", function (object, ...) { standardGeneric ("prepareLayout")})
setMethod (f="prepareLayout",
        signature="MrpVisualizer",
        definition=function (object, mrp) {
            mat <- createMatForLayout (object, mrp)

            rows <- dim (mat)[1]
            cols <- dim (mat)[2]
            
            sizeOfMaps <- min (object@height / cols, object@width / rows)
            vert <- max (object@height - sizeOfMaps * (dim (mrp@data@ybarWeighted)[2]) * object@shrinkVertical, 0)
            widths <- rep(sizeOfMaps, cols)
            heights <- c (vert*0.3, rep(sizeOfMaps*object@shrinkVertical, dim (mrp@data@ybarWeighted)[2]), vert*0.15, vert*0.55)
            
            return (list(mat=mat, widths=widths, heights=heights, respect=FALSE))
        })

setGeneric ("createMatForLayout", function (object, mrp) { standardGeneric ("createMatForLayout")})
setMethod (f="createMatForLayout", 
        signature="MrpVisualizer",
        definition=function (object, mrp) {
            ## TODO: pull options out of object here
            ## if (includeAllRow) {
            ##     nrows <- nrows + 1
            ## }
            ## if (includeAllCol) {
            ##     ncols <- ncols + 1
            ## }
            nrows <- dim (mrp@data@ybarWeighted)[2]
            ncols <- dim (mrp@data@ybarWeighted)[3]
            
            rowLabels <- seq(1, nrows)  ## the labels for each row
            colLabels <- c(0, seq(max(rowLabels)+1, max(rowLabels)+ncols))
            mainChart <-  matrix (seq (max(colLabels)+1, max(colLabels)+nrows*ncols), nrows, ncols, byrow=TRUE)
            
            total.mat <- rbind (colLabels, cbind (rowLabels, mainChart))
            
            colorBar <- c(0,rep(max(total.mat)+1, ncols))
            footer <- c(0, rep(max (colorBar)+1, ncols))
            total.mat <- rbind(total.mat, colorBar, footer)
            return (total.mat)
        }
)



## Options:
## - Include options for poststratification.
##   * Subtract mean.
##   * Poststratification in either direction
##   * Poststratification only.
## - labels
## - captions
##

newMrpVisualizer <- function () {
    return (new ("MrpVisualizer"))
}

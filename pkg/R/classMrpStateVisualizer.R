setClass(Class="MrpStateVisualizer",
        contains="MrpVisualizer",
        representation=representation (
                colorFunction="function",
                statemaps.creator="function",
                
                colorFunction.lo="numeric",
                colorFunction.hi="numeric",
                colorFunction.nShades="numeric",
                
                statemaps.alternative="character",
                statemaps.minimumProportion="numeric",
                statemaps.border="character",
                
                caption="character",
                title="character"
        ),
        prototype=prototype(
                colorFunction.lo=-0.25,
                colorFunction.hi=0.25,
                colorFunction.nShades=1001,
                
                statemaps.alternative="White",
                statemaps.minimumProportion=0.01,
                statemaps.border="gray85",
                
                caption="The state is left blank where a category represents less than 1% of the voters of a state.",
                title="",
                
                plot.var1=function (data, average, population) { return (plot (data))}
        )        
)

setMethod (f="placeMisc",
        signature="MrpVisualizer",
        definition=function (object, mrp) {
            ## Place color bar
            colors <- object@colorFunction (seq (object@colorFunction.lo, object@colorFunction.hi, length.out=object@colorFunction.nShades))
            n <- length (colors)
            plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
            rect(0:(n-1)/n, 0.2, 1:n/n, 1, col = colors, border = NA)
            
            ## Place text
            text(0,   0, paste (round ((p(mrp)+ object@colorFunction.lo)*100), "%", sep=""), xpd=TRUE)
            text(0.5, 0, paste (round (p(mrp)*100), "%", sep=""), xpd=TRUE)
            text(1,   0, paste (round ((p(mrp) + object@colorFunction.hi)*100), "%", sep=""), xpd=TRUE)
            
            # Place caption
            plot (c(0,1), c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
            text (x=0.5, y=0.5, labels=object@caption, adj=c(0.5, 0.5))
            
            # Place title
            mtext(object@title, side=3, line=0.5, xpd=TRUE, cex=2, outer=TRUE)
        })

setGeneric ("setPlotFunction", function (object) { standardGeneric ("setPlotFunction")})
setMethod (f="setPlotFunction",
        signature="MrpStateVisualizer",
        definition=function (object) {
            object@plot.var1 <- createStatemapsFunction (object)
            return (object)
        })

setGeneric ("setColorFunction", function (object) { standardGeneric ("setColorFunction")})
setMethod (f="setColorFunction",
        signature="MrpStateVisualizer",
        definition=function(object) {
            object@colorFunction <- createColorFunction (object)
            return (object)
        })

setGeneric ("createColorFunction", function (object) { standardGeneric ("createColorFunction")})
setMethod (f="createColorFunction", 
        definition=function (object) {
            colorFunction <- function (data) {
                data <- pmin (object@colorFunction.hi, pmax (object@colorFunction.lo, data))
                colors <-  diverge_hcl(object@colorFunction.nShades, h = c(190, 60), c = 200, l = c(10, 90))
                return (colors[floor (1 + (object@colorFunction.nShades-1)*(data-object@colorFunction.lo)/(object@colorFunction.hi-object@colorFunction.lo))])
            }
            return (colorFunction)
        })

setGeneric ("createStatemapsFunction", function (object) { standardGeneric ("createStatemapsFunction")})
setMethod (f="createStatemapsFunction",
        signature="MrpStateVisualizer",
        definition=function (object) {
            statemaps <- function (data, average, population) {
                # some parameters to be pulled out
                stopifnot (length (population) == length(data))
                
                data <- data - average
                # Trim data to only include 48 states
                if (length(data)==51){
                    no.dc <- -9
                    data <- data[no.dc]
                    population <- population[no.dc]
                }
                if (length(data)==50){
                    lower48 <- state.abb!="AK" & state.abb!="HI"
                    data <- data[lower48]
                    population <- population[lower48]
                }
                else if (length(data)!=48){
                    stop ("wrong number of states")  
                } 
                data <- replace (data, population < object@statemaps.minimumProportion, NA)
                
                mapping <- c (lapply ((1:7), c), lapply ((9:19), c), list(20:22), list(23:24), lapply ((25:33), c),
                        list(34:37), list(38:40), lapply ((41:52), c), list(53:55), list(56:60), lapply ((61:63), c))
                data.long <- rep (NA, 63)
                projection <- "bonne"
                
                for (i in 1:48){
                    data.long[mapping[[i]]] <- data[i]
                }
                colors <- object@colorFunction (data.long)
                colors <- replace (colors, is.na (colors), object@statemaps.alternative)
                
                map('state', proj=projection, param=25, lty=0)
                m <- map('state', proj=projection, param=25, fill=TRUE, plot=FALSE)
                polygon(m$x,m$y, col=colors, lwd=0.5, border=object@statemaps.border)
            }
            return (statemaps)
        })

newMrpStateVisualizer <- function () {
    mrpStateVisualizer <- new ("MrpStateVisualizer")
    mrpStateVisualizer <- setColorFunction (mrpStateVisualizer)
    mrpStateVisualizer <- setPlotFunction (mrpStateVisualizer)
    
    return (mrpStateVisualizer)
}

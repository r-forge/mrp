setMethod("spplot", signature("mrp"),
          definition=function(obj, formula, spmap=NULL, FID="state", exclude=NULL, add.settings=list(), ...) {
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
            
            dimlabels <- lapply(obj@poll@levels[attr(plot.terms,"term.labels")],
                                function(x) { return(x$levels) })
            if(length(dimlabels)==1) {
              dimlabels[[2]] <- ""
              dimlabels <- rev(dimlabels)
            }
            ## Quietly swap the order.
            ## will need to be better if we allow
            ## multipage plots.
            
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
                              par.settings=lattice:::updateList(mrp.theme(
                                  length(dimlabels[[1]]), # row length
                                  length(dimlabels[[2]])),
                                add.settings)# col length
                              ,...
                              )

            
            
            return(theplot)
          } )

mrp.theme <- function(rowlength,collength){
  list(
       strip.background = list(col = "transparent"),
       reference.line = list(col="#00000044"),
       ##par.main.text=list(fontfamily="gotham"),
       add.line=list(col="#00000022",lwd=0), # state borders
       ##add.text=list(cex=.7,fontface="italic",
       ##  fontfamily="gotham"),
       axis.line=list(lwd=0),
       ## Here we are going to do some
       ## strip and strip.left magic.
       layout.heights=list(strip = 
         c(rep(0, rowlength-1), 
           1)), # should by dynamic for linebreaks
       layout.widths=list(strip.left=
         c(1, rep(0,collength-1))),
       strip.border=list(col="transparent"),
       regions=list(col=blue2green2red(100))
       )}


panel.polygonsplot =
function (x, y, z, subscripts, at = pretty(z), shrink, labels = NULL, 
   		label.style = c("mixed", "flat", "align"), contour = FALSE, 
   		region = TRUE, col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
   		cex = add.text$cex, font = add.text$font, fontfamily = add.text$fontfamily, 
   		fontface = add.text$fontface, col.text = add.text$col, ..., 
   		col.regions = regions$col, alpha.regions = regions$alpha, 
		grid.polygons, sp.layout, emptycolor="#00000033") 
{
	regions <- trellis.par.get("regions")
	add.line <- trellis.par.get("add.line")
	add.text <- trellis.par.get("add.text")
	numcol <- length(at) - 1
	numcol.r <- length(col.regions)
	col.regions <- if (numcol.r <= numcol) 
   			rep(col.regions, length = numcol)
   		else col.regions[floor(1 + (1:numcol - 1) * (numcol.r - 1)/(numcol - 1))]
	zcol <- rep(NA, length(z))
	for (i in seq(along = col.regions)) zcol[!is.na(x) & !is.na(y) & 
      			!is.na(z) & z >= at[i] & z < at[i + 1]] <- i
	label.style <- match.arg(label.style)
	x <- as.numeric(x[subscripts])
	y <- as.numeric(y[subscripts])
	z <- as.numeric(z[subscripts])
	zcol <- as.numeric(zcol[subscripts])

	#EJP,2010-10-8: 
	#if (is(grid.polygons, "SpatialLines"))
	#	sp.panel.layout(sp.layout, panel.number())
	sp:::sp.panel.layout(sp.layout, panel.number(), first = TRUE)
	if (any(subscripts)) {
		if (is(grid.polygons, "SpatialLines")) {
			sp.lines3 = function(x, col, ...) panel.lines(coordinates(x), col = col, ...)
			sp.lines2 = function(x, col, ...) lapply(x@Lines, sp.lines3, col, ...)
			for (i in 1:length(grid.polygons@lines))
				sp.lines2(grid.polygons@lines[[i]], col = col.regions[zcol[i]], lwd = lwd, lty = lty, ...)
		} else {
			pls = slot(grid.polygons, "polygons")
   			pO = slot(grid.polygons, "plotOrder")
   			for (i in pO) {
       			Srs <- slot(pls[[i]], "Polygons")
       			pOi <- slot(pls[[i]], "plotOrder")
       			for (j in pOi) {
					coords = slot(Srs[[j]], "coords")
					if (slot(Srs[[j]], "hole")) {
						bg = trellis.par.get()$background
						if (bg$col == "transparent")
							fill = "white"
						else
							fill = bg$col
						alpha = bg$alpha
					} else {
                                          if(is.na(zcol[i])) {
                                            fill = trellis.par.get()$reference.line$col
                                          }
						fill = col.regions[zcol[i]]
						alpha = alpha.regions
					}
					gp = grid:::gpar(fill = fill, alpha = alpha, col = col, lwd = lwd, lty = lty)
					grid.polygon(coords[,1], coords[,2], default.units = "native", 
						gp = gp)
				}
   			}
		}
	}
	# EJP, 2010-10-8
	#if (!is(grid.polygons, "SpatialLines"))
	#	sp.panel.layout(sp.layout, panel.number())
	sp:::sp.panel.layout(sp.layout, panel.number())
}

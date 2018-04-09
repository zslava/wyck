#' Add level facts on the chart intraday
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export 
`addZonefact` <- function(zones, lcol="deepskyblue",ltyp="dashed",lcex=0.5) {

 lchob <- quantmod:::get.current.chob()
 refdata <- lchob@xdata

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))


  lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)   #lchob@xdata is xts object

  chobTA <- new("chobTA")
  chobTA@new <- FALSE
  chobTA@TA.values <-x[lchob@xsubset,]

  curdata <- lchob@xdata[lchob@xsubset]
  xsubset <- lchob@xsubset
  spacing <- lchob@spacing
 
 #browser()
  datatz<-indexTZ(curdata)

  
  xvals <- seq(from=lchob@xrange[1], by=spacing, length=lchob@xrange[2] )

  
  ##compute zones to plot
  zones<-lapply(zones,function(x,xvalues,xdata){
                        cmpZoneRange(x,xfvals=xvalues,cdserie=xdata)
                        #browser()
                        return(x)
                      }
                ,xvalues=xvals,xdata=curdata)


  chobTA@name <- "chartZonefact"
  chobTA@call <- match.call()
  chobTA@on <- 1 #!NB
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale
                       ,xvals=xvals
                       ,zones=zones
                       ,lcol=lcol
					             ,ltyp=ltyp	
                       ,lcex=lcex		   
                        )

  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)

    chartSeries.chob <- quantmod:::chartSeries.chob
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  }
} #}}}
#gets called by addZonefact
#' @export 
#' @keywords internal
`chartZonefact` <-
function(x) {
    spacing <- x@params$spacing
    xvals  <- x@params$xvals
    zones <- x@params$zones
    lcol  <-  x@params$lcol
	  ltyp <-   x@params$ltyp
    lcex <-   x@params$lcex

    lapply(zones, function(x,lcolor,ltype,cex){
                    if(hasZoneRange(x)){
                      lines(x=x@lines2plot$lx, y=x@lines2plot$ly, lty=ltype, col=lcolor)
                      lines(x=x@lines2plot$hx, y=x@lines2plot$hy, lty=ltype, col=lcolor)

                      llbltxt <- paste(as.numeric(formatC(x@loYval,digits=4, format='f')),sep='')
                      hlbltxt <- paste(as.numeric(formatC(x@hiYval,digits=4, format='f')),sep='')
                      #browser()
                      text( x=x@lines2plot$lblxpos, y=x@loYval, llbltxt,pos=1,col=lcolor,cex=cex) #pos=1 below
                      text( x=x@lines2plot$lblxpos, y=x@hiYval, hlbltxt,pos=3,col=lcolor,cex=cex) #pos=3 above
                    }
                  }
           ,lcolor=lcol,ltype=ltyp,cex=lcex)
 
} # }}}

######################################
### same function for daily levels
######################################

#' @keywords internal
#' @export 
`addLevelfactIntrad` <- function(levels, lcol="darkgoldenrod",ltyp="longdash",lcex=0.5) {

 lchob <- quantmod:::get.current.chob()
 refdata <- lchob@xdata

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))


  lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)   #lchob@xdata is xts object

  chobTA <- new("chobTA")
  chobTA@new <- FALSE
  chobTA@TA.values <-x[lchob@xsubset,]

  curdata <- lchob@xdata[lchob@xsubset]
  xsubset <- lchob@xsubset
  spacing <- lchob@spacing
 
 
  datatz<-indexTZ(curdata)

  
  xvals <- seq(from=lchob@xrange[1], by=spacing, length=lchob@xrange[2] )

  
  ##compute zones to plot
  levels<-lapply(levels,function(x,xvalues,xdata){
                          cmpLevelRange(x,xfvals=xvalues,cdserie=xdata)
                          return(x)
                          }
                ,xvalues=xvals,xdata=curdata)
  
  
  

  chobTA@name <- "chartLevelfactIntra"
  chobTA@call <- match.call()
  chobTA@on <- 1 #!NB
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale
                       ,xvals=xvals
                       ,levels=levels
                       ,lcol=lcol
                       ,ltyp=ltyp 
                       ,lcex=lcex      
                        )

  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)

    chartSeries.chob <- quantmod:::chartSeries.chob
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  }
} #}}}
#gets called by addZonefact
#' @export 
#' @keywords internal
`chartLevelfactIntra` <-
function(x) {
    spacing <- x@params$spacing
    xvals  <- x@params$xvals
    levels <- x@params$levels
    lcol  <-  x@params$lcol
    ltyp <-   x@params$ltyp
    lcex <-   x@params$lcex

    lapply(levels, function(x,lcolor,ltype,cex){
                    if(hasLevelRange(x)){
                      lines(x=x@x2plot, y=x@y2plot, lty=ltype, col=lcolor)

                      lbltxt <- paste(as.numeric(formatC(x@leftYval,digits=4, format='f')),sep='')
                      #browser()
                      text( x=last(x@x2plot), y=x@leftYval, lbltxt,pos=1,col=lcolor,cex=cex) #pos=1 below
                    }
                  }
           ,lcolor=lcol,ltype=ltyp,cex=lcex)
 
} # }}}



#' Add level facts on the chart
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export 
`addLeveleod` <- function(ctime, lcol="cadetblue",ltyp="longdash", lcex=0.5) {

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

  #compute x, y vector for eod line
  lfmt <- "%Y-%m-%d %H:%M:%S"
  
  pdaytt  <- ctime - ( as.numeric(format(ctime,format='%H'))*3600 
                   + as.numeric(format(ctime,format='%M'))*60
                   + as.numeric(format(ctime,format='%S'))  + 1)
  subprd <- paste("::",base::format(pdaytt,format=lfmt),sep="")
  xvals <- seq(from=lchob@xrange[1], by=spacing, length=lchob@xrange[2] )
  msk <- rep(NA, length(xvals)) #default value
  yvals <- rep(0, length(xvals)) #default value
  hasRange <- TRUE
  lefttime <- index(last(curdata[subprd])) #  
  if (length(lefttime) == 0) { hasRange <- FALSE}
  if (hasRange){
    leftidx <- match(lefttime, index(curdata)) # never supposed to be NA 
    msk[leftidx:length(msk)] <- 1
    yvals <- rep(Cl(curdata[lefttime]), length(xvals)) # close of last quote of the previous day
  }
  yvals <- yvals * msk

  chobTA@name <- "chartLeveleod"
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
                       ,yvals=yvals
                       ,hasRange=hasRange
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
#gets called by addlevelfact
#' @export 
#' @keywords internal
`chartLeveleod` <-
function(x) {
    spacing <- x@params$spacing
    xvals  <- x@params$xvals
    yvals  <- x@params$yvals
    hasRange <- x@params$hasRange
    lcol  <-  x@params$lcol
	  ltyp <-   x@params$ltyp
	  lcex <-   x@params$lcex

    if(hasRange){
      lines(x=xvals, y=yvals, lty=ltyp, col=lcol)
      yvalue <- yvals[!is.na(yvals)][1]
      subidx<-which(yvals == yvalue)
      mid_idx <- ceiling(first(subidx) + 0.5*(last(subidx)-first(subidx)))
      xpos <- xvals[mid_idx]
      lbltxt <- paste(as.numeric(formatC(yvalue
                                 ,digits=4, format='f')),sep='')
      text( x=xpos, y=yvalue, lbltxt
           ,pos=1,col=lcol,cex=lcex) #pos=1 below
    }  
    
    #browser()
} # }}}

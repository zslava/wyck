#' Add trend facts on the chart
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export 
`addTrendfact` <- function(tfs,tcol="pink") {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))
  lchob <- quantmod:::get.current.chob()
  xdata <- lchob@xdata
  if ( length(tfs) > 0 ){
   p<-periodicity(xdata)
   pfreq <- p$frequency
   if (p$units == 'mins' ) { pfreq <- pfreq * 60 } #convert frequency to seconds
   tff <- tfs[[1]]
   if ( p$units != getDunit(tff) || p$frequency != getDfrequency(tff) ){
       stop(paste("[addTrendfact validation]"
                  ," periodicity mismatch between artifact(s) and refdata"))
   }
 }
  
  x <- as.matrix(lchob@xdata)   #lchob@xdata is xts object

  chobTA <- new("chobTA")
  chobTA@new <- FALSE
  chobTA@TA.values <-x[lchob@xsubset,]

  #curdata <- lchob@xdata[lchob@xsubset]
  xsubset <- lchob@xsubset
  spacing <- lchob@spacing
  ##create x.range here
  x.range <- seq(lchob@xrange[1],lchob@xrange[2]*spacing)


 ### compute unscaled trendline on xsubset
 if ( length(tfs) > 0 ){
  for (j in 1:length(tfs)){
      ctf <- tfs[[j]]
      cmpIntercept(ctf, xdata)
      cmpShowMskTrend(ctf, xdata)
      xcalcvals <- seq(from=first(xsubset), by=1/spacing, length=length(x.range))
      xplotvals <- x.range
      cmpUnscaledTrendRange(ctf, xcalcvals, xplotvals,xdata)
      tfs[[j]] <-ctf
   }
 }

 #browser()

  chobTA@name <- "chartTrendfact"
  chobTA@call <- match.call()
  chobTA@on <- 1 #!NB very important
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale
                       ,tfs=tfs
                       ,scaled=F
                       ,tcol=tcol
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
#gets called by addTrendfact
#' @keywords internal
#' @export 
`chartTrendfact` <-
function(x) {
    tfs    <- x@params$tfs
    tcol  <-  x@params$tcol

    scaled <- x@params$scaled
    if(scaled){ ltyval <- 'dotdash' }
    else      { ltyval <- 'dotted' }

    if(length(tfs) > 0 ) {
      for (j in 1:length(tfs)){
      ctf <- tfs[[j]]
#      ccolor <- getColor(ctf)
      ccolor <- tcol
      trline <- getTrendLine2plot(ctf)
      lines(x=trline$xvals, y=trline$yvals, lty=ltyval, col=ccolor)
      #browser()     
     }
   }
} # }}}

########
# values for src: w1,d1,h1
#values for trg: d1, subd1
#########
#' Add trend facts from higher time frame on the chart lower time frame chart
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export 
`addscTrendfact` <- function(tfs, srcdata, src='d1', trg='subd1',tcol="red") {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))
  #validation
  df <- NA
  df[ src %in% 'w1' ] <- 604800
  df[ src %in% 'd1' ] <- 86400
  df[ src %in% 'h1' ] <- 3600
 if ( length(tfs) > 0 ){
   tff <- tfs[[1]]
   if (  getDfrequency(tff) != df ){
       stop(paste("[addscTrendfact validation]"
                  ," periodicity mismatch between Trend artifact(s) and srcdata"))
   }
 }
 #validation
 if ( periodicity(srcdata)$frequency != df ){
       stop(paste("[addscTrendfact validation]"
                  ," periodicity mismatch between src param and srcdata"))
 }

  lchob <- quantmod:::get.current.chob()
  xdata <- lchob@xdata
  ##validation
  if ( trg == 'subd1' && periodicity(xdata)$frequency >= 86400 ){
       stop(paste("[addscTrendfact validation]"
                  ," periodicity of lchob data higher than", trg))
  }

  x <- as.matrix(lchob@xdata)   #lchob@xdata is xts object

  chobTA <- new("chobTA")
  chobTA@new <- FALSE
  chobTA@TA.values <-x[lchob@xsubset,]

  curdata <- lchob@xdata[lchob@xsubset]
  xsubset <- lchob@xsubset
  spacing <- lchob@spacing
  ##create x.range here
  x.range <- seq(lchob@xrange[1],lchob@xrange[2]*spacing)

 if ( length(tfs) > 0 ){
  for (j in 1:length(tfs)){
      ctf <- tfs[[j]]
      cmpIntercept(ctf, srcdata)
      cmpShowMskTrend(ctf, srcdata)
      ### compute unscaled trendline  or compute scaled trendline
         xplotvals <- seq(from=lchob@xrange[1], by=spacing, length=lchob@xrange[2] )
         cmpnScaledTrendRange(ctf, xplotvals,srcdata,curdata,trg)
      tfs[[j]] <-ctf
      #browser()
   }
 }

  chobTA@name <- "chartTrendfact"
  chobTA@call <- match.call()
  chobTA@on <- 1 #!NB very important
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale
                       ,tfs=tfs
                       ,scaled=T
                       ,tcol=tcol
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

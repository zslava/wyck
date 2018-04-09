#' Add level facts on the chart
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export 
`addLevelfact` <- function(lvls,lcex=0.5,loff=0.10,lcol="blue", hcol="forestgreen",iccol="brown",ltyp="solid") {

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
  ##create x.range here
  x.range <- seq(lchob@xrange[1],lchob@xrange[2]*spacing)
 ##compute intercepts
 if ( length(lvls) > 0 ){
  for (j in 1:length(lvls)){
      clvl <- lvls[[j]]
      ### compute masked level horizontal line ( on ref periodicity and subperiodicity as.well
       xcalcvals <- seq(from=first(xsubset),by=1/spacing, length=length(x.range))
       xplotvals <- seq(from=lchob@xrange[1], by=spacing, length=lchob@xrange[2] )
       cmpLevelRange(clvl, xplotvals, curdata)
       lvls[[j]] <-clvl
       #browser()
  }
 }

  chobTA@name <- "chartLevelfact"
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
                       ,lvls=lvls
                       ,xsubset=lchob@xsubset
                       ,lcol=lcol
                       ,hcol=hcol
					   ,iccol=iccol			   
                       ,lcex=lcex
                       ,loff=loff
					   ,ltyp=ltyp			   
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
`chartLevelfact` <-
function(x) {
    spacing <- x@params$spacing
    lvls    <- x@params$lvls
    lcol  <-  x@params$lcol
    hcol  <-  x@params$hcol
	  iccol  <-  x@params$iccol	
    lcex <-   x@params$lcex
    loff <-   x@params$loff
	  ltyp <-   x@params$ltyp
	
    if( length(lvls) > 0 ) {
      for (j in 1:length(lvls)){

      clvl <- lvls[[j]]
	  
	    ##print labels on extremities
	    lbltxt <- paste(as.numeric(formatC(getLevel(clvl), digits=4, format='f')),sep='')		  
	    if ( getSlabel(clvl) == "ICL" ){
		   ccolor <- iccol
	    }else if ( getSlabel(clvl) == "HFL" ){
		    #lbltxt <- paste(formatC(getLevel(clvl), digits=4, format='f'),sep='')
		    ccolor <- hcol
	    }else if ( getSlabel(clvl) == "LVL" ){
		    ccolor <- lcol
	    }
      trline <- getLevelLine2plot(clvl)
      
      lines(x=trline$xvals, y=trline$yvals, lty=ltyp, col=ccolor)

      nemptyyvals <- trline$yvals[!is.na(trline$yvals) ]
      lxpos <- match( first(nemptyyvals), trline$yvals )
      ypos <-getLevel(clvl)
	    if( length(nemptyyvals) > 1 ){
      text( (lxpos-2)*spacing, ypos, lbltxt
           ,col=ccolor
           ,pos=3 # below
           ,cex=lcex
           ,offset=loff ) ##!
     }
     #browser()
   }
 }
 
} # }}}

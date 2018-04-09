
#' Add level label facts on the chart
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
###' @export
`addWfact` <- function(wfs,lbltype="dt",lcex=0.5,loff=0.25,lupcol="green",ldncol="red",cepw=FALSE,mside,rolls) {
#`addWfact` <- function(wfs,lbltype="dt",lcex=0.5,loff=0.25,lcol="red") {
  ## lbltype values dt or ab
  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)   #lchob@xdata is xts object



  ##compute labels as function of lbltype
   if (length(wfs) > 0 ) {
     for (j in 1:length(wfs)){
      cwf <- wfs[[j]]
      cmpLabelParams(cwf,lbltype)
      wfs[[j]] <- cwf
     }
   }


  chobTA <- new("chobTA")
  chobTA@new <- FALSE
  chobTA@TA.values <-x[lchob@xsubset,]

  curdata <- lchob@xdata[lchob@xsubset]
  xsubset <- lchob@xsubset


  chobTA@name <- "chartWfact"
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
                       ,xsubset=lchob@xsubset
                       ,curdata=curdata
                       ,wfs=wfs
                       ,lupcol=lupcol
					             ,ldncol=ldncol
			                 ,lcex=lcex
                       ,loff=loff
			   	             ,cepw=cepw
			   	             ,mside=mside
					             ,rolls=rolls
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
###' @export 
#' @keywords internal
`chartWfact` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    wfs    <- x@params$wfs
    xsubset <- x@params$xsubset
    curdata <- x@params$curdata

    y <- x@params$curdata
    x.pos <- 1 + spacing * (1:length(y) - 1)

    lupcol  <-  x@params$lupcol
	  ldncol  <-  x@params$ldncol
	  lcex <-   x@params$lcex
    loff <-   x@params$loff
	  cepw <-   x@params$cepw
	
	mside <- x@params$mside
	rolls     <-  x@params$rolls
	
	d1fmt<-"%Y-%m-%d"
	##rolls vertical lines for contract sequence instruments
	if( length(rolls) >0){
		rlids <- match( unlist(rolls) , strftime( index(curdata),format=d1fmt)   )
		prlids <- rlids[!is.na(rlids)]
		rvl <- prlids * spacing
		rvl <- rvl -(1) ##-spacing
		abline(v=rvl,col=x@params$colors$grid.col, lty="dotdash")	
	}
	
  ##determine  the index of facts defined within xsubset
  if ( length(wfs) == 0 ){ #abort
		return( invisible())
	}
      wfdts <- as.POSIXct( unlist(lapply(wfs, function(x){ getOwndtimeStr(x) })), tz=indexTZ(curdata) )
      dtlimleft <- first(index(curdata))
      dtlimright <- last(index(curdata))
      msk <- ifelse( (wfdts >= dtlimleft & wfdts <= dtlimright), 1,NA )
      fltrdidx <- msk * 1:length(wfdts)
      fltrdidx <- fltrdidx[ !is.na(fltrdidx) ]

   if (length(fltrdidx) > 0 ) {
	 #fwfdts <- as.POSIXct( unlist(lapply(wfs[fltrdidx], function(x){ getOwndtimeStr(x) })) )
	 fwfdts <-  unlist(lapply(wfs[fltrdidx], function(x){ getOwndtimeStr(x) }))
		
    #browser


		 ###place optionally wyckoff vertical lines here
	 if(cepw){
		#wfid <- match(fwfdts,index(curdata))
		wfid <- match(fwfdts, strftime( index(curdata),format=d1fmt)  )
		
		vl <- wfid * spacing
		vl <-vl-spacing	 
		abline(v=vl,col=x@params$colors$grid.col, lty="dashed")
	 }
	 

	 ### put wyckoff fact text labels 
     for (j in 1:length(fltrdidx)){
      cid <- fltrdidx[j]
      cwf <- wfs[[cid]]
      #ownstridx <- getOwndtimeStr(cwf)
      ownPOSIXct <- getOwndtime(cwf) 
      #browser()
      #wfidx <- match( index(y[ownstridx]), index(y) )
      wfidx <- match(  ownPOSIXct, index(y) )
      #browser()
      if (!is.na(wfidx)){
        xpos <- x.pos[wfidx]
        #compute y-position
        yval <- getYval(cwf)

          ypos <- yval
          if ( abs(yval - as.numeric( Hi(y[wfidx]))) < abs(yval -as.numeric(Lo(y[wfidx]))))
               { lpos <- 3 }
          else { lpos <- 1 }
		  
		 ##color
	  	wcolor <- lupcol #default u color
	  	m<-match(getSlabel(cwf)  ,mside[,1])
	  	if(!is.na(m)){
	  		#browser()
	  		if(as.numeric(mside[m,2]) == 1){
	  			wcolor <- lupcol
	  		}else{
	  			wcolor <-ldncol
	  		}
	  	}
	  	##plot label
	  	text(xpos, ypos
              ,getLabel2chart(cwf)
	  		,col=wcolor, pos=lpos
	          ,offset=loff, cex=lcex )
     } 
    }
  }
  #browser()
}



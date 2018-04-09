#' Add vertical separation lines on volumes pane in a  chart according to period
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export
`addfVoep` <- function(log.scale=FALSE,bv=T,cep=T,avrgvcol="magenta") {
   lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)
  if(!lchob@show.vol || !has.Vo(x))
    return(invisible(new('chobTA', new=FALSE, name="chartNULL", call=match.call())))

  Volumes <- Vo(x)
  max.vol <- max(Volumes,na.rm=TRUE)
  vol.scale <- list(100, "100s")
  if (max.vol > 10000)
    vol.scale <- list(1000, "1000s")
  if (max.vol > 1e+05)
    vol.scale <- list(10000, "10,000s")
  if (max.vol > 1e+06)
    vol.scale <- list(1e+05, "100,000s")
  if (max.vol > 1e+07)
    vol.scale <- list(1e+06, "millions")

#   ### set param for endpoints
#   epbrd <- list('years', 1) #default value
#   p <- periodicity(x)
#   pf <-p$frequency
#   pu <-p$units
#   if (pf > 0 && pf < 5  && pu == "mins" ) { epbrd <- list('minutes', 15) }
#   if (pf >= 5 && pf <= 15 && pu == "mins" ) { epbrd <- list('hours', 1) }
#   if (pf > 15  && pf <= 30 && pu=="mins" ) { epbrd <- list('hours', 2) }
#   if (pf >= 3600 && pf < 86400 && pu=="hours") { epbrd <- list('days', 1) }
#   if (pf >= 86400 && pf < 604800 && pu=="days") { epbrd <- list('weeks', 1) }
#   if (pf >= 604800 && pf < 2592000 && pu=="days") { epbrd <- list('months', 1) }
#   if (pf >= 2592000 && pf < 31536000 && pu=="days") { epbrd <- list('years', 1) }



  if(lchob@color.vol & is.OHLC(x)) {
    # calculate colors for bars, if applicable.
    Opens  <- Op(x)
    Closes <- Cl(x)
    if(lchob@multi.col) {
      # colored bars - 4 color
      last.Closes <- as.numeric(Lag(Closes))
      last.Closes[1] <- Closes[1]
      bar.col <- ifelse(Opens < Closes,
                        ifelse(Opens < last.Closes,
                               lchob@colors$dn.up.col,
                               lchob@colors$up.up.col),
                        ifelse(Opens < last.Closes,
                               lchob@colors$dn.dn.col,
                               lchob@colors$up.dn.col))
    } else {
      # colored bars - 2 color
      bar.col <- ifelse(Opens < Closes,
                        lchob@colors$up.col,
                        lchob@colors$dn.col)
    }
      # 1 color bars
  } else bar.col <- ifelse(!is.null(lchob@colors$Vo.bar.col),
                           lchob@colors$Vo.bar.col,lchob@colors$border)
  border.col <- ifelse(is.null(lchob@colors$border),
                       bar.col,lchob@colors$border)

  bar.col <- bar.col[lchob@xsubset]
  

  
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  chobTA@TA.values <- (Volumes/vol.scale[[1]])[lchob@xsubset]
  chobTA@name <- "chartfVoep"
  chobTA@call <- match.call()

  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        vol.scale=vol.scale,
                        x.labels=lchob@x.labels,
                        log.scale=log.scale,
                        bar.col=bar.col,border.col=border.col,
                        time.scale=lchob@time.scale
					   ,cep=cep
                       #,epbrd=epbrd
			           ,avrgvcol=avrgvcol
                        )
  if(bv){
    chobTA@params$thin <- FALSE 
  }else{
	  chobTA@params$thin <- TRUE
  } 
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  }
} # }}}
# chartVoep {{{
#' @export 
`chartfVoep` <-
function(x) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    if(class(x) != "chobTA") stop("chartVo requires a suitable chobTA object")
    Volumes <- x@TA.values

    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)
    ## for vertical lines
	cep   <- x@params$cep
	#epbrd <- x@params$epbord
    avrgvcol <-x@params$avrgvcol
	
#    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol
    log.scale <- ifelse(x@params$log.scale,"y","")

    vol.scale <- x@params$vol.scale

    if(x@new) {
      plot.new()
      plot.window(xlim=c(1, x@params$xrange[2] * spacing),
                  ylim=c(min(Volumes,na.rm=TRUE),max(Volumes,na.rm=TRUE)),
                  log=log.scale)
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      abline(h=axTicks(2), col=x@params$colors$grid.col, lty='dotted')
    }


	
	##optional vertical periodic lines
	if(cep){
		
		epbrd <- list('years', 1) #default value
		p <- periodicity(Volumes)
		pf <-p$frequency
		pu <-p$units
		if (pf > 0 && pf < 5  && pu == "mins" ) { epbrd <- list('minutes', 30) }
		if (pf >= 5 && pf <= 15 && pu == "mins" ) { epbrd <- list('hours', 1) }
		if (pf > 15  && pf <= 30 && pu=="mins" ) { epbrd <- list('hours', 2) }
		if (pf >= 3600 && pf < 86400 && pu=="hours") { epbrd <- list('days', 1) }
		if (pf >= 86400 && pf < 604800 && pu=="days") { epbrd <- list('weeks', 1) }
		if (pf >= 604800 && pf < 2592000 && pu=="days") { epbrd <- list('months', 1) }
		if (pf >= 2592000 && pf < 31536000 && pu=="days") { epbrd <- list('years', 1) }		
		
		
		epvl <- endpoints(Volumes, on=epbrd[[1]], k=epbrd[[2]])
		vl <- epvl*spacing
		##get rid of first and last vbord
		vl <-vl[-1]
		vl <-vl[-length(vl)]
		abline(v=vl,col=x@params$colors$grid.col, lty="dotted")
	}
	
	x.pos <- 1 + spacing * (1:length(Volumes) - 1)
	
	bar.col <- if(x@params$color.vol) {
				x@params$bar.col
			} else x@params$border.col
	
	border.col <- x@params$border.col	
    #### plotting volume bars 
    if(x@params$thin) {
      # plot thin volume bars if appropriate
      segments(x.pos,0,x.pos,Volumes,col=bar.col)
    } else {
      rect(x.pos-spacing/3,0,x.pos+spacing/3,Volumes,
           col=bar.col,border=border.col)
    }

    legend("topleft"
          ,legend=c( paste("Volume (",vol.scale[[2]],"):",sep='')
                    ,format(last(Volumes)*vol.scale[[1]],big.mark=',')
                    ,"Avrg subset Volume:"
                    ,format(mean(Volumes,na.rm=TRUE)*vol.scale[[1]],big.mark=',')
                   )
                  ,text.col=c(x@params$colors$fg.col, last(bar.col))
                  ,bty="n"
                  ,y.inter=0.95)





	
	## line for avg volume
	abline(h=mean(Volumes,na.rm=T),col=avrgvcol,lty="dashed")
	
	axis(2)
	axis(4)
	box(col=x@params$colors$fg.col)
	invisible(vector('list',2))
	
}

#' @export
`addwVo` <- function(wfs,log.scale=F,bv=T,cep=F,cepw=F,wcol=F,vwupcol="green",vwdncol="skyblue",avrgvcol="magenta",mside,rolls) {
	lchob <- quantmod:::get.current.chob()	
	x <- as.matrix(lchob@xdata)
	if(!lchob@show.vol || !has.Vo(x))
		return(invisible(new('chobTA', new=FALSE, name="chartNULL", call=match.call())))
	
	Volumes <- Vo(x)
	max.vol <- max(Volumes,na.rm=TRUE)
	vol.scale <- list(100, "100s")
	if (max.vol > 10000) 
		vol.scale <- list(1000, "1000s")
	if (max.vol > 1e+05) 
		vol.scale <- list(10000, "10,000s")
	if (max.vol > 1e+06) 
		vol.scale <- list(1e+05, "100,000s")
	if (max.vol > 1e+07) 
		vol.scale <- list(1e+06, "millions")
	
    #browser()
	
	if(lchob@color.vol & is.OHLC(x)) {
		# calculate colors for bars, if applicable.
		Opens  <- Op(x)
		Closes <- Cl(x)
		if(lchob@multi.col) {
			# colored bars - 4 color
			last.Closes <- as.numeric(Lag(Closes))
			last.Closes[1] <- Closes[1]
			bar.col <- ifelse(Opens < Closes,
					ifelse(Opens < last.Closes,
							lchob@colors$dn.up.col,
							lchob@colors$up.up.col),
					ifelse(Opens < last.Closes,
							lchob@colors$dn.dn.col,
							lchob@colors$up.dn.col))
		} else {
			# colored bars - 2 color
			bar.col <- ifelse(Opens < Closes,
					lchob@colors$up.col,
					lchob@colors$dn.col)
		}
		# 1 color bars
	} else bar.col <- ifelse(!is.null(lchob@colors$Vo.bar.col),
				lchob@colors$Vo.bar.col,lchob@colors$border)
	border.col <- ifelse(is.null(lchob@colors$border),
			bar.col,lchob@colors$border)
	
	bar.col <- bar.col[lchob@xsubset]
	
	chobTA <- new("chobTA")
	chobTA@new <- TRUE
	
	curdata <- lchob@xdata[lchob@xsubset]
	
	chobTA@TA.values <- (Volumes/vol.scale[[1]])[lchob@xsubset]
	chobTA@name <- "chartwVo"
	chobTA@call <- match.call()
	
	chobTA@params <- list(xrange=lchob@xrange,
			colors=lchob@colors,
			color.vol=lchob@color.vol,
			multi.col=lchob@multi.col,
			spacing=lchob@spacing,
			width=lchob@width,
			bp=lchob@bp,
			vol.scale=vol.scale,
			x.labels=lchob@x.labels,
			log.scale=log.scale,
			bar.col=bar.col,border.col=border.col,
			time.scale=lchob@time.scale
		    ,curdata=curdata
	        ,wupcol=vwupcol
		    ,wdncol=vwdncol
	        ,avrgvcol=avrgvcol
	        ,wcol=wcol
			,wfs=wfs
			,mside=mside
		    ,cep=cep
		    ,cepw=cepw
	        ,rolls=rolls)
	if(bv){
	  chobTA@params$thin <- FALSE
     }else{
	  chobTA@params$thin <- TRUE
  	 }
	
	
	if(is.null(sys.call(-1))) {
		TA <- lchob@passed.args$TA
		lchob@passed.args$TA <- c(TA,chobTA)
		lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
		do.call('chartSeries.chob',list(lchob))
		invisible(chobTA)
	} else {
		return(chobTA)
	} 
}
#' @export
`chartwVo` <-
		function(x) {
	# if volume is to be plotted, do so here
	# scale volume - vol.divisor
	if(class(x) != "chobTA") stop("chartVo requires a suitable chobTA object")
	Volumes <- x@TA.values
	
	spacing <- x@params$spacing
	width <- x@params$width
	
	x.range <- x@params$xrange
	x.range <- seq(x.range[1],x.range[2]*spacing)
	
#    multi.col <- x@params$multi.col
	color.vol <- x@params$color.vol
	log.scale <- ifelse(x@params$log.scale,"y","")
	
	vol.scale <- x@params$vol.scale
	
	wcol    <-  x@params$wcol
	wupcol  <-  x@params$wupcol
	wdncol  <-  x@params$wdncol
	avrgvcol<-  x@params$avrgvcol
	wfs     <-  x@params$wfs
	mside   <-  x@params$mside
	curdata <-  x@params$curdata
	cep      <-  x@params$cep 
	cepw     <-  x@params$cepw
	rolls     <-  x@params$rolls
	
	
	if(x@new) {
		plot.new()
		plot.window(xlim=c(1, x@params$xrange[2] * spacing),
				ylim=c(min(Volumes,na.rm=TRUE),max(Volumes,na.rm=TRUE)),
				log=log.scale)
		coords <- par('usr')
		rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
		abline(h=axTicks(2), col=x@params$colors$grid.col, lty='dotted')
	}
	
	x.pos <- 1 + spacing * (1:length(Volumes) - 1)
	
	bar.col <- if(x@params$color.vol) {
				x@params$bar.col
			} else x@params$border.col
	
	border.col <- x@params$border.col
	
	if(x@params$thin) {
		# plot thin volume bars if appropriate
		segments(x.pos,0,x.pos,Volumes,col=bar.col)
	} else {
		rect(x.pos-spacing/3,0,x.pos+spacing/3,Volumes,
				col=bar.col,border=border.col)
	}
	legend.text <- list(list(
					legend=c(paste("Volume (",vol.scale[[2]],"):",sep=''),format(last(Volumes)*vol.scale[[1]],big.mark=',')),
					text.col=c(x@params$colors$fg.col, last(bar.col))
			))
	legend("topleft",
			legend=c(paste("Volume (",vol.scale[[2]],"):",sep=''),format(last(Volumes)*vol.scale[[1]],big.mark=',')),
			text.col=c(x@params$colors$fg.col, last(bar.col)), bty="n", y.inter=0.95)

	
	
	## line for avg volume
	 abline(h=mean(Volumes,na.rm=T),col=avrgvcol,lty="dashed")
	
	
	##optionally chart periodic vertical lines
	if(cep){
		### set param for periodic endpoints
		epbrd <- list('years', 1) #default value
		p <- periodicity(Volumes)
		pf <-p$frequency
		pu <-p$units
		if (pf > 0 && pf < 5  && pu == "mins" ) { epbrd <- list('minutes', 15) }
		if (pf >= 5 && pf <= 15 && pu == "mins" ) { epbrd <- list('hours', 1) }
		if (pf > 15  && pf <= 30 && pu=="mins" ) { epbrd <- list('hours', 2) }
		if (pf >= 3600 && pf < 86400 && pu=="hours") { epbrd <- list('days', 1) }
		if (pf >= 86400 && pf < 604800 && pu=="days") { epbrd <- list('weeks', 1) }
		if (pf >= 604800 && pf < 2592000 && pu=="days") { epbrd <- list('months', 1) }
		if (pf >= 2592000 && pf < 31536000 && pu=="days") { epbrd <- list('years', 1) }	
		
		epvl <- endpoints(Volumes, on=epbrd[[1]], k=epbrd[[2]]) 
		vl <- epvl*spacing
		##get rid of first and last vbord
		vl <-vl[-1] 
		vl <-vl[-length(vl)]
		abline(v=vl,col=x@params$colors$grid.col, lty="dotted")                           
		
	}
	
	##optionally chart colors for Wfacts
	d1fmt<-"%Y-%m-%d"
	##rolls vertical lines for contract sequence instrument
	if( length(rolls) >0){
		rlids <- match( unlist(rolls) , strftime( index(curdata),format=d1fmt)   )
		prlids <- rlids[!is.na(rlids)]
		rvl <- prlids * spacing
		rvl <- rvl -(1) #-spacing
		abline(v=rvl,col=x@params$colors$grid.col, lty="dotdash")	
	}	
	##determine  the index of facts defined within xsubset
	if ( length(wfs) > 0 ){
		wfdts <- as.POSIXct( unlist(lapply(wfs, function(x){ getOwndtimeStr(x) })) )

		dtlimleft <- first(index(curdata))
		dtlimright <- last(index(curdata))
		msk <- ifelse( (wfdts >= dtlimleft & wfdts <= dtlimright), 1,NA )
		fltrdidx <- msk * 1:length(wfdts)
		fltrdidx <- fltrdidx[ !is.na(fltrdidx) ]
		if (length(fltrdidx) > 0 ) {
			
		 #fwfdts <- as.POSIXct( unlist(lapply(wfs[fltrdidx], function(x){ getOwndtimeStr(x) })) )
		 fwfdts <-  unlist(lapply(wfs[fltrdidx], function(x){ getOwndtime(x) }))
		 
		 wfid <- match(fwfdts,index(curdata))
		 #wfid <- match(fwfdts, strftime( index(curdata),format=d1fmt)  )
		 vl <- wfid * spacing
		 vl <- vl-(spacing-1)
		 ###optionally chart vertical lines for wfacts
		 if(cepw){
		   abline(v=vl,col=x@params$colors$grid.col, lty="dashed")	
	     }
		 fwsl <- unlist(lapply(wfs[fltrdidx], function(x){getSlabel(x)})) 
		 colIds<-as.numeric(sapply(fwsl,function(x,y){y[match(x,y[,1]),2]},y=mside))
		 colIds[is.na(colIds)] <- 1 ## set colId to bullish for wfs no more present in wdict
		 cols<-ifelse(colIds==1,wupcol,wdncol)
		 bar.col[wfid] <-cols
		
		
		 ##optionally redraw volume bars for wyckoff
		 if(wcol){
		 rect(vl-spacing/3,0,vl+spacing/3,Volumes[wfid],
				col=cols,border=border.col)
	     }
	   }
	}

	axis(2)
	axis(4)
	box(col=x@params$colors$fg.col)
	
	
	invisible(vector('list',2))
} # }}}




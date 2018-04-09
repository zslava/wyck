


 #' @export
 #' @docType methods
setGeneric(name='co',function(object,tf, lbl=FALSE,nbp=1,scf=1){standardGeneric('co')})
 #'  produce a chart with order labels
  #' @aliases co,Wwatchlist,ANY-method
setMethod('co','Wwatchlist'
	      , function(object, tf,lbl=FALSE,nbp=1,scf=1 ){


if(missing(tf)){
     ctf <-  getCurtimeframe(object) 
	}else{
	 tfokvals <- c('w1', 'd1', 'h1', 'm30', 'm15', 'm10', 'm5', 'm3', 'm1')
     if( !(tf %in% tfokvals)){
           stop(paste("[Wwatchlist:cco validation]"
                     ,"supplied param for tf is not of expected value"))
     }
     ctf <- tf
	}
	mldl <- object@mdata
	ob <- object@orderbook

	iname<-getCinstrument(mldl)
	cdate <- getCdate(mldl)
    chartName <- paste(iname," ","(",ctf,")",sep="")

    
    chartdates <- chartDatesRange(edate=cdate, dsubset=getSubset(object,ctf), nbp=nbp,scf=scf)
    ## add to chartdates right 23:59:00
    chartdates$rightdt <- chartdates$rightdt + 23*3600 + 59*60  # this is a temp solution

    csubs <- chartDatesRange2String(edate=cdate, dsubset=getSubset(object,ctf), nbp=nbp,scf=scf)

    fmt<- '%Y-%m-%d %H:%M:%S'   
    o2c <-  getOrdersSubset(ob, ticker=iname
                            ,sdate=base::format(chartdates$leftdt,format=fmt)
                            ,edate=base::format(chartdates$rightdt,format=fmt)
    	                      ,tcrit='uncanceled', ocrit='uncanceled')
    
    assign('o2c',o2c,envir=parent.frame(n=1))
    assign('lbl',lbl,envir=parent.frame(n=1))

    #ohlc data
    dd  <- getCdatedata(mldl, iname, ctf)
    ##graph params
    wg <- getGraphconf(object)
 	theme.pars <- getThemepars(wg)
	bartyp <- theme.pars$bartype
	gtheme <-  theme.pars$themecolor
	upcol <-   theme.pars$up.col
	dncol <-   theme.pars$dn.col
    btype <- "bars"

    assign('gtheme',gtheme,envir=parent.frame(n=1))
    TAstr <- "addVo();addOrderfact(o2c,lcex=0.5,poff=2,theme=gtheme,txtlbl=lbl)"


    chartSeries( dd
                ,name=chartName
                ,TA=TAstr                    
                ,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
                ,subset=csubs )
    
    rm(o2c,envir=parent.frame(n=1))
    rm(lbl,envir=parent.frame(n=1))
    rm(gtheme,envir=parent.frame(n=1))

return( invisible() )
})

 #' @export
 #' @docType methods
setGeneric(name='itco',function(.Object,dfreq=3600,lbl=FALSE,npf=1,scf=1){standardGeneric('itco')})
 #'  produce a chart with order labels on intraday chart
  #' @aliases itco,Wwatchlist,ANY-method
setMethod('itco','Wwatchlist'
          , function(.Object, dfreq=3600,lbl=FALSE,npf=1,scf=1 ){

    if( !dfreq  %in% getDataFreqValidValues()){
          stop(paste("[OrderCharting:itco validation]"
            ,"dfreq expected to have a value in", paste(getDataFreqValidValues(),collapse=", ") ))    
    
    }

    nameObject <- deparse(substitute(.Object))
    cinstr<-getCinstrument(getMarketdata(.Object))
    ob <- .Object@orderbook
    #daily
    if(dfreq >=86400  ) {
      ctf <- getCurtimeframe(.Object)
      chkdt <- getMarketdatalastdate(.Object, tkr=cinstr,tf=ctf) # returns POSIXct
      cdata <- getMarketdataserie(.Object, tkr=cinstr, tf=ctf)
    }else{ #intraday
      chkdt <- getCintradate(getMarketdata(.Object))      # returns POSIXct
      ndd <- 1+ceiling(scf*getIntraDayWindowDays(dfreq,isToint=TRUE))
      cdata <-getIntraCdatedata(getMarketdata(.Object), cinstr, dfreq=dfreq, ndays=ndd)
    }

    ## getOrdersSubset !!
    fmt<- '%Y-%m-%d %H:%M:%S'   
    o2c <-  getOrdersSubset(ob, ticker=cinstr
                            ,sdate=base::format(index(first(cdata)),format=fmt)
                            ,edate=base::format(index(last(cdata)),format=fmt)
                              ,tcrit='uncanceled', ocrit='uncanceled')

    gtheme <- "black"
    lo <- list(ctime=chkdt)
    assign('gtheme',gtheme,envir=parent.frame(n=npf))
    assign('lo',lo,envir=parent.frame(n=npf))
    assign('lbl',lbl,envir=parent.frame(n=npf))
    assign('o2c',o2c,envir=parent.frame(n=1))

    #TAstr <- "addVo()"
    TAstr <- "addVo();addOrderfact(o2c,lcex=0.5,poff=2,theme=gtheme,txtlbl=lbl)"
    
    layout(matrix(1:2,nrow=2,ncol=1  ))
    chartName <- paste(cinstr,'_',dfreq,sep='')
    d_offset <- ceiling(scf*getNbarsUnit(cdata)) 
    chartSeries(cdata[(nrow(cdata)-d_offset):nrow(cdata)]  #window
                        ,name=chartName,type='bars',layout=NULL
                        ,TA=TAstr )
    layout(1) # reset back layout
    rm('lo',envir=parent.frame(n=npf)) 
    rm('lbl',envir=parent.frame(n=npf)) 
    rm('o2c',envir=parent.frame(n=npf)) 

    assign(nameObject,.Object,envir=parent.frame())
    return( invisible() )
    

})
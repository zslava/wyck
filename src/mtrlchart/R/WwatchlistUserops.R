####Wwatchlist Facts user operation functions

###### operational functions time frame setting
 #' @export
 #' @docType methods
setGeneric(name='stfw1',function(.Object){standardGeneric('stfw1')})
 #' set current time frame to weekly
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases stfw1,Wwatchlist,ANY-method
setMethod('stfw1','Wwatchlist'
		  ,function(.Object){
			nameObject <- deparse(substitute(.Object))
			 setCurtimeframe(.Object) <- 'w1'
			 assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })

 #' @export
 #' @docType methods
setGeneric(name='stfd1',function(.Object){standardGeneric('stfd1')})
 #' set current time frame to daily
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases stfd1,Wwatchlist,ANY-method
setMethod('stfd1','Wwatchlist'
		  ,function(.Object){
			nameObject <- deparse(substitute(.Object))
			 setCurtimeframe(.Object) <- 'd1'
			 assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })
 #' @export
 #' @docType methods
setGeneric(name='stfh1',function(.Object){standardGeneric('stfh1')})
 #' set current time frame to hourly
 #'
 #' reserved for future use
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified‰
 #' @aliases stfh1,Wwatchlist,ANY-method
setMethod('stfh1','Wwatchlist'
		  ,function(.Object){
			nameObject <- deparse(substitute(.Object))
			 setCurtimeframe(.Object) <- 'h1'
			 assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })

####### operational functions market data update ##########
## go next day
 #' @export
 #' @docType methods
setGeneric(name='gnd',function(.Object,f=FALSE,n=1,sav=TRUE){standardGeneric('gnd')})
 #' go to next day data
 #'
 #' downloads data in case of data mode \code{"live"} or goes to next date data
 #' if data mode is \code{"historic"}
 #'
 #' @param Wwatchlist instance
 #' @param f Boolean if true rewinds watchlist to first instrument
 #'  default value is TRUE
 #' @return void. Wwatchlist instance is modified
 #' @aliases gnd,Wwatchlist,ANY-method
setMethod('gnd','Wwatchlist'
		  ,function(.Object,f=FALSE,n=1,sav=TRUE){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- getMarketdata(.Object)
				   fetchNextDay(mldl,n=n)
				   #add syncinc intraday time if it is set
				   if(!is.null(mldl@mpars$cintrdate)){
				   	syncIntradayDate(mldl)
				   }
				   .Object@mdata <- mldl
				   fmt <- "%Y-%m-%d %A"
				   lfmt <- "%Y-%m-%d %H:%M:%S"
				   print(paste('current date:',format(getCdate(mldl),format=fmt) ))
				   if(!is.null(mldl@mpars$cintrdate)){				   
				   	print(paste('current time:',format(getCintradate(mldl),format=lfmt) ))
				   }
				   if(f){
					  gif(.Object)
					  cat("Instrument list rewinded to start\n")
				   }
			if(sav && getMode(mldl)=='live'){persistMdata(.Object)} ##here we need to persist just a mldata field for live dmode

		   }else{
			  stop(paste("[Wwatchlist:gnd validation]"
						,"market list data undefined for this wwatchlist."))
		   }
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })
## go next day with rechart
 #' @export
 #' @docType methods
setGeneric(name='gndc',function(.Object,itrf=T){standardGeneric('gndc')})
 #' go to next day data with rechart
 #'
 #' downloads data in case of data mode \code{"live"} or goes to next date data
 #' if data mode is \code{"historic"}  ; chart is updated
 #'
 #' @param Wwatchlist instance
 #' @param itrf Boolean if true rewinds watchlist to first instrument
 #'  default value is TRUE
 #' @return void. Wwatchlist instance is modified
 #' @aliases gndc,Wwatchlist,ANY-method
setMethod('gndc','Wwatchlist'
		  ,function(.Object,itrf=T){
			nameObject <- deparse(substitute(.Object))
			gnd(.Object,itrf)
			cc(.Object,npf=2) #rechart
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })

### go next week
 #' @export
 #' @docType methods
setGeneric(name='gnw',function(.Object,itrf=T){standardGeneric('gnw')})
 #' go to next week data
 #'
 #' relevant for data mode  \code{"historic"}. cursor  goes to next week data
 #'
 #' @param Wwatchlist instance
 #' @param itrf Boolean if true rewinds watchlist to first instrument
 #'  default value is TRUE
 #' @return void. Wwatchlist instance is modified
 #' @aliases gnw,Wwatchlist,ANY-method
setMethod('gnw','Wwatchlist'
		  ,function(.Object,itrf=T){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- .Object@mdata
				   fetchNextWeek(mldl)
				   .Object@mdata <- mldl
				   cat("CURRENT DATE: "
					   ,strftime(getCdate(.Object@mdata),format="%Y-%m-%d")
					   ,strftime(getCdate(.Object@mdata), format="%A" ) , "\n")
				   if( itrf){
					  gif(.Object)
					  cat("Instrument list rewinded to start\n")
				   }
			persist(.Object)
		   }else{
			  stop(paste("[Wwatchlist:gnw validation]"
						,"market list data undefined for this wwatchlist."))
		   }
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })

## go next minute (for intraday data)
 #' @export
 #' @docType methods
setGeneric(name='gnm',function(.Object,n=1,scale=60){standardGeneric('gnm')})
 #' go to next day data
 #'
 #' TODO  implement for live mode of Mldata 
 #' TODO  update  cdate with n is big  to have cdate and cintradate synced
 #'
 #' @param Wwatchlist instance
 #' @param  n = number of units to advance
 #' @param  scale = intraday time frame in seconds (1,60,180,300,..3600,..)
 #' @return void. Wwatchlist instance is modified
 #' @aliases gnm,Wwatchlist,ANY-method
setMethod('gnm','Wwatchlist'
		  ,function(.Object,n=1,scale=60){
			nameObject <- deparse(substitute(.Object))
			mldl <- getMarketdata(.Object)
			if(getMode(mldl) == 'historic' && !is.null(mldl@mpars$cintrdate) ) {
					incrIntradayDate(mldl,delta=n,scale=scale)
			}
			.Object@mdata <- mldl
			lfmt <- "%Y-%m-%d %H:%M:%S"
			if(!is.null(mldl@mpars$cintrdate)){				   
				print(paste('current time:',format(getCintradate(mldl),format=lfmt) ))
			}
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })



#' for live feed from contracts list
#' @export
  #' @docType methods
  setGeneric(name='gndcs'
  			,function(.Object,itrf=TRUE
					,rollon="volume"
					,skipintraday=FALSE
					,intrd="now",freq=60
					,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg"){
  			standardGeneric('gndcs')})
  #' load  from disk  data from futures using contract sequence list config file
  #'
  #' @aliases gndcs,Wwatchlist,ANY-method
  setMethod('gndcs','Wwatchlist'
		,function(.Object,itrf=TRUE
				,rollon="volume"
				,skipintraday=FALSE
				,intrd="now",freq=60
				,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg"){
			nameObject <- deparse(substitute(.Object))
			if (!is.na( getCinstrument(.Object@mdata))){
		  	mldl <- .Object@mdata
		  	
		  	loadUp2dateContracts(mldl
							  	,rollon=rollon
							  	,skipintraday=skipintraday
							  	,intrd=intrd
							  	,freq=freq
							  	,cslistconf=cslistconf ) 
		 	.Object@mdata <- mldl
		  	cat("CURRENT DATE: "
			  	,strftime(getCdate(.Object@mdata),format="%Y-%m-%d")
			  	,strftime(getCdate(.Object@mdata), format="%A" ) , "\n")
		  	if( itrf){
				gif(.Object)
				cat("Instrument list rewinded to start\n")
		  	}
		  	persist(.Object)
			}else{
		  		stop(paste("[Wwatchlist:gndcs validation]"
				 		 ,"market list data undefined for this wwatchlist."))
			}
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
	  })  

#' @export
  #' @docType methods
  setGeneric(name='gndfcs'
  			,function(.Object, cslist
  					,itrf=TRUE
					,rollon="volume"
					,skipintraday=FALSE){
  			standardGeneric('gndfcs')})
  #'  fetch market data from loaded contract sequences  list
  #'
  #' @aliases gndfcs,Wwatchlist,ANY-method
  setMethod('gndfcs','Wwatchlist'
		,function(.Object, cslist
				,itrf=TRUE
				,rollon="volume"
				,skipintraday=FALSE){
			nameObject <- deparse(substitute(.Object))
			if ( !is.ContractSeqList(cslist) ){
					stop(paste("Wwatchlist:gndfcs validation]"
									,"cslist passed param is not of type ContractSeqList"))
				}
		if (!is.na( getCinstrument(.Object@mdata))){
		  	mldl <- .Object@mdata
		  	
		  	fetchUp2dateContracts(mldl, csl=cslist
							  	,rollon=rollon
							  	,skipintraday=skipintraday) 
		 	.Object@mdata <- mldl
		  	cat("CURRENT DATE: "
			  	,strftime(getCdate(.Object@mdata),format="%Y-%m-%d")
			  	,strftime(getCdate(.Object@mdata), format="%A" ) , "\n")
		  	if( itrf){
				gif(.Object)
				cat("Instrument list rewinded to start\n")
		  	}
		  	persist(.Object)
			}else{
		  		stop(paste("[Wwatchlist:gndfcs validation]"
				 		 ,"market list data undefined for this wwatchlist."))
			}
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
	  })  


## data mode { live ; historic}
 #' @export
 #' @docType methods
setGeneric(name='dmode',function(.Object,mode,startdate,enddate){standardGeneric('dmode')})
 #' sets data mode
 #'
 #'  sets data mode to \code{"live"|"historic"}
 #'
 #' @param Wwatchlist instance
 #' @param more character  with values \code{"live"|"historic"}
 #' @param startdate character. Relevant for mode \code{"historic"}
 #' @param enddate character. Relevant for mode \code{"historic"}
 #' to set last date in data dates sequence
 #' @return void. Wwatchlist instance is modified
 #' @aliases dmode,Wwatchlist,ANY-method
setMethod('dmode','Wwatchlist'
		  ,function(.Object,mode='live',startdate,enddate ){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- .Object@mdata
				   setMode(mldl, mode=mode,startdate=startdate, enddate=enddate)
				   .Object@mdata <- mldl
			persist(.Object)
		   }else{
			  stop(paste("[Wwatchlist:dmode validation]"
						,"market list data undefined for this wwatchlist."))
		   }
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  }
		  )

##data current date
 #' @export
 #' @docType methods
setGeneric(name='dcdate',function(.Object,date,sav=FALSE){standardGeneric('dcdate')})
 #' sets data current date
 #'
 #' mostly appropriate for data mode \code{historic}
 #'
 #' @param Wwatchlist instance
 #' @param date character in format \code{%Y-%m-%d}
 #' @return void. Wwatchlist instance is modified
 #' @aliases dcdate,Wwatchlist,ANY-method
setMethod('dcdate','Wwatchlist'
		  ,function(.Object, date=strftime(Sys.Date()),sav=FALSE ){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- .Object@mdata
				   setCdate(mldl) <- date
				   #add syncinc intraday time if it is set
				   if(!is.null(mldl@mpars$cintrdate)){
				   	syncIntradayDate(mldl)
				   }
				   .Object@mdata <- mldl
				   fmt <- "%Y-%m-%d %A"
				   lfmt <- "%Y-%m-%d %H:%M:%S"
				   print(paste('current date:',format(getCdate(mldl),format=fmt) ))
				   if(!is.null(mldl@mpars$cintrdate)){				   
				   	print(paste('current time:',format(getCintradate(mldl),format=lfmt) ))
				   }
				   if(sav){persist(.Object)}
		   }else{
			  stop(paste("[Wwatchlist:dcdate validation]"
						,"market list data undefined for this wwatchlist."))
		   }
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })

## go watchlist instrument first
 #' @export
 #' @docType methods
setGeneric(name='gif',function(.Object){standardGeneric('gif')})
 #' go to first instrument in the watchlist
 #'
 #' sets current instrument to first instrument in the list
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases gif,Wwatchlist,ANY-method
setMethod('gif','Wwatchlist'
		  ,function(.Object){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   setInstrumentPosition(.Object@mdata, 'first')
				   cat("CURRENT INSTRUMENT: ", getCinstrument(.Object@mdata), "\n")
			
		   }else{
			  stop(paste("[Wwatchlist:gif validation]"
						,"market list data undefined for this wwatchlist."))
		   }
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })
#go watchlist instrument next
 #' @export
 #' @docType methods
setGeneric(name='gin',function(.Object,rechart=FALSE){standardGeneric('gin')})
 #' go to next instrument in the watchlist
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases gin,Wwatchlist,ANY-method
setMethod('gin','Wwatchlist'
		  ,function(.Object,rechart=FALSE){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- .Object@mdata
				   setInstrumentPosition(mldl, 'next')
				   .Object@mdata <- mldl
				   cat("CURRENT INSTRUMENT: ", getCinstrument(.Object@mdata), "\n")
		   }else{
			  stop(paste("[Wwatchlist:gin validation]"
						,"market list data undefined for this wwatchlist."))
		   }
		   if(rechart){
				cc(.Object,npf=2) #rechart
				ps(.Object) #reprint story
			}
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })

#go watchlist instrument previous
 #' @export
 #' @docType methods
setGeneric(name='gip',function(.Object){standardGeneric('gip')})
 #' go to previous instrument in the watchlist
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases gip,Wwatchlist,ANY-method
setMethod('gip','Wwatchlist'
		  ,function(.Object){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- .Object@mdata
				   setInstrumentPosition(mldl, 'previous')
				   .Object@mdata <- mldl
				   cat("CURRENT INSTRUMENT: ", getCinstrument(.Object@mdata), "\n")
			
		   }else{
			  stop(paste("[Wwatchlist:gip validation]"
						,"market list data undefined for this wwatchlist."))
		   }
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		  })

#go watchlist instrument specific
 #' @export
 #' @docType methods
setGeneric(name='gis',function(.Object,instr){standardGeneric('gis')})
 #' go to specified instrument in the watchlist
 #'
 #' @param Wwatchlist instance
 #' @param instr character  instrument ID
 #' @return void. Wwatchlist instance is modified
 #' @aliases gis,Wwatchlist,ANY-method
setMethod('gis','Wwatchlist'
		  ,function(.Object,instr){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- .Object@mdata
				   setSpecificInstrumentPosition(mldl, instr)
				   .Object@mdata <- mldl
				   cat("CURRENT INSTRUMENT: ", getCinstrument(.Object@mdata), "\n")

		   }else{
			  stop(paste("[Wwatchlist:gis validation]"
						,"market list data undefined for this wwatchlist."))
		   }
		   assign(nameObject,.Object,envir=parent.frame())
		   return ( invisible() )
})

#go watchlist instrument by index
 #' @export
 #' @docType methods
setGeneric(name='gi',function(.Object,i=0){standardGeneric('gi')})
 #' go to specified instrument in the watchlist
 #'
 #' @param Wwatchlist instance
 #' @param instr character  instrument ID
 #' @return void. Wwatchlist instance is modified
 #' @aliases gi,Wwatchlist,ANY-method
setMethod('gi','Wwatchlist'
		  ,function(.Object,i=0){
			nameObject <- deparse(substitute(.Object))
		   if (!is.na( getCinstrument(.Object@mdata))){
				   mldl <- .Object@mdata
				   instr <- .Object@orderbook@pfpars$watchinst[i]
				   setSpecificInstrumentPosition(mldl, instr)
				   .Object@mdata <- mldl
				   cat("CURRENT INSTRUMENT: ", getCinstrument(.Object@mdata), "\n")

		   }else{
			  stop(paste("[Wwatchlist:gis validation]"
						,"market list data undefined for this wwatchlist."))
		   }
		   assign(nameObject,.Object,envir=parent.frame())
		   return ( invisible() )
})


#' @export
#' @docType methods
setGeneric(name='ps',function(.Object,tf=getCurtimeframe(.Object),lf=F,n=250,uhist=F,ct=T,tex=F,sig=F)
			{standardGeneric('ps')})
 #' print instrument story
 #'
 #' @param Wwatchlist instance
 #' @param tf character time frame. default value current time frame
 #' @param lf boolean  flag  for long or short format
 #' @param n number of recent days to print instrument story. Defaut value n=250
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases ps,Wwatchlist,ANY-method
setMethod('ps','Wwatchlist'
		  ,function(.Object,tf=getCurtimeframe(.Object),lf=F,n=250,uhist=F,ct=T,tex=F,sig=F){
			  cstory <- getCurstory(.Object)
			  cdate <- getCdate(.Object@mdata)
			  cinstr <- getSinstrument(cstory)
			  cdata <- getCdatedata(.Object@mdata, cinstr, tf)
			  cdfreq	<- getCurtimefrequency(.Object,timeframe=tf)
			  if(tex){
				printlatexStory(cstory,tf=tf,limitdate=cdate,xtsdata=cdata,ndays=n,uhist=uhist,lflag=lf)
			}else{
				printStory(cstory,tf=tf,limitdate=cdate,xtsdata=cdata,ndays=n,uhist=uhist,lflag=lf)
		    }
		  	if(sig){ printDonchian(.Object) } ## print donchian alerts for text version of story
			 
			if(ct){
			  	dfmt <- "%Y-%m-%d"
             	cdt_tz <- base::format(cdate, format="%Z")
                cat("CURRENT DATE: ", strftime(cdate,format=dfmt,tz=cdt_tz)
                                 	,cdt_tz
                                 	,strftime(cdate, format="%A",tz=cdt_tz ), "\n")
			  }
			  return(invisible())
		  })
#' @export
#' @docType methods
setGeneric(name='psc',function(.Object,lf=F,nd=250,nw=250,uhist=F)
			{standardGeneric('psc')})
 #' print combined instrument story daily and weekly artefacts
 #'
 #' @param Wwatchlist instance
 #' @param lf boolean  flag  for long or short format
 #' @param nd number of recent days to print instrument daily artifacts. Defaut value nd=250
 #' @param nw number of recent days to print instrument weekly artifacts. Defaut value nw=250
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases psc,Wwatchlist,ANY-method
setMethod('psc','Wwatchlist'
		  ,function(.Object,lf=F,nd=250,nw=250,uhist=F){
			  cstory <- getCurstory(.Object)
			  cdate <- getCdate(.Object@mdata)
			  cinstr <- getSinstrument(cstory)
			  cdata <- getCdatedata(.Object@mdata, cinstr, "d1")
			  cwdata <- getCdatedata(.Object@mdata, cinstr, "w1")
			  #daily
			  printStory(cstory,tf="d1",limitdate=cdate,xtsdata=cdata,ndays=nd,uhist=uhist,lflag=lf)
			  #weekly
			  printStory(cstory,tf="w1",limitdate=cdate,xtsdata=cwdata,ndays=nw,uhist=uhist,lflag=lf)
			  return(invisible())
		  })



 #' @export
 #' @docType methods
setGeneric(name='pst',function(.Object,uhist=F)
			{standardGeneric('pst')})
 #' print instrument story in technical format
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases pst,Wwatchlist,ANY-method
setMethod('pst','Wwatchlist'
		  ,function(.Object,uhist=F){
			  cstory <- getCurstory(.Object)
			  printtStory(cstory,uhist=uhist)
			  return(invisible())
		  })
#' @export
 #' @docType methods
setGeneric(name='pptr',function(object, ftarg,targ ,pin, pstop, riskpt=0.01, capital=10000, mult=1,cbl=F,cbs=F)
			{standardGeneric('pptr')})
 #' assess risk reward for potential trade
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases pptr,Wwatchlist,ANY-method
setMethod('pptr','Wwatchlist'
		  ,function(object, ftarg,targ,pin,pstop,riskpt=0.01, capital=10000, mult=1,cbl=F,cbs=F){
			if(missing(ftarg) || missing(targ)  ){
			  stop("missing price ftarget or price target param")
			}
			ctf  <- getCurtimeframe(object)
			cstory <- getCurstory(object)
			cinstr <- getSinstrument(cstory)
			cdata <- getCdatedata(object@mdata, cinstr, ctf)
			ctop<- as.numeric(Hi(last(cdata)))
			cbot <-as.numeric(Lo(last(cdata)))
			if(cbl){
				print(paste("Trade Long on break-out on current bar"))
				pin <- ctop
				pstop <- cbot
			}
			if(cbs){
				print(paste("Trade Short on break-down on current bar"))
				pin <- cbot
				pstop <- ctop
			}

			fprr<-abs(pin-ftarg)/abs(pin-pstop)
			rr<-abs(pin-targ)/abs(pin-pstop)
			cap2risk <- capital * riskpt
			psize <- cap2risk / (mult * abs(pin-pstop))
			print(paste("entry=", pin, "; stop=",pstop, ";", "abs risk=", -abs(pin-pstop),";","firstproblem=",ftarg, "; target=",targ
						,"risk per trade@",riskpt ,"; equity=",capital, "; multiplier=", mult))
			print(paste("fpz R:R", "|",formatC(fprr,format='f',digits=2), "|"
					   ," targ R:R", "|",formatC(rr,format='f',digits=2),"|"
					   ,"; fpz abs profit=", abs(pin-ftarg)
					   ,"; targ abs profit=", abs(pin-targ)
					   ,"; pos size:", formatC(psize,format='f',digits=2),"contracts")
				 )
			  return(invisible())
		  })



 #' @export
 #' @docType methods
setGeneric(name='pd',function(.Object,tf=getCurtimeframe(.Object),n=6,all=F)
			{standardGeneric('pd')})
 #' print current instrument  tail of data
 #'
 #' @param Wwatchlist instance
 #' @param tf character time frame. Default value is current time frame
 #' @param n numeric. Number of data points in tail. Default = 6
 #' @param all boolean. If true print data for all instruments in the watchlist.
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases pd,Wwatchlist,ANY-method
setMethod('pd','Wwatchlist'
		  ,function(.Object,tf=getCurtimeframe(.Object),n=6,all=F){
			  ntail <- n
			  mldl <- .Object@mdata
			  if(!all){
			  cinstr <- getCinstrument(mldl)
			  dd <- getCdatedata(mldl, cinstr,tf)
			  cat(paste("ohlc tail data for", cinstr,"timeframe=",tf, "\n"))
			  print(tail(dd,ntail))
			  }else{ ##print data on all instruments in list
				  datinstrs <- unlist ( lapply( mldl, function(x){x$name} ))
				  for( j in 1:length(datinstrs)){
					  cinstr <- datinstrs[j]
					  dd <- getCdatedata(mldl, cinstr,tf)
					  cat(paste("ohlc tail data for", cinstr,"timeframe=",tf, "\n"))
					  print(tail(dd,ntail))
				  }
			  }
		  return(invisible())
})

 #' @export
 #' @docType methods
setGeneric(name='pdi',function(.Object,dfreq=3600,nday=1,n=10)
			{standardGeneric('pdi')})
 #' print current instrument  tail of intraday data
 #'
 #' @param Wwatchlist instance
 #' @param dfreq  numeric data frequency 60 is munute data
 #' @param nday numeric. Number of days of data in tail. Default = 1
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases pdi,Wwatchlist,ANY-method
setMethod('pdi','Wwatchlist'
		  ,function(.Object,dfreq=3600,nday=1,n=10){
			  cinstr <- getCinstrument(getMarketdata(.Object))
			  dd <- getIntraCdatedata(getMarketdata(.Object),iname=cinstr,ndays=nday,dfreq=dfreq)
			  cat(paste("ohlc tail data for", cinstr,"data frequency:",dfreq, "\n"))
			  print(tail(dd,n))
		  return(invisible())
})


  #' @export
  #' @docType methods
  setGeneric(name='pmeta',function(.Object,all=F,n=4,ctime=Sys.time())
		  {standardGeneric('pmeta')})
  #' print meta information for current instrument
  #'
  #' @param Wwatchlist instance
  #' @return void. Wwatchlist instance is unmodified
  #' @aliases pmeta,Wwatchlist,ANY-method
  setMethod('pmeta','Wwatchlist'
		  ,function(.Object,all=F,n=4,ctime=Sys.time()){
			  mldl <- .Object@mdata

			  if(!all){
				  cinstr <- getCinstrument(mldl)
				  printIdatMeta(mldl,cinstr,n=n,ctime=ctime)
			  }else{ ##print data on all instruments in list
				  datinstrs <- unlist ( lapply( mldl@idata, function(x){x$name} ))
				  for( j in 1:length(datinstrs)){
					  cinstr <- datinstrs[j]
					  printIdatMeta(mldl,cinstr,n=n,ctime=ctime)
				  }
			  }
			  return(invisible())
		  })

  #' @export
  #' @docType methods
  setGeneric(name='pliquid',function(object,cdt=T,ctime=Sys.time(),n=10,topdir="~/googledrive/mkdata/dtn/contrseq")
		  {standardGeneric('pliquid')})
  #' print liquidity data from futures contract sequence
  #'
  #'
  #' @param Wwatchlist instance
  #' @param topdir  topdir which stores contract sequences
  #' @return void. Wwatchlist instance is umodified
  #' @aliases pliquid,Wwatchlist,ANY-method
  setMethod('pliquid','Wwatchlist'
		  ,function(object,cdt=T,ctime=Sys.time(),n=10,topdir="~/googledrive/mkdata/dtn/contrseq"){
			  mldl <- object@mdata
			  iname <- getCinstrument(mldl)
			  if(cdt){
				  cdate <- getCdate(mldl)
			  }else{
				  cdate <-ctime
			  }
			  cs <- readcs(iname,topstoredir=topdir)
			  printLiquidity(cs,ctime=cdate,n=n)   # method from ContractSeq class
			  return ( invisible() )
		  })


 #' @export
 #' @docType methods
setGeneric(name='rmdTail',function(.Object,n=1,tf=getCurtimeframe(.Object),sav=FALSE )
			{standardGeneric('rmdTail')})
 #' remove last n points of data from instrument list
 #'
 #' @param Wwatchlist instance
 #' @param n numeric. Number of data points in tail to remove. Default = 1
 #' @return void. Wwatchlist instance is modified
 #' @aliases rmdTail,Wwatchlist,ANY-method
setMethod('rmdTail','Wwatchlist'
		  ,function(.Object,n=1,tf=getCurtimeframe(.Object),sav=FALSE ){
			nameObject <- deparse(substitute(.Object))
			mld <- .Object@mdata
			removeDatatail(mld,rmd=F,n=n,tf=tf)
			qa<-readline("are you sure you with to proceed with deletion?(y/n)")
			if ( qa =="y"){
			   removeDatatail(mld,rmd=T,n=n,tf=tf)
			}else{
				print("operation cancelled")
			}
			.Object@mdata <- mld
			if(sav){persist(.Object)}  #saves to rdat file
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		  })

#############################################################
#
####### wyckooff fact management
#
##############################################################
##isSelect is used in a number of interactive user functions to update artifacts
 #' @docType methods
setGeneric(name='iSelectfact',function(.Object){standardGeneric('iSelectfact')})
 #' interactively select an artifact on a chart
 #'
 #' @param Wwatchlist instance
 #' @return list of artifact's slabel and artifact's date
 #' @keywords internal
 #' @aliases iSelectfact,Wwatchlist,ANY-method
setMethod('iSelectfact','Wwatchlist'
		  ,function(.Object){

			 ctf  	<- getCurtimeframe(.Object)
			 cdfreq <- getCurtimefrequency(.Object)
			 cstory <- getCurstory(.Object)
			 cinstr <- getSinstrument(cstory)
			 graphedinstr <- getChartInstrumentName()
			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:iSelectFact validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }

			 cat('Select bar  of an Artifact:\n ')
			 pbar <- zooompick()
			 cat("selected " , strftime(pbar$t) , " ", pbar$y , "\n")

			 afidx <- findArtifact(cstory, pbar$t, cdfreq, tz=pbar$tz)
			 selres <- as.list(NA)
			 #browser()
			 if(is.na(afidx[1])  ){
				 print(paste("No artefact found at selected",pbar$t, "."))
			 }else{
				 for ( j in 1:length(afidx)){
					fact2sel <- getFact(cstory,afidx[j] )
					print(paste(j , "-", getOwndtimeStr(fact2sel)
								  ,appendStory( fact2sel,lflag=T,uhist=F )
								  ,"df:",getDfrequency(fact2sel) ))
				 }
				 qa<-readline('type selected artifact number (0 - cancel)')
				 if ( as.numeric(qa) %in%  1:length(afidx) ){
					#browser()
					selectid <- afidx[as.numeric(qa)]
					selfact <- getFact(cstory,selectid)
					selres <- list(slabel= getSlabel(selfact)
								  ,dtime=getOwndtimeStr(selfact)
								  ,timeframe=ctf)
				 }else{
					 print(paste("Artifact selection cancelled"))
				 }
			 }
			 return(selres)

		  })

###### place Wyckoff fact
 #' @export
 #' @docType methods
setGeneric(name='pWfct',function(.Object,slb,cand=F,cbt=F,cbb=F,sav=FALSE){standardGeneric('pWfct')})
 #' interactively place a labeled fact on a chart
 #'
 #' @param Wwatchlist instance
 #' @param slb character  labeled artifact's short label
#' @param cand boolean  if TRUE set current Fact as candidate. Default value is FALSE
#' @param cbt,cbb boolean  if TRUE will place a wyckoff fact to the most right bar top or bottom
#' @return void. Wwatchlist instance is modified
 #' @aliases pWfct,Wwatchlist,ANY-method
setMethod('pWfct','Wwatchlist'
		  ,function(.Object,slb,cand=F,cbt=F,cbb=F,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pWfct validation]"
						,"missing chart to place a fact"))
			 }
			if (missing(slb)){
			  stop(paste("[Wwatchlist:pWfct validation]"
						,"missing param slb"))
			}
			wyklbls <-  .Object@wdict@wels[,1]
			if ( is.na( match(slb, wyklbls) )){
					stop(paste("[Wwatchlist:pWfct validation]"
							   ,"bad value for slb param. refer to whelp()"))
			}

			 nameObject <- deparse(substitute(.Object))

			 cstory <- getCurstory(.Object)
			 cinstr <- getSinstrument(cstory)
			 ctf  <- getCurtimeframe(.Object)
			 cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
             tzxts <-indexTZ(cdata)

			 udate <- strftime(getCdate(.Object@mdata),format="%y%b%d")
			 tzval <- indexTZ(cdata)
			 #graphedinstr <- quantmod:::get.chob()[[dev.cur()]]@name
			 graphedinstr <- getChartInstrumentName()
			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pFct validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			 if( cbt || cbb){
				 cat('Selected current bar  for the new Wyckoff fact:\n ')
				 dt <- paste(index(last(cdata)))
				 if (cbt){ yvl <- as.numeric(Hi(last(cdata))) }
				 if (cbb){ yvl <- as.numeric(Lo(last(cdata))) }
				 pbar <- list(t=dt, y=yvl,tz=tzval)
			 }else{
			  cat('Select bar  for the new Wyckoff fact:\n ')
			  pbar <- zooompick()
			 }
			 #browser()
			 cat("selected " , strftime(pbar$t) , " ", pbar$y , "\n")
			 dperiod <- getChartDataPeriodicity()
			 llb <- paste(getWykLongname(.Object@wdict,slb)," - ",getWyksentiment(.Object@wdict,slb),sep="" )
			 wf <- defWfact(cinstr,slb, pbar$t, pbar$y
						  ,llb
						  ,dperiod$units, dperiod$frequency,tzxts)
			 ##storeobject update history
			 apndNote(wf) <- paste("[",udate,"]","u:",slb,sep="")
			if (cand){
			   setIsconfirmed(wf) <- FALSE
			   apndNote(wf)<-":cd?y"
			 }
			 addFact(cstory) <- wf
			 updStory(.Object) <- cstory
			 ## print placement info here show(Artifact)
			 show(wf)
			 cat(paste("added to wstory for", cinstr, "\n"))
			 ##rechart after update
			 cc(.Object,npf=2)
			 ##reprint story
			 #ps(.Object)
			 if(sav) { persist(.Object) }  #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
		   })

   ##update wyckoff fact label
   #' @export
   #' @docType methods
   setGeneric(name='swLbl',function(.Object,slbl,dtime,sav=FALSE){standardGeneric('swLbl')})
   #'  toggle label position from bar top to bottom and vice versa for Wyckoff facts
   #'
   #' @param cb boolean. Update label position on current bar
   #' @return void. Wwatchlist instance is modified
   #' @aliases swLbl,Wwatchlist,ANY-method
   setMethod('swLbl','Wwatchlist'
		   ,function(.Object,slbl,dtime,sav=FALSE){
			   ##validation
			   if ( dev.cur() == 1 ){
				   stop(paste("[Wwatchlist:swLbl validation]"
								   ,"missing chart to place a fact"))
			   }


			   nameObject <- deparse(substitute(.Object))
			   okTypes <- .Object@wdict@wels[,1]
			   cstory <- getCurstory(.Object)
			   cinstr <- getSinstrument(cstory)
			   ctf  	<- getCurtimeframe(.Object)
			   cdfreq	<- getCurtimefrequency(.Object)
			   cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
						   if(missing(slbl)){
				   stop(paste("[Wwatchlist:swLbl validation]"
								   ,"missing slabel param"))
						   }
			   if(missing(dtime) ){
				   #cat('Select the bar  with a Wyckoff fact to change label position:\n ')
				   #sfactPars <- iSelectfact(.Object)
				   #browser()
				   #dtime <-sfactPars$t
				   #slbl <- sfactPars$slabel
				   dtime  <- paste(index(last(cdata)))
			   }
						   #browser()
			   af2dIdx <- getArtifact(cstory, slbl,dtime, cdfreq, tz=indexTZ(cdata))
			   if ( !is.na(af2dIdx )){
				   fact2mod <- getFact(cstory,af2dIdx)
				   ##validation on types
				   if(is.na(match(getSlabel(fact2mod),okTypes))){
					   stop(cat("[Wwatchlist:swLbl validation]"
									   ," bad fact type"
									   ," allowed types:",okTypes,"\n"))
				   }
				   yval <-getYval(fact2mod)
				   btop <- as.numeric(Hi(cdata[dtime]))
				   bbot <- as.numeric(Lo(cdata[dtime]))
				   if(abs(yval - btop) < abs(yval - bbot)){
					   nyval <- bbot
				   }else{
					   nyval <- btop
				   }
				   setYval(fact2mod)<-nyval
				   setFact(cstory, af2dIdx) <- fact2mod
			   }else{
				   print(paste("Artifact", slbl,"at", dtime
										   , "at timeframe", ctf
										   , "not found."))
			   }
			   updStory(.Object) <- cstory
			   ##rechart after modif
			   cc(.Object,npf=2)
			   if(sav){ persist(.Object)}  #saves to rdat file
			   assign(nameObject,.Object,envir=parent.frame())
			   return(invisible())
		   })

##update wyckoff fact label
 #' @export
 #' @docType methods
setGeneric(name='uWfct',function(.Object,slbo,slbn,dtime,cand=F,sav=FALSE){standardGeneric('uWfct')})
 #' update a labeled fact
 #'
 #' change fact's short label from old value to new value
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases uWfct,Wwatchlist,ANY-method
setMethod('uWfct','Wwatchlist'
		  ,function(.Object,slbo, slbn,dtime,cand=F,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:uWfct validation]"
						,"missing chart to place a fact"))
			 }
			if ( missing(slbo)  || missing(slbn) ){
			  stop(paste("[Wwatchlist:uWfct validation]"
						,"missing param slbo or slbn"))
			}
			wyklbls <-  .Object@wdict@wels[,1]
			if ( is.na( match(slbo, wyklbls) )){
					stop(paste("[Wwatchlist:uWfct validation]"
							   ,"bad value for slbo param. refer to whelp()"))
			}
			if ( is.na( match(slbn, wyklbls) )){
					stop(paste("[Wwatchlist:uWfct validation]"
							   ,"bad value for slbn param. refer to whelp()"))
			}

			 nameObject <- deparse(substitute(.Object))
			 cstory <- getCurstory(.Object)
			 cinstr <- getSinstrument(cstory)
			 ctf  <- getCurtimeframe(.Object)
			 cdfreq	<- getCurtimefrequency(.Object)
			 udate <- strftime(getCdate(.Object@mdata),format="%y%b%d")
             cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			if(missing(dtime)){
			 cat('Select the bar  with a fact to update:\n ')
			  pbar <- zooompick()
			  #browser()
			  dtime <-pbar$t
			}
			af2dIdx <- getArtifact(cstory, slbo,dtime, cdfreq,tz=indexTZ(cdata))
			if ( !is.na(af2dIdx )){
				 fact2mod <- getFact(cstory,af2dIdx)
				 #browser()
				 qstr<-paste( "are you sure you want to modify artifact"
							  , getDfrequency(fact2mod)
							  , getOwndtimeStr(fact2mod)
							  , appendStory( fact2mod,lflag=T,uhist=T)
							  , "(y/n)" )
				 qa<-readline(qstr)
				 if ( qa =="y"){
				   if(cand){
					 if(getIsconfirmed(fact2mod)  )	{
						 setIsconfirmed(fact2mod)<-FALSE
						 apndNote(fact2mod)<-paste("[",udate,"]",":cd?y",sep="")
					 }
					 else{
						 setIsconfirmed(fact2mod)<-TRUE
						 apndNote(fact2mod)<-paste("[",udate,"]",":cd?n",sep="")
					 }
				   }else{
					setSlabel(fact2mod) <- slbn
					setLlabel(fact2mod) <- getWykLongname(.Object@wdict, slbn)
					apndNote(fact2mod) <- paste("[",udate,"]","u:",slbn,sep="")
				   }
				   setFact(cstory, af2dIdx) <- fact2mod
				   print(paste(appendStory( fact2mod,lflag=T,uhist=F),"DONE." ))
				}else{print("Wyckoff Update cancelled.")}
			}else{
			  print(paste("Artifact", slbo,"at", dtime
						  , "at timeframe", ctf
						  , "not found."))
			}
			updStory(.Object) <- cstory
			if(sav){ persist(.Object) } #saves to rdat file
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		   })
##update interactive  wyckoff fact label
 #' @export
 #' @docType methods
setGeneric(name='upWfct',function(.Object,slbn,cand=F,sav=FALSE){standardGeneric('upWfct')})
 #' interactively update a labeled fact
 #'
 #' change fact's short label from old value to new value
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases upWfct,Wwatchlist,ANY-method
setMethod('upWfct','Wwatchlist'
		  ,function(.Object,slbn,cand=F,sav=FALSE){
			##validation
			wyklbls <-  .Object@wdict@wels[,1]

			 nameObject <- deparse(substitute(.Object))
			 cstory <- getCurstory(.Object)
			 cinstr <- getSinstrument(cstory)
			 ctf  <- getCurtimeframe(.Object)
			 sfactPars <- iSelectfact(.Object)
			 if(cand){
				 slbn <- sfactPars$slabel
			 }
			 ##call non interactive version of wfact update
			 uWfct(.Object, sfactPars$slabel, slbn, sfactPars$dtime,cand=cand,sav=sav)
			##rechart after update
			 cc(.Object,npf=2)
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		  })

  ##place chart contents fact
  #' @docType methods
  setGeneric(name='pCfct',function(.Object,type,cont,dtime,cbt=F,cbb=F)
						  {standardGeneric('pCfct')})
 #' place String based fact on a chart
 #'
 #' @param Wwatchlist instance
 #' @param type character  type of Fact {BVW, BPS, TRG,ACT,BIA} Required param
 #' @param cont character. Contents of  string based fact
 #' @param dtime character. datetime of the fact to be placed (optional)
 #' @return void. Wwatchlist instance is modified
 #' @aliases pCfct,Wwatchlist,ANY-method
  setMethod('pCfct','Wwatchlist'
		  ,function(.Object,type,cont,dtime,cbt=F,cbb=F){
			  ##validation
			  if ( dev.cur() == 1 ){
				  stop(paste("[Wwatchlist:pCfct validation]"
								  ,"missing chart to place a fact"))
			  }
			  okTypes<-c("NOB", "BVW", "BPR","HYP")
			  if(is.na(match(type,okTypes))){
				  stop(paste("[Wwatchlist:pCfct validation]"
								  ,"invalid type param"))

			  }
			  nameObject <- deparse(substitute(.Object))

			  cstory <- getCurstory(.Object)
			  cinstr <- getSinstrument(cstory)
			  ctf  <- getCurtimeframe(.Object)
			  cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			  tzxts <- indexTZ(cdata)
			  graphedinstr <- getChartInstrumentName()
			  udate <- strftime(getCdate(.Object@mdata),format="%y%b%d")


			  if( length(grep(cinstr ,graphedinstr))==0 ) {
				  stop(paste("[Wwatchlist:pNob validation]"
								  ,"current instrument"
								  , cinstr ,"does not correspond to charted instrument"))
			  }
			  if( cbt || cbb){
				  cat('Selected current bar  for the new char fact:\n ')
				  dt <- paste(index(last(cdata)))
				  if (cbt){ yvl <- as.numeric(Hi(last(cdata))) }
				  if (cbb){ yvl <- as.numeric(Lo(last(cdata))) }
				  pbar <- list(t=dt, y=yvl, tz=tzxts)
			  }else if( !missing(dtime)){
				  dt <- dtime
				  yvl<- as.numeric(Lo(cdata[dtime]))
				  pbar <-list(t=dt,y=yvl,tz=tzxts)
			  }
			  else{
				  cat('Select bar  for the new char based fact:\n ')
				  pbar <- zooompick()
			  }
			  tzval <- pbar$tz
			  cat("selected " , strftime(pbar$t) , " ", pbar$y , "\n")
			  dp <- getChartDataPeriodicity()
              
			  selfact <- function(atype,inst,cont,dt,yval,dunits,dfreq ){
				  o <- switch(atype,
						  "NOB" = defNobfact(inst,cont,dt,yval,dunits,dfreq,tzval)
						  ,"BVW" =  defBvfact(inst,cont,dt,yval,dunits,dfreq,tzval)
						  ,"BPR" =  defBpfact(inst,cont,dt,yval,dunits,dfreq,tzval)
						  ,"HYP" =  defHfact(inst,cont,dt,yval,dunits,dfreq,tzval)
				  )
				  return(o)
			  }

			  cf <- selfact(type,cinstr,cont, pbar$t, pbar$y,dp$units, dp$frequency)
			  ##storeobject update history
			  apndNote(cf) <- paste("[",udate,"]","u:",sep="")

			  addFact(cstory) <- cf
			  updStory(.Object) <- cstory
			  ## print placement info here show(Artifact)
			  show(cf)
			  cat(paste("added to wstory for", cinstr, "\n"))
			  ##reprint story
			  ps(.Object)
			  assign(nameObject,.Object,envir=parent.frame())
			  return(invisible())
		  })

  #' @export
  #' @docType methods
  setGeneric(name='pVfct',function(.Object,cont,dtime,cbt=F,cbb=F,sav=FALSE)
		  {standardGeneric('pVfct')})
  #' place bar view (interpetation) fact on a chart (will appear in instrument story)
  #'
  #' @param Wwatchlist instance
  #' @param cont character. Contents of  string based fact
  #' @param dtime character. datetime of the fact to be placed (optional)
  #' @return void. Wwatchlist instance is modified
  #' @aliases pVfct,Wwatchlist,ANY-method
  setMethod('pVfct','Wwatchlist'
		  ,function(.Object,cont,dtime,cbt=F,cbb=F,sav=FALSE){
			  nameObject <- deparse(substitute(.Object))
			  type<-"BVW"
			  pCfct(.Object,type,cont=cont,dtime=dtime,cbt=cbt,cbb=cbb)
			  if(sav){ persist(.Object) } #saves to rdat file
			  assign(nameObject,.Object,envir=parent.frame())
			  return(invisible())
		  })

  #' @export
  #' @docType methods
  setGeneric(name='pBfct',function(.Object,cont,dtime,cbt=F,cbb=F,sav=FALSE)
		  {standardGeneric('pBfct')})
  #' place bar properties  fact on a chart (will appear in instrument story)
  #'
  #' @param Wwatchlist instance
  #' @param cont character. Contents of  string based fact
 #' @param dtime character. datetime of the fact to be placed (optional)
 #' @return void. Wwatchlist instance is modified
  #' @aliases pVfct,Wwatchlist,ANY-method
  setMethod('pBfct','Wwatchlist'
		  ,function(.Object,cont,dtime,cbt=F,cbb=F,sav=FALSE){
			  nameObject <- deparse(substitute(.Object))
			  type<-"BPR"
			  pCfct(.Object,type,dtime=dtime,cont=cont,cbt=cbt,cbb=cbb)
			  if(sav){ persist(.Object) } #saves to rdat file
			  assign(nameObject,.Object,envir=parent.frame())
			  return(invisible())
		  })

  #' @export
  #' @docType methods
  setGeneric(name='pHfct',function(.Object,cont,dtime,cbt=F,cbb=F,sav=FALSE)
		  {standardGeneric('pHfct')})
 #' place  hyp fact on a chart (will appear in instrument story)
 #'
 #' @param Wwatchlist instance
 #' @param cont character. Contents of  string based fact
 #' @param dtime character. datetime of the fact to be placed (optional)
 #' @return void. Wwatchlist instance is modified
 #' @aliases pHfct,Wwatchlist,ANY-method
  setMethod('pHfct','Wwatchlist'
		  ,function(.Object,cont,dtime,cbt=F,cbb=F,sav=FALSE){
			  nameObject <- deparse(substitute(.Object))
			  type<-"HYP"
			  pCfct(.Object,type,cont=cont,dtime=dtime,cbt=cbt,cbb=cbb)
			  if(sav){ persist(.Object) } #saves to rdat file
			  assign(nameObject,.Object,envir=parent.frame())
			  return(invisible())
		  })

  ##place chart contents fact
  #' @export
  #' @docType methods
  setGeneric(name='pAfct',function(.Object,wc,cand=F,bc,vc,hc,dtime,cbt=F,cbb=F,sav=FALSE)
		  {standardGeneric('pAfct')})
  #' place optionally one of each facts: Wyckoff, BPR, BVW, HYP
  #'
  #' @param Wwatchlist instance
  #' @param wc character  if specified, wil place a  wyckoff fact
  #' @param cand Boolean. if TRUE set isCandidate status to Wyckoff fact
  #' @param bc character. if specified, will place a BPR  fact
  #' @param vc character. if specified, will place a BVW  fact
  #' @param hc character. if specified, will place a HYP  fact
  #' @param dtime character. datetime of the fact to be placed (optional)
  #' @param cbt,cbb boolean  if TRUE will place a wyckoff fact to the most right bar top or bottom
  #' @return void. Wwatchlist instance is modified
  #' @aliases pAfct,Wwatchlist,ANY-method
  setMethod('pAfct','Wwatchlist'
		  ,function(.Object,wc,cand=F,bc,vc,hc,dtime,cbt=F,cbb=F,sav=FALSE){
			  ##validation
			  if ( dev.cur() == 1 ){
				  stop(paste("[Wwatchlist:pAfct validation]"
								  ,"missing chart to place a fact"))
			  }
			  nameObject <- deparse(substitute(.Object))

			  cstory <- getCurstory(.Object)
			  cinstr <- getSinstrument(cstory)
			  ctf  <- getCurtimeframe(.Object)
			  cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			  tzxts <- indexTZ(cdata)
			  graphedinstr <- getChartInstrumentName()
			  udate <- strftime(getCdate(.Object@mdata),format="%y%b%d")


			  if( length(grep(cinstr ,graphedinstr))==0 ) {
				  stop(paste("[Wwatchlist:pNob validation]"
								  ,"current instrument"
								  , cinstr ,"does not correspond to charted instrument"))
			  }
			  if( cbt || cbb){
				  cat('Selected current bar  for the new char fact:\n ')
				  dt <- paste(index(last(cdata)))
				  if (cbt){ yvl <- as.numeric(Hi(last(cdata))) }
				  if (cbb){ yvl <- as.numeric(Lo(last(cdata))) }
				  pbar <- list(t=dt, y=yvl,tz=tzxts)
			  }else if( !missing(dtime)){
				  dt <- dtime
				  yvl<- as.numeric(Lo(cdata[dtime]))
				  pbar <-list(t=dt,y=yvl,tz=tzxts)
			  }
			  else{
				  cat('Select bar  for the new char based fact:\n ')
				  pbar <- zooompick()
			  }
			  tzval <- pbar$tz
			  cat("selected " , strftime(pbar$t) , " ", pbar$y , "\n")
			  dp <- getChartDataPeriodicity()
			  dunits<- dp$units
			  dfreq <-dp$frequency
			  dt <-pbar$t
			  yval <-pbar$y
			  ##place optionally Wfact
			  prc<-FALSE
			  if(!missing(wc)){
				  prc<-TRUE
				  wdict <- getWdict(.Object)
				  wkside <- getWykside(wdict)
				  if(is.na(match(wc,wkside[,1]))){
					  stop(paste("[Wwatchlist:pAfct validation]"
									  ,"bad value for Wyckoff type. Refer to whelp()"))
				  }
				  llb <- paste(getWykLongname(wdict,wc)," - ",getWyksentiment(wdict,wc),sep="" )
				  wf <- defWfact(cinstr,wc, dt, yval,llb,dunits, dfreq,tzval)
				  ##store object update history
				  apndNote(wf) <- paste("[",udate,"]","u:",wc,sep="")
				  if (cand){
					  setIsconfirmed(wf) <- FALSE
					  apndNote(wf)<-":cd?y"
				  }
				  addFact(cstory) <- wf
				  show(wf)
				  cat(paste("added to wstory for", cinstr, "\n"))
			  }
			  ## place optionally BPR fact
			  if(!missing(bc)){
				  if(!is.character(bc)){
					  stop(paste("[Wwatchlist:pAfct validation]"
									  ,"BPR contents should be a character"))
				  }
				  bf <- defBpfact(cinstr,bc,dt,yval,dunits,dfreq,tzval)
				  apndNote(bf) <- paste("[",udate,"]","u:",sep="")
				  addFact(cstory) <- bf
				  show(bf)
				  cat(paste("added to wstory for", cinstr, "\n"))
			  }
			  ## place optionally BVW fact
			  if(!missing(vc)){
				  if(!is.character(vc)){
					  stop(paste("[Wwatchlist:pAfct validation]"
									  ,"BVW contents should be a character"))
				  }
				  vf <- defBvfact(cinstr,vc,dt,yval,dunits,dfreq,tzval)
				  apndNote(vf) <- paste("[",udate,"]","u:",sep="")
				  addFact(cstory) <- vf
				  show(vf)
				  cat(paste("added to wstory for", cinstr, "\n"))
			  }
			  ## place optionally HYP fact
			  if(!missing(hc)){
				  if(!is.character(hc)){
					  stop(paste("[Wwatchlist:pAfct validation]"
									  ,"HYP contents should be a character"))
				  }
				  hf <- defHfact(cinstr,hc,dt,yval,dunits,dfreq,tzval)
				  apndNote(hf) <- paste("[",udate,"]","u:",sep="")
				  addFact(cstory) <- hf
				  show(hf)
				  cat(paste("added to wstory for", cinstr, "\n"))
			  }
			  updStory(.Object) <- cstory
			  ##reprint story
			  ps(.Object)
			  ##rechart after update
			  if(prc){ cc(.Object,npf=2) }
              if(sav) { persist(.Object)}
			  assign(nameObject,.Object,envir=parent.frame())
			  return(invisible())
		  })



##place note fact (soon  to be deprecated
 #' @export
 #' @docType methods
setGeneric(name='pNob',function(.Object,cont,cbt=F,cbb=F,sav=FALSE){standardGeneric('pNob')})
 #' place Note fact on a chart
 #' (soon to be deprecated)
 #' registers a note fact in an instrument story
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases pNob,Wwatchlist,ANY-method
setMethod('pNob','Wwatchlist'
		  ,function(.Object,cont,cbt=F,cbb=F,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pNob validation]"
						,"missing chart to place a fact"))
			 }

			 nameObject <- deparse(substitute(.Object))

			 cstory <- getCurstory(.Object)
			 cinstr <- getSinstrument(cstory)
			 ctf  <- getCurtimeframe(.Object)
			 cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			 graphedinstr <- getChartInstrumentName()
             tzxts <-indexTZ(cdata)

			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pNob validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			 if( cbt || cbb){
				 cat('Selected current bar  for the new Wyckoff fact:\n ')
				 dt <- paste(index(last(cdata)))
				 if (cbt){ yvl <- as.numeric(Hi(last(cdata))) }
				 if (cbb){ yvl <- as.numeric(Lo(last(cdata))) }
				 pbar <- list(t=dt, y=yvl, tz=tzxts)
			 }else{
			  cat('Select bar  for the new Nota bene fact:\n ')
			  pbar <- zooompick()
			 }
			 cat("selected " , strftime(pbar$t) , " ", pbar$y , "\n")
			 dperiod <- getChartDataPeriodicity()
			 #browser()
			 nbf <- defNobfact(cinstr,content=cont, oDateTime=pbar$t, yvalue=pbar$y
							  ,dunit=dperiod$units, dfreq=dperiod$frequency, tzvalue=pbar$tz)
			 addFact(cstory) <- nbf
			 updStory(.Object) <- cstory
			 ## print placement info here show(Artifact)
			 show(nbf)
			 cat(paste("added to wstory for", cinstr, "\n"))
			 ##reprint story
			 ps(.Object)
			 if(sav){ persist(.Object) } #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
		   })




   ##update wyckoff fact label
   #' @export
   #' @docType methods
   setGeneric(name='apndFct',function(.Object,acont,type,dtime,sav=FALSE){standardGeneric('apndFct')})
   #' append contents  for facts BVW, BPR, HYP
   #'
   #' @param acont character. string to append
   #' @param type character. Fact Type. allowed vlaues: BVW, BPR,HYP (Optional)
   #' @param dtime character. Fact's datetime (Optional)
   #' @return void. Wwatchlist instance is modified
   #' @aliases uWfct,Wwatchlist,ANY-method
   setMethod('apndFct','Wwatchlist'
		   ,function(.Object,acont,type,dtime,sav=FALSE){
			   ##validation
			   if ( dev.cur() == 1 ){
				   stop(paste("[Wwatchlist:apndFct validation]"
								   ,"missing chart to place a fact"))
			   }
			   okTypes <- c("BVW","BPR","HYP","NOB")

			   nameObject <- deparse(substitute(.Object))
			   cstory <- getCurstory(.Object)
			   cinstr <- getSinstrument(cstory)
			   ctf  <- getCurtimeframe(.Object)
			   cdfreq	<- getCurtimefrequency(.Object)

			   #udate <- strftime(getCdate(.Object@mdata),format="%y%b%d")
			   dd <- getCdatedata(.Object@mdata, cinstr,ctf)
		 	   udate <- strftime( index(last(dd)), format="%Y-%m-%d" )


			   if(missing(dtime) || missing(type)){
				   cat('Select the bar  with a fact to update:\n ')
				   sfactPars <- iSelectfact(.Object)
				   #browser()
				   dtime <-sfactPars$t
				   type <- sfactPars$slabel
			   }
			   af2dIdx <- getArtifact(cstory, type,dtime, cdfreq, tz=indexTZ(dd))
			   if ( !is.na(af2dIdx )){
				   fact2mod <- getFact(cstory,af2dIdx)
				   ##validation on types
				   if(is.na(match(getSlabel(fact2mod),okTypes))){
					   stop(cat("[Wwatchlist:apndFct validation]"
									   ," bad fact type"
									   ," allowed types:",okTypes,"\n"))
				   }
				   qstr<-paste( "are you sure you want to modify artifact"
						   , getDfrequency(fact2mod)
						   , getOwndtimeStr(fact2mod)
						   , appendStory( fact2mod,lflag=T,uhist=T)
						   , "(y/n)" )
				   qa<-readline(qstr)
				   if ( qa =="y"){
					  fcont <- getContent(fact2mod)
					  slb <- getSlabel(fact2mod)
					  if(slb == "HYP"){
						 fcont <- paste(fcont,";","[",udate,"]:",acont,sep="")
					  }else{
						 fcont <- paste(fcont,";",acont)
					  }
					  setContent(fact2mod)<-fcont
					  apndNote(fact2mod) <- paste("[",udate,"]","apnd:",sep="")#logging
					  setFact(cstory, af2dIdx) <- fact2mod
					  print(paste(appendStory( fact2mod,lflag=T,uhist=F),"DONE." ))
				   }else{print("Append cancelled.")}
			   }else{
				   print(paste("Artifact", type,"at", dtime
								   , "at timeframe", ctf
								   , "not found."))
			   }
			   updStory(.Object) <- cstory
			   if(sav){persist(.Object)}  #saves to rdat file
			   assign(nameObject,.Object,envir=parent.frame())
			   return(invisible())
		   })


##place creek level fact
 #' @export
 #' @docType methods
setGeneric(name='pLvl',function(.Object,offset=0,stopdate,sav=FALSE){standardGeneric('pLvl')})
 #' place level fact on  a chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases pLvl,Wwatchlist,ANY-method
setMethod('pLvl','Wwatchlist'
		  ,function(.Object,offset=0, stopdate,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pLvl validation]"
						,"missing chart to place a fact"))
			 }
			 nameObject <- deparse(substitute(.Object))

			 cstory <- getCurstory(.Object)
			 ctf  <- getCurtimeframe(.Object)
			 cinstr <- getSinstrument(cstory)
			 graphedinstr <- getChartInstrumentName()

			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pLvl validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			 cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			 cat('Select bar  for level:\n ')

			 #browser()
			 lpbar <- zooompick()
			 cat("selected " , strftime(lpbar$t) , " ", lpbar$y , "\n")
          
          	 #browser()
			 lvl <- defLevel(iname=cinstr, leftDtime=lpbar$t
			 	            ,leftyval=lpbar$y, xtsdat=cdata, loffset=offset)
             
			 if(!missing(stopdate)){
			   cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			   fmt <- "%Y-%m-%d"
			   stoptime <- as.POSIXct(stopdate , format=fmt )
			   setStopshowLevel(lvl,cdata) <- stoptime
			 }

			 addFact(cstory) <- lvl
			 updStory(.Object) <- cstory
			 ## print placement info here show(Artifact)
			 show(lvl)
			 cat(paste("added to wstory for", cinstr,"\n"))
			 ##rechart after update
			 cc(.Object,npf=2)

			 if(sav){persist(.Object)}  #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
		   })
###place ice level
 #' @export
 #' @docType methods
setGeneric(name='pIcl',function(.Object,offset=0,stopdate,sav=FALSE){standardGeneric('pIcl')})
 #' place ice level fact on  a chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases pIcl,Wwatchlist,ANY-method
setMethod('pIcl','Wwatchlist'
		  ,function(.Object,offset=0,stopdate,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pIcl validation]"
						,"missing chart to place a fact"))
			 }
			 nameObject <- deparse(substitute(.Object))

			 cstory <- getCurstory(.Object)
			 ctf  <- getCurtimeframe(.Object)
			 cinstr <- getSinstrument(cstory)
			 graphedinstr <- getChartInstrumentName()

			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pIcl validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			 cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			 cat('Select bar  for level:\n ')
			 lpbar <- zooompick()
			 cat("selected " , strftime(lpbar$t) , " ", lpbar$y , "\n")

			 lvl <- defIceLevel(cinstr, leftDtime=lpbar$t, leftyval=lpbar$y,xtsdat=cdata, loffset=offset )

			 if(!missing(stopdate)){
			   cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			   fmt <- "%Y-%m-%d"
			   stoptime <- as.POSIXct(stopdate , format=fmt )
			   setStopshowLevel(lvl,cdata) <- stoptime
			 }

			 addFact(cstory) <- lvl
			 updStory(.Object) <- cstory
			 ## print placement info here show(Artifact)
			 show(lvl)
			 cat(paste("added to wstory for", cinstr,"\n"))
			 ##rechart after update
			 cc(.Object,npf=2)

			 if(sav){persist(.Object)}  #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
		   })
##place half level
 #' @export
 #' @docType methods
setGeneric(name='pHfl',function(.Object,offset=0,sav=FALSE){standardGeneric('pHfl')})
 #' place half level fact on  a chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases pHvl,Wwatchlist,ANY-method
setMethod('pHfl','Wwatchlist'
		  ,function(.Object,offset=0,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pHvl validation]"
						,"missing chart to place a fact"))
			 }
			 nameObject <- deparse(substitute(.Object))

			 cstory <- getCurstory(.Object)
			 ctf  <- getCurtimeframe(.Object)
			 cinstr <- getSinstrument(cstory)
			 graphedinstr <- getChartInstrumentName()

			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pHvl validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			 cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			 cat('Select bottom  bar  for  half level:\n ')
			 lpbar <- zooompick()
			 cat("selected " , strftime(lpbar$t) , " ", lpbar$y , "\n")
			 cat('Select top  bar  for  half level:\n ')
			 rpbar <- zooompick()

			 lvl <- defHalfLevel(cinstr
			 	                ,lpbar$t, lpbar$y
			 	                ,rpbar$t, rpbar$y
			 	                ,cdata
			 	                ,loffset=offset )

			 addFact(cstory) <- lvl
			 updStory(.Object) <- cstory
			 ## print placement info here show(Artifact)
			 show(lvl)
			 cat(paste("added to wstory for", cinstr,"\n"))
			 ##rechart after update
			 cc(.Object,npf=2)
			 if(sav){ persist(.Object) }  #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
		   })

##place trend
 #' @export
 #' @docType methods
setGeneric(name='pTrl',function(.Object,sav=FALSE){standardGeneric('pTrl')})
 #' place trend fact on  a chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases pTrl,Wwatchlist,ANY-method
setMethod('pTrl','Wwatchlist'
		  ,function(.Object,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pTrl validation]"
						,"missing chart to place a fact"))
			 }
			 nameObject <- deparse(substitute(.Object))

			 cstory <- getCurstory(.Object)
			 ctf  <- getCurtimeframe(.Object)
			 cinstr <- getSinstrument(cstory)
			 graphedinstr <- getChartInstrumentName()

			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pTrl validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			 cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			 cat('Select bar  for trend left point:\n ')
			 lpbar <- zooompick()
			 cat('Select bar  for trend right point:\n ')
			 rpbar <- zooompick()
			 cat("left selected " , strftime(lpbar$t) , " ", lpbar$y , "\n")
			 cat("right selected " , strftime(rpbar$t) , " ", rpbar$y , "\n")

			 tr <- defTrend(cinstr, lpbar$t, lpbar$y, rpbar$t, rpbar$y,cdata )
			 addFact(cstory) <- tr
			 updStory(.Object) <- cstory
			 ## print placement info here show(Artifact)
			 show(tr)
			 cat(paste("added to wstory for", cinstr, "\n"))
			 ##rechart after update
			 cc(.Object,npf=2)
			 if(sav){persist(.Object)}  #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
		   })
#place trend channel
 #' @export
 #' @docType methods
setGeneric(name='pChn',function(.Object,sav=FALSE){standardGeneric('pChn')})
 #' place trend fact on  a chart to for a trend channel
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases pChn,Wwatchlist,ANY-method
setMethod('pChn','Wwatchlist'
		  ,function(.Object,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pChn validation]"
						,"missing chart to place a fact"))
			 }
			 nameObject <- deparse(substitute(.Object))
			 cstory <- getCurstory(.Object)
			 cinstr <- getSinstrument(cstory)
			 ctf  <- getCurtimeframe(.Object)
			 cdfreq	<- getCurtimefrequency(.Object)
			 graphedinstr <- getChartInstrumentName()

			if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pChn validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			print(paste("Select a base trend artifact for trend channel"))
			sfactPars <- iSelectfact(.Object)
			dtime <- sfactPars$dtime
			af2dIdx <- getArtifact(cstory, 'TRL', dtime, cdfreq,tz=indexTZ(cdata))
			if ( is.na(af2dIdx )){
			  stop(paste("[Wwatchlist:pChn validation]"
						,"specified param "
						, dtime ,"does not contain a trend artifact"
						, "with time frame", ctf))
			}
			basetr <- getFact(cstory,af2dIdx)
			cat('Select one bar  for trend channel:\n ')
			cpbar <- zooompick()
			chantr  <- defChan(basetr, cpbar$t, cpbar$y, cdata)
			addFact(cstory) <- chantr
			updStory(.Object) <- cstory

			 ## print placement info here show(Artifact)
			 show(chantr)
			 cat(paste("added to wstory for", cinstr, "\n"))
			 ##rechart after update
			 cc(.Object,npf=2)
			 if(sav){persist(.Object)}  #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
		   })

##place zone level fact (for intraday mostly)
 #' @export
 #' @docType methods
setGeneric(name='pZon',function(.Object,offset=0,scf=1,sav=FALSE){standardGeneric('pZon')})
 #' place level fact on  a chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases pZon,Wwatchlist,ANY-method
setMethod('pZon','Wwatchlist'
		  ,function(.Object,offset=0,scf=1,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pZon validation]"
						,"missing chart to place a fact"))
			 }
			 nameObject <- deparse(substitute(.Object))

			 cstory <- getCurstory(.Object)
			 ctf  <- getCurtimeframe(.Object)
			 cinstr <- getSinstrument(cstory)
			 graphedinstr <- getChartInstrumentName()

			 if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pLvl validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			 #cdata <- getCdatedata(.Object@mdata, cinstr, ctf)
			 cdata <- getChartSubsetOhlcData()
			 cat('Select bar  for zone low  end:\n ')
			 llbar <- zooompick()
			 cat("selected " , llbar$t, " ", llbar$y , "\n")
			 cat('Select bar  for zone high  end:\n ')
			 lhbar <- zooompick()
			 cat("selected " , lhbar$t, " ", lhbar$y , "\n")
          
          	 #browser()
          	 zone <- defZone(iname=cinstr, loDtime=llbar$t, hiDtime=lhbar$t
          	 				,loYval=llbar$y, hiYval=lhbar$y
          	 				,xtsdat=cdata, ndayscale=scf, loffset=offset )


			 addFact(cstory) <- zone
			 updStory(.Object) <- cstory
			 ## print placement info here show(Artifact)
			 show(zone)
			 cat(paste("added to wstory for", cinstr,"\n"))
			 ##rechart after update
			 if (periodicity(cdata)$frequency < 86400){
			 	itcs(.Object,npf=3)
			 }else{
			 	cc(.Object,npf=2) #daily and up data
			} 	
			 if(sav){persist(.Object)}  #saves to rdat file
			 assign(nameObject,.Object,envir=parent.frame())
			 return(invisible())
})

##place limit showRightDatetime (for intraday mostly)
 #' @export
 #' @docType methods
setGeneric(name='stopIzon',function(.Object,id=c(1),nday=-1,f=FALSE,sav=FALSE){standardGeneric('stopIzon')})
 #' place level fact on  a chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases stopIzon,Wwatchlist,ANY-method
setMethod('stopIzon','Wwatchlist'
		  ,function(.Object,id=c(1),nday=-1,f=FALSE,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pZon validation]"
						,"missing chart to modify facts"))
			 }
			 nameObject <- deparse(substitute(.Object))
			 cstory <- getCurstory(.Object)
			 ctf  <- getCurtimeframe(.Object)
             curdata <- getChartSubsetOhlcData()
             chartTime <- index(last(curdata))
             #all zones before charting time
             allzn <- getAllIntradayTypefacts(cstory, 'ZoneLine',time_limit=chartTime)             
             revid <- sort(id,decreasing=TRUE)
             tidx <- length(allzn)+1 -revid  # an array of indices             
             fltzn <- allzn[tidx]
             dayoffset <- -1
             if(sign(nday)== 1) { dayoffset <- 1 } 

  			 prevday <- chartTime - ( as.numeric(format(chartTime,format='%H'))*3600 
                   + as.numeric(format(chartTime,format='%M'))*60
                   + as.numeric(format(chartTime,format='%S'))  + 1) 
  			       + 86400* sign(nday)*(abs(nday)+dayoffset)
            pdaytt <- prevday  + sign(nday) * 86400  * ( abs(nday) + dayoffset )
  			lfmt <- "%Y-%m-%d %H:%M:%S" 
            subprd <- paste("::",base::format(pdaytt,format=lfmt),sep="")
            limit_time <- index(last(curdata[subprd]))
            if (sign(nday)== 1){ limit_time <- pdaytt } 
            
            #browser()
            ## confirmation
            if(f) isGo <- TRUE else isGo<-FALSE
	   		if(!isGo){
	   		  print(fltzn)	
			  qstr<-paste( "are you sure you want to change right date for above zones to"
						  , format(limit_time,format=lfmt)
						  , "(y/n)?" )
			  qa<-readline(qstr)
			}else{
				qa<-"y"
			}
			if ( qa =="y"){
			   isGo <- TRUE
			   print("Confirmed.")
			 }else{
			 	isGo <- FALSE
			 	print("Cancelled.")
			 }
			##action 
            if(isGo){
            	modzn <- lapply(fltzn,function(x,limtime,ndays){
            						setShowRightDatetime(x)<-limtime
            						return(x)}
                			,limtime=limit_time,ndays=nday)
            
            	for(j in 1:length(modzn) ){
            		idx <- getFactIndex(cstory,modzn[[j]])
        			setFact(cstory,idx) <- modzn[[j]]
            	}
				updStory(.Object) <- cstory
            
				##rechart after update
				itcs(.Object,npf=3)
            }
			if(sav){persist(.Object)}  #saves to rdat file
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
})

#place line break right  (stop showing level to the right ( or remove it)
 #' @export
 #' @docType methods
setGeneric(name='pLepr',function(.Object,putl=T,sav=FALSE){standardGeneric('pLepr')})
 #' place right stop show limit for level facts  and trend facts
 #'
 #' @param putl Boolean.  if False a previously placed limit is erased
 #' @return void. Wwatchlist instance is modified
 #' @aliases pLepr,Wwatchlist,ANY-method
setMethod('pLepr','Wwatchlist'
		  ,function(.Object,putl=T,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:pLepr validation]"
						,"missing chart to place a fact"))
			 }

			nameObject <- deparse(substitute(.Object))
			cstory <- getCurstory(.Object)
			cinstr <- getSinstrument(cstory)
			ctf  <- getCurtimeframe(.Object)
			cdfreq	<- getCurtimefrequency(.Object)
			cdata <- getCdatedata(.Object@mdata, cinstr, ctf)

			graphedinstr <- getChartInstrumentName()

			if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:pLepr validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }
			fmt <- "%Y-%m-%d %H:%M:%S"
			if(cdfreq >=86400) { fmt <- "%Y-%m-%d" }

			#interactive select level or trend fact
			print(paste("select line artifact to modify its right limit"))
			sfactPars <- iSelectfact(.Object)
			slbl <- sfactPars$slabel
			#browser()
			if (is.null(slbl)){
			  stop(paste("[Wwatchlist:pLepr validation]"
						,"selected label should have value TRL or LVL"))
			}
			if (      slbl != 'TRL'
				   && slbl != 'LVL'
				   && slbl != 'ICL'
				   && slbl != 'HFL' ){
			  stop(paste("[Wwatchlist:pLepr validation]"
						,"param slbl should have value TRL, LVL, ICL or HFL"))
			}
			dtime <-  sfactPars$dtime
			af2dIdx <- getArtifact(cstory, slbl, dtime, cdfreq,tz=indexTZ(cdata))
			if ( is.na(af2dIdx )){
			  stop(paste("[Wwatchlist:pLepr validation]"
						,"specified param "
						, dtime ,"does not contain an artifact"
						,slbl, "with time frame", ctf))
			}
			linefact <- getFact(cstory,af2dIdx)
			show(linefact)
			if(putl ){
				 cat("Select bar at right to stop showing the line artifact \n")
				 rbar <- zooompick()
				 cat("bar selected " , strftime(rbar$t) , " ", rbar$y , "\n")
				 
				 stoptime <- as.POSIXct(rbar$t , format=fmt, tz=rbar$tz )
			}
			#browser()
			 if( slbl == 'LVL' || slbl == 'HFL' || slbl == 'ICL' ){
			     if(putl) setStopshowLevel(linefact,cdata) <- stoptime
			     else  		setStopshowLevel(linefact,cdata) <- as.POSIXct(NA)		  
			}
			 if( slbl == 'TRL'){
			   if(putl) { setStopshowTrend(linefact,cdata) <- stoptime }
			   else     { setStopshowTrend(linefact,cdata) <- as.POSIXct(NA) }
			}
			setFact(cstory,af2dIdx) <- linefact

			updStory(.Object) <- cstory
			show(linefact)
			cat(paste("updated in wstory for", cinstr, "\n"))
			##rechart after update
			cc(.Object,npf=2)
			if(sav){persist(.Object)}  #saves to rdat file
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		   })

#update  HaflLevel fact
 #' @export
 #' @docType methods
setGeneric(name='upHfl',function(.Object,sav=FALSE){standardGeneric('upHfl')})
 #' place right stop show limit for level facts  and trend facts
 #'
 #' @param putl Boolean.  if False a previously placed limit is erased
 #' @return void. Wwatchlist instance is modified
 #' @aliases upHfl,Wwatchlist,ANY-method
setMethod('upHfl','Wwatchlist'
		  ,function(.Object,sav=FALSE){
			##validation
			 if ( dev.cur() == 1 ){
			  stop(paste("[Wwatchlist:upHfl validation]"
						,"missing chart to place a fact"))
			 }

			nameObject <- deparse(substitute(.Object))
			cstory <- getCurstory(.Object)
			cinstr <- getSinstrument(cstory)
			ctf  <- getCurtimeframe(.Object)
			cdfreq	<- getCurtimefrequency(.Object)
			cdata <- getCdatedata(.Object@mdata, cinstr, ctf)

			graphedinstr <- getChartInstrumentName()

	  if( length(grep(cinstr ,graphedinstr))==0 ) {
			  stop(paste("[Wwatchlist:upHfl validation]"
						,"current instrument"
						, cinstr ,"does not correspond to charted instrument"))
			 }

			#interactive select level or trend fact
			msg <- "select HFL artifact to modify" 
			print(msg)
			sfactPars <- iSelectfact(.Object)
			slbl <- sfactPars$slabel
			#browser()
			if (is.null(slbl)){
			  stop(paste("[Wwatchlist:upHfl validation]"
						,"selected label should have value HFL"))
			}
			if ( slbl != 'HFL' ){
			  stop(paste("[Wwatchlist:upHfl validation]"
						,"param slbl should have value HFL "))
			}
			dtime <-  sfactPars$dtime
			af2dIdx <- getArtifact(cstory, slbl, dtime, cdfreq,tz=indexTZ(cdata))
			if ( is.na(af2dIdx )){
			  stop(paste("[Wwatchlist:upHfl validation]"
						,"specified param "
						, dtime ,"does not contain an artifact"
						,slbl, "with time frame", ctf))
			}
			linefact <- getFact(cstory,af2dIdx)
			show(linefact)
			cat('Select bottom  bar  for  half level:\n ')
			lpbar <- zooompick()
			cat("selected " , strftime(lpbar$t) , " ", lpbar$y , "\n")
			cat('Select top  bar  for  half level:\n ')
			rpbar <- zooompick()

			nlinefact <- defHalfLevel(cinstr, lpbar$t, lpbar$y, rpbar$t, rpbar$y ,  cdata )
			setFact(cstory,af2dIdx) <- nlinefact
			updStory(.Object) <- cstory
			show(linefact)
			cat(paste("updated in wstory for", cinstr, "\n"))
			##rechart after update
			cc(.Object,npf=2)
			if(sav){persist(.Object)}  #saves to rdat file
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		   })




#### deleteFact method
 #' @export
 #' @docType methods
setGeneric(name='rmFact',function(.Object,slbl,dtime, ctf,f=F,sav=FALSE){standardGeneric('rmFact')})
 #' remove an artifact from  the instrument story
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases rmFact,Wwatchlist,ANY-method
setMethod('rmFact','Wwatchlist'
		  ,function(.Object,slbl,dtime,ctf,f=F,sav=FALSE){
			##validation
			alltypes <- c( .Object@wdict@types[,1] , .Object@wdict@wels[,1] )
			if ( is.na( match(slbl, alltypes) )){
					stop(paste("[Wwatchlist:dFact validation]"
							   ,"bad value for slb param. refer to whelp()"))
			}
			nameObject <- deparse(substitute(.Object))
			if(missing(ctf)){
			 ctf  <- getCurtimeframe(.Object)
			}
			cdfreq	<- getCurtimefrequency(.Object,ctf)

			cstory <- getCurstory(.Object)
			cinstr <- getSinstrument(cstory)
			cdata <- getCdatedata(.Object@mdata, cinstr, ctf)


			if(missing(dtime)){
			 cat('Select the bar  of the fact to be removed:\n ')
			  pbar <- zooompick()
			  dtime <-pbar$t
			}
			af2dIdx <- getArtifact(cstory, slbl,dtime, cdfreq, tz=indexTZ(cdata))
			#browser()
			if ( !is.na(af2dIdx )){
				 fact2del <- getFact(cstory,af2dIdx)
				 if(!f){
				  qstr<-paste( "are you sure you want to delete artifact"
							  , getDfrequency(fact2del)
							  , getOwndtimeStr(fact2del)
							  , appendStory( fact2del,lflag=T,uhist=T)
							  , "(y/n)?" )
				  qa<-readline(qstr)
				}else{
					qa<-"y"
				}
				if ( qa =="y"){
				   delFact(cstory, af2dIdx)
				   print("Deleted.")
				 }else{print("Delete cancelled.")}
			}else{
			  print(paste("Artifact", slbl,"at", dtime
						  , "at timeframe", ctf
						  , "not found."))
			}
			updStory(.Object) <- cstory
			if(sav){persist(.Object)}  #saves to rdat file
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		   })
##interactive delete fact
 #' @export
 #' @docType methods
setGeneric(name='rmpFact',function(.Object,sav=FALSE){standardGeneric('rmpFact')})
 #' intreactively remove an artifact from  chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases rmpFact,Wwatchlist,ANY-method
setMethod('rmpFact','Wwatchlist'
		  ,function(.Object,sav=FALSE){
			 nameObject <- deparse(substitute(.Object))
			 cstory <- getCurstory(.Object)
			 cinstr <- getSinstrument(cstory)
			 ctf  <- getCurtimeframe(.Object)
			 sfactPars <- iSelectfact(.Object)
			 ##call non interactive version of wfact update
			 rmFact(.Object, sfactPars$slabel, sfactPars$dtime,ctf, sav=sav)
			 ##rechart after update
			 cc(.Object,npf=2)

			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		  })


##change daily, weekly timezone in xts objects in mldata
 #' @export
 #' @docType methods
setGeneric(name='setDailydataTZ',function(.Object,tzval='UTC',rebuild=FALSE,sav=FALSE){standardGeneric('setDailydataTZ')})
 #' intreactively remove an artifact from  chart
 #'
 #' @return void. Wwatchlist instance is modified
 #' @aliases setDailydataTZ,Wwatchlist,ANY-method
setMethod('setDailydataTZ','Wwatchlist'
		  ,function(.Object,tzval='UTC',rebuild=FALSE,sav=FALSE){
			nameObject <- deparse(substitute(.Object))
			mldl <- getMarketdata(.Object)
			setInterdayTZ(mldl,tz=tzval,rebuild=rebuild)
			setMarketdata(.Object) <- mldl
			if(sav){persistMdata(.Object)}  #saves Mldata to its own rdat file
											#can take long time for Mldata with intraday data

			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		  })



 #' @export
 #' @docType methods
setGeneric(name='wldhelp',function(.Object){standardGeneric('wldhelp')})
#' @aliases wldhelp,Wwatchlist,ANY-method
setMethod('wldhelp','Wwatchlist'
		  ,function(.Object){
			  cat("#############################################################\n")
			  cat("*** Wwatchlist load and saving functions ***\n")
			  cat("#############################################################\n")
			  cat("lwl('~/googledrive/cdat/wls/wlistname.rdat',verbose=F) ::\t load wwatchlist from rdat, if a separate market data rdat exists, it will use it \n")
			  cat("persist(o,withmarketdata=F,verbose=F) ::\t save wwatchlist  to o@rdatfile rdat, default option without market data \n")
			  cat("persistMdata(o,verbose=T) ::\t save a market data for wwatchlist in a separate rdat file \n")
			  cat("#############################################################\n")
			  cat("*** Attach Market data functions ***\n")
			  cat("#############################################################\n")
			  cat('gndcs(o,rollon,skipintraday,cslistconf) ::\t for live mode load updated contract seq list (futures markets )\n')
			  cat('gndfcs(o,cslist,rollon) ::\t for live mode fetch loaded contract seq list (futures markets )\n')
			  cat('dmode(o,mode,enddate) ::\t set market data mode live or historic'
				  ,'(specify enddate for historic mode ) \n')
			  cat('dcdate(o,date) ::\t set current date (Applicable for mode historic) \n')
			  cat('rmdTail(o,n,tf) ::\t remove n last data points in each instrument of a watchlist\n')
			  cat("setDailydataTZ(o,tzval='UTC',rebuild=T,sav=F) ::\t change timezone on daily, weekly xts data with index rebuilding\n")

			  cat("#############################################################\n")
			  cat("*** Move around market data functions ***\n")
			  cat("#############################################################\n")
			  cat('gnd(o) ::\t go to next day market data (for live mode download latest data)\n')
			  cat('gndc(o) ::\t go to next day market data and rechart current instrument\n')
			  cat('gnw(o) ::\t go to enxt week market data (available only in historic data mode)\n')
			  cat("stfw1(o), stfd1(o), sfth1(o) ::\t set current time frame to w1,d1,h1 \n")
			  cat("gif(o), gin(o), gip(o) ::\t set current instrument from "
				  ," market data list first, next previous \n")
			  cat("gis(o,instr) ::\t set specific instrument as current instrument from market data list\n")
			  cat("pd(o,n=6) ::\t print tail of market data for n last bars\n")
			  cat("pdi(o,dfreq=3600,nday=1) ::\t print tail of intraday market data of specified frequency\n")
      
			  cat("#############################################################\n")
			  cat("*** Market data administration functionsp ***\n")
			  cat("#############################################################\n")
			  cat('addWatchGroup(o,ngrp, pos) ::\t add new instrument group to  groupsordered vector'
				  ,'in market data  after position pos  \n')
			  cat('addWatchInstrument(o,name,tkr,typ,lbl,grp)::\t  add new instrument to watch list in market data'
				  ,'by specifying name, ticker, type, label and its group \n')
			  cat('rmWatchInstrument(o,name)::\t delete instrument from watch list. '
				  ,'All associated facts will be deleted!\n')
			  cat('bulkreplaceWyklbl(o,oslb,nslb,nllb) ::\t Bulk replace defined facts'
				  ,'from old short label to new short label and  long label \n')

})

 
 #' @export
 #' @docType methods
setGeneric(name='wfhelp',function(.Object){standardGeneric('wfhelp')})
#' @aliases wfhelp,Wwatchlist,ANY-method
setMethod('wfhelp','Wwatchlist'
		  ,function(.Object){
		  cat('######## Wyckoff artifacts manipulation #######\n')
			cat('------------- Placing artefacts ---------\n')
			  cat('pWfct(o,slbl,cand=F,cbt=F,cbb=F) ::\t place labeled fact in current time frame,'
				  ,'cand - optional if TRUE set fact as candidate to be confirmed\n'
				  ,'\t\tif cbt or cbb set to TRUE, a fact will be non-interactively placed to current bar\n')
			  cat('swLbl(o,slbl,dtime) ::\t switch Wyckoff label from bar top to bottom. if dtime missing, modify fact on last bar\n')
			  cat('pBfct(o,content,cbt=F,cbb=F) ::\t place bar props fact in current time frame with note in content param\n'
			  ,'\t\tif cbt or cbb set to TRUE, a fact will be non-interactively placed to current bar\n')
			  cat('pVfct(o,content,cbt=F,cbb=F) ::\t place bar view fact in current time frame with note in content param\n'
			  ,'\t\tif cbt or cbb set to TRUE, a fact will be non-interactively placed to current bar\n')
			  cat('pHfct(o,content,cbt=F,cbb=F) ::\t place Hypo fact in current time frame with note in content param\n'
			  ,'\t\tif cbt or cbb set to TRUE, a fact will be non-interactively placed to current bar\n')
			  cat('pNob(o,content,cbt=F,cbb=F) ::\t place Note fact in current time frame with note in content param\n'
					  ,'\t\tif cbt or cbb set to TRUE, a note fact will be non-interactively placed to current bar\n')
			 cat('pAfct(o,wc,cand=F,bc,vc,hc,dtime,cbt=F,cbb=F) ::\t place one or few facts: Wyckoff, BPR,BVW,HYP\n'
			  ,'\t\tif cbt or cbb set to TRUE, fact(s) will be non-interactively placed to a current bar\n')
			  cat('pLvl(o,anote) ::\t place  Creek Level fact in current time frame (anote optional annotation)\n')
			  cat('pIcl(o,anote) ::\t place  ICE Level fact in current time frame (anote optional annotation)\n')
			  cat('pHfl(o,anote) ::\t place  Half Level fact in current time frame (anote optional annotation)\n')
			  cat('pTrl(o) ::\t place  Trend  in current time frame\n')
			  cat('pLepr(o,putl=T) ::\t place  (or remove) show Right Limit for TRL, LVL,HFL \n')
			  cat('pChn(o,dtime) ::\t place  Trend  Channel , dtime is the ownTimedate field of existing trend fact  \n')
			 cat('------------- Updating artefacts ---------\n')
			  cat('upHfl(o)  ::\t  update  existing half level artifact \n')   
			  cat('uWfct(o,slblo, slbn, dtime,cand=F ) ::\t  Modify wfact from old wyckoff label to new wyckoff label\n')
			  cat('upWfct(o,slbn ,cand=F) ::\t  Interactively modify wfact from old wfact label to new wyckoff label\n'
			  ,'\t\tif cand=TRUE, toggles isCandidate state of the wyckoff fact\n')
			 cat('apndFct(o,acont,type,dtime) ::\t Append contents for BPR,BVW and HYP facts\n')
			 cat('------------- Removing artefacts ---------\n')
			  cat('rmFact(o,slbl,dtime,ctf) ::\t  Delete fact of any type from current story\n')
			  cat('rmpFact(o) ::\t  Interactive delete fact of any type from current story\n')

			 cat('------------- Print Wyckoff story ---------\n')
			  cat('ps(o,tf,lf,n=250) ::\t print current instrument story'
				 ,'optional tf specify timeframe, lf=F/T long/short date format\n'
				 ,'\t\toptionally specify number of last days to the print story within a period \n')
			  cat('pst(o) ::\t print technical details for current instrument story\n')
			  cat("############## Show Market data functions ###############\n")
			 cat('------------- Print OHLC data ---------\n')
			  cat('pd(o,tf) ::\t print tail of ohlc data for current intstrument.'
				  ,' Optional tf param \n')
			  cat('pmeta(o) ::\t print meta info for current intstrument data (splits, dividents)\n')
			  cat('pliquid(o,ctime,topdir) ::\t print liquidy  info for current intstrument contrcts sequence \n')
			 cat('------------- Potential Trade risk reward ---------\n')
			  cat('pptr(o,fptarg,ptarg ,pin, pstop, riskpt=0.01, capital=10000, mult=1,cbl=F,cbs=F) ::\t print risk'
			   ,' rewards\n\t\t for specified potential trade entry (fptarg = first problem zone, ptarg = trade target)\n'
				 ,'\t\t (optional cbl=T / cbs=T  prints infor on long/short trade placed on last  bar)\n')
	
})
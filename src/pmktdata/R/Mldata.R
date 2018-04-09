#' Market data for a list of financial instruments
#'
#' S4 container which can hold daily and  | or intraday market data for several
#' data providers. Contains methods to manage the list of instruments and to manage the
#' associated market data
#' @rdname Mldata-class
#' @name Mldata
#' @exportClass Mldata
setClass(Class='Mldata'
				 ,representation(idata='list'
												,imetadata='data.frame'
												,groupsordered='character'
												,dataprovider='character'
												,curmktidx='numeric'
												,mode='character'
												,dseq='POSIXct'
												,enddate='POSIXct'
												,cdate='POSIXct'
												,mpars='list'
											#	,histd1='numeric'  # to be depr
											#	,histh1='numeric'  # to be depr
												)
				 ,prototype(    idata=list()
											 ,imetadata=as.data.frame(NA)
											 ,groupsordered=as.character(NA)
											 ,dataprovider='yahoo'
											 ,curmktidx=0
											 ,mode='historic'
											 ,dseq=as.POSIXct(NA)
											 ,enddate=Sys.time()
											 ,cdate=as.POSIXct(NA)
											 ,mpars=list()
										#	 ,histd1=10000
										#	 ,histh1=31
									 )
				 )

##user-friend function for constructor

#' Initializes Mldata instrument list from a csv file
#'
#' @export
instrlistdatacsv <-function(csvfile, grpsorted
													,mode='live'
													,enddate=strftime(Sys.Date()), startdate=strftime(Sys.Date()-10000 )
													,dsrc='yahoo', dld=T )
	{
		 if ( !file.exists(csvfile)){
			 stop(paste("[Mldata:instrlistdatacsv validation]"
									,csvfile,"not found"))
		 }
		 if (    mode != 'live'
					&& mode != 'historic')
		 {
				 stop(paste("[Mldata:instrlistdatacsv validation]"
									 ,"mode param should be either live or historic"))
		 }

			 ldef <- read.csv(csvfile,skip=1,header=FALSE)
			 instrvec <- as.character(ldef[,1])
			 tkrvec <- as.character(ldef[,2])
			 typvec <- as.character(ldef[,3])
			 lblvec <- as.character(ldef[,4])
			 grpvec <- as.character(ldef[,5])

			#check grpsorted has all elements in grpvec
			mdl <-  instrlistdata( instrvec, tkrvec, typvec, lblvec ,grpvec,grpsorted
																 ,mode=mode, enddate=enddate, startdate=startdate
																 ,dsrc=dsrc,dld=dld)

	return(mdl)
	}






#another constructor
#' Initializes Mldata instrument list from a  vectors with config data
#'
#' @export
instrlistdata <-function(instrvec,tickervec,typevec,lblvec,grpvec,grpsorted
												,mode='live'
												,enddate=strftime(Sys.Date()), startdate=strftime(Sys.Date()-10000)
												,dsrc='yahoo', dld=T)
	{

		##validation
	 if (  length(instrvec) != length(tickervec)
				|| length(instrvec) != length(typevec)
				|| length(instrvec) != length(lblvec) ){
			 stop(paste("[Mldata:instrlistdata validation]"
								 ,"vectors of instruments, corresponding tickers annd instrument type"
								 ,"are of different lengths"))
		 }
	 okdsrcvals <- c("yahoo","tblox","dtn")
	 if( is.na(match(dsrc,okdsrcvals)))
	 #if (    dsrc != 'yahoo' && dsrc != 'tblox' && dsrc != 'dtn')
		 {
			 stop(paste("[Mldata:instlistdata validation]","bad value for dsrc param"))
		 }
	 o <- new('Mldata')

		##initiate instrument's list
		mdata <- matrix(ncol=5, byrow=F
									 ,data=c(instrvec,tickervec,typevec,lblvec,grpvec))
		dfr <- as.data.frame(mdata, stringsAsFactors=FALSE)
		colnames(dfr) <- c('IDNAM', 'TKR', 'TYP', 'LBL' ,'GRP')
		##sort according to double column

	 dfr <- dfr[with(dfr, order(as.integer(factor(GRP,grpsorted)), IDNAM)), ]


		o@imetadata <- dfr
		o@groupsordered <- grpsorted
		o@curmktidx <- 1

		o@dataprovider <- dsrc
		#set mode
		if( mode == 'live') {    setMode(o, mode='live',startdate=startdate, enddate=enddate) }
		if( mode == 'historic') {    setMode(o, mode='historic',startdate=startdate,enddate=enddate ) }

	 ##initial data download
	 if (dld){
		iniDataDld(o,startdate=startdate)
	 }
	return(o)
 }





### biz methods
#' @export
#' @docType methods
setGeneric(name='getCinstrument',function(object){standardGeneric('getCinstrument')})
#' @aliases getCinstrument,Mldata,ANY-method
setMethod('getCinstrument','Mldata'
					,function(object){
							dfr <- object@imetadata
							if (is.na(dfr[1,1])) {  return( as.character(NA)) }
							posid <- object@curmktidx
							grpsorted <- object@groupsordered
							sortedidx <- with(dfr, order(as.integer(factor(GRP,grpsorted)), IDNAM))
							residx <- sortedidx[posid]
						return ( object@imetadata$IDNAM[residx] )
					})

### biz methods
#' @export
#' @docType methods
setGeneric(name='getInstrumentLabel',function(object,iname){standardGeneric('getInstrumentLabel')})
#' @aliases getInstrumentLabel,Mldata,ANY-method
setMethod('getInstrumentLabel','Mldata'
					,function(object,iname){
							dfr <- object@imetadata
							if (is.na(dfr[1,1])) {  return( as.character(NA)) }
							iidx <- match ( iname, dfr$IDNAM )
							if(!is.na(iidx)){
									lbl <- dfr$LBL[iidx]
							}else{
									lbl <- as.character(NA)
							}
							return ( lbl )
					})

#' @export
#' @docType methods
setGeneric(name='getSortedinstruments',function(object){standardGeneric('getSortedinstruments')})
#' @aliases getSortedinstruments,Mldata,ANY-method
setMethod('getSortedinstruments','Mldata'
					,function(object){
							dfr <- object@imetadata
							if (is.na(dfr[1,1])) {  return( as.character(NA)) }
							grpsorted <- object@groupsordered
							sortedidx <- with(dfr, order(as.integer(factor(GRP,grpsorted)), IDNAM))
						return ( object@imetadata$IDNAM[sortedidx] )
					})


#' @export
#' @docType methods
setGeneric(name='setInstrumentPosition',function(.Object,pos){standardGeneric('setInstrumentPosition')})
#' @aliases setInstrumentPosition,Mldata,ANY-method
setMethod('setInstrumentPosition','Mldata'
					,function(.Object,pos){
						 ##validation
						 if ( (  pos != 'first'
									&& pos != 'last'
									&& pos != 'next'
									&& pos != 'previous'
									)){
										 stop(paste("[Mldata:setInstrumentPosition validation]"
																		," bad value of pos param"))
						 }
						 nameObject <- deparse(substitute(.Object))
						 imetanames <- .Object@imetadata$IDNAM
						 imkts <- unlist(lapply(.Object@idata , function(x){x$name } ))
						 nmk <- length(imetanames)

						 posid <- .Object@curmktidx
						 posid[ pos %in% 'first' ] <- 1
						 posid[ pos %in% 'last' ] <- nmk
						 posid[ pos %in% 'next' ] <-  if(posid == nmk){ 1 }else{ posid +1}
						 posid[ pos %in% 'previous' ] <- if(posid ==1 ){nmk}else{posid-1}

						 .Object@curmktidx <- posid

						 assign(nameObject,.Object,envir=parent.frame())
						 return(invisible())

					 })
#' @export
#' @docType methods
setGeneric(name='setSpecificInstrumentPosition',function(.Object,idname){standardGeneric('setSpecificInstrumentPosition')})
#' @aliases setSpecificInstrumentPosition,Mldata,ANY-method
setMethod('setSpecificInstrumentPosition','Mldata'
					,function(.Object,idname){
						 ##validation
						 if (is.na(match(idname, .Object@imetadata$IDNAM ))){
										 stop(paste("[Mldata:setSpecificInstrumentPosition validation]"
																,"idname param is missing from imetadata"))
						 }
						 nameObject <- deparse(substitute(.Object))
						 posidx <- match(idname, .Object@imetadata$IDNAM )
						 .Object@curmktidx <- posidx
						 assign(nameObject,.Object,envir=parent.frame())
						 return(invisible())
					 })

#' @export
#' @docType methods
setGeneric(name='getMode',function(object){standardGeneric('getMode')})
#' @aliases getMode,Mldata,ANY-method
setMethod('getMode','Mldata'
					,function(object){
						 return ( object@mode)
					})

#' @export
#' @docType methods
setGeneric(name='setMode',function(.Object
									,mode
									,startdate=as.POSIXct('1900-01-01',tz='UTC')
	                      			,enddate=as.POSIXct(strftime(Sys.time(),format='%Y-%m-%d'),tz='UTC'))
	                      			{standardGeneric('setMode')})
#' @aliases setMode,Mldata,ANY-method
setMethod('setMode','Mldata'
					,function(.Object
						       		,mode='live'
						       		,startdate=as.POSIXct('1900-01-01',tz='UTC')
						            ,enddate=as.POSIXct(strftime(Sys.time(),format='%Y-%m-%d'),tz='UTC'))
					{
						 ##validation
					if (   mode != 'live'
							&& mode != 'historic')
					{
						 stop(paste("[Mldata:setMode validation]"
												,"mode param can have values live or historic "))
					}
					
					if (missing(startdate)){
						startdate=as.POSIXct('1900-01-01',tz='UTC') 
						print(paste('Mldata:setMode using default value for startdate', startdate))
					}
					if (missing(enddate)){
				        enddate=as.POSIXct(strftime(Sys.time(),format='%Y-%m-%d'),tz='UTC')
						print(paste('Mldata:setMode using default value for enddate', enddate))
					}
					mtz <-'UTC' #sequence will be in UTC
					dfmt <- "%Y-%m-%d"
					nameObject <- deparse(substitute(.Object))
					##internal function
					is.on.weekend <-function(reftime){
						reftimecl <- as.POSIXlt(reftime)
					return ( reftimecl$wday %in% c(0,6))
					}
					if (mode == 'historic'){
					 .Object@mode <- 'historic'
					 edate  <-  as.POSIXct(strftime( enddate, format=dfmt), tz=mtz)
					 #edate <-  edate + 3600 * 23 + 60*59 + 59   ## make sure date ends  on last second of day
					 .Object@enddate <- edate
					 }
					if (mode == 'live'){
						.Object@mode <- 'live'
						cdt <- Sys.time()
						edate <-  as.POSIXct(strftime( cdt, format=dfmt), tz=mtz)
						#edate <-  edate + 3600 * 23 + 60*59 + 59   ## make sure date ends  on last second of day
						.Object@enddate  <- edate
						.Object@cdate   <- edate
					}
						#set dseq for both modes live and historic
						sdate <- as.POSIXct(startdate, format=dfmt, tz=mtz)
						fseq <- seq( from=sdate, to=edate, by=86400 )
						fseq <- fseq[!is.on.weekend(fseq)]
						.Object@dseq <- fseq
				 if (mode == 'historic'){
						 ##set cdate as first element of dseq
						.Object@cdate   <- fseq[ length(fseq) - 250 ]  # 1 year from historic enddate
				 }

						 assign(nameObject,.Object,envir=parent.frame())
						 return(invisible())
					 })



#' @export
#' @docType methods
setGeneric(name='getCdate',function(object){standardGeneric('getCdate')})
#' @aliases getCdate,Mldata,ANY-method
setMethod('getCdate','Mldata'
					,function(object){ return ( object@cdate )
			})

#' @export
#' @docType methods
setGeneric(name='getCintradate',function(object){standardGeneric('getCintradate')})
#' @aliases getCintradate,Mldata,ANY-method
setMethod('getCintradate','Mldata'
					,function(object){
					 if (!is.null(object@mpars$cintrdate)){
					 	return ( object@mpars$cintrdate$idate )	
					 }else{
					 	return (NULL)
					 }					 
			})
#' @export
#' @docType methods
setGeneric(name='getCintraTZ',function(object){standardGeneric('getCintraTZ')})
#' @aliases getCintraTZ,Mldata,ANY-method
setMethod('getCintraTZ','Mldata'
					,function(object){
					 if (!is.null(object@mpars$cintrdate)){
					 	return ( object@mpars$cintrdate$itz )	
					 }else{
					 	return (NULL)
					 }					 
			})




#' @export
#' @docType methods
setGeneric(name='setCdate<-'
							 ,function(object,value){standardGeneric('setCdate<-')})
# set current date in Mldata

# sets a specifed date in Mldata sequence of dates
# @return returns  a modified \code{Mldata} instance
# @name setCdate
#' @aliases setCdate<-,Mldata,ANY-method
setReplaceMethod('setCdate'
				 ,'Mldata'
				 ,function(object,value){
						##validate input
						if ( object@mode == "live"){
							 stop(paste("[Mldata:setCdate validation]"
								,"method applicable only to data mode historic"))
						}
						cinstr <- getCinstrument(object);
						fd1<- "%Y-%m-%d" 
						isValueIntraday <- TRUE
						nH <- as.numeric(strftime(value,format='%H')) 
						nM <- as.numeric(strftime(value,format='%M')) 
						nS <- as.numeric(strftime(value,format='%S')) 
						if ( nH == 0 && nM == 0 && nS == 0){
								 isValueIntraday <- FALSE
						}
						inames <-  unlist(lapply(object@idata , function(x){ x$name } ))
						iidx <- match(cinstr, inames)
						xtsd1 <- object@idata[[iidx]]$d1
						
						utcd1 <- as.POSIXct(strftime(index(xtsd1), format=fd1), tz='UTC')

						
						tedate <-  strftime( value, format=fd1)
						

						if (object@mode == 'historic'){ # find closest value in dseq
								#browser()
								#compute unconditionally cdate for tf=d1
								tidx <- match( tedate, strftime(index(xtsd1),format=fd1) )
								#attempte to find and assign the closest date
								if(is.na(tidx)){
									tidx <- match (  min(abs( as.POSIXct(tedate,tz="UTC")- utcd1)),
										                 abs( as.POSIXct(tedate,tz="UTC")- utcd1))
								}

								#tidx <- match( min( abs(object@dseq-tedate) )  #do not stick to dseq
								#	                ,abs(object@dseq - tedate))
								
                                if(!is.na(tidx)){
                                	object@cdate <- index(xtsd1[tidx])  
							    }else{
							    	object@cdate <- index(xtsd1[1])
							    }
						 }
						 
						 return (object)
				 })

#' @export
#' @docType methods
setGeneric(name='syncIntradayDate',function(.Object)
	                      			{standardGeneric('syncIntradayDate')})
#' @aliases syncIntradayDate,Mldata,ANY-method
setMethod('syncIntradayDate','Mldata'
			,function(.Object)
			{
				nameObject <- deparse(substitute(.Object))

				if( is.null(.Object@idata[[1]]$intraday) ) {
					 stop(paste("[Mldata:setIntradayTZ validation]"
								,"intraday data is missing"))
				}
				if (is.null(.Object@mpars$cintrdate) ) {
					.Object@mpars$cintrdate <- list()
				}
				.Object@mpars$cintrdate$itz <- indexTZ(.Object@idata[[1]]$intraday[[1]]$dat)
                fmt<-"%Y-%m-%d"
				dst<-format(.Object@cdate, format=fmt)
				idt <- as.POSIXct(dst, tz="UTC") +23*3600+59*60+00 #lst min of day
				.Object@mpars$cintrdate$idate <-idt
				 assign(nameObject,.Object,envir=parent.frame())

				return(invisible())
			}
		) 



## get set sorted groups char vector
#' @export
#' @docType methods
setGeneric(name='getGroups',function(object){standardGeneric('getGroups')})
#' @aliases getGroups,Mldata,ANY-method
setMethod('getGroups','Mldata'
					,function(object){ return ( object@groupsordered ) }
					)

#' @export
#' @docType methods
setGeneric(name='setGroups<-'
							 ,function(object,value){standardGeneric('setGroups<-')})
setReplaceMethod('setGroups'
								 ,'Mldata'
								 ,function(object,value){
									 #check existing groups is a subvector of new groups vector
									 igrps <- as.character(object@imetadata$GRP)
									 iugrps <- unique(igrps)
									 m<- match(iugrps, value)
									 ml <- length(m[is.na(m)] )
									 if ( ml > 0 ){
											stop(paste("[Mldata:setGroups validation]"
												,"new groups vector does not contain all existing instrument groups"))
									 }
									 object@groupsordered <- value
									 return(object)
								 })

#' @export
#' @docType methods
setGeneric(name='insertGroup'
							 ,function(.Object,newgrp, position){standardGeneric('insertGroup')})
#' @aliases insertGroup,Mldata,ANY-method
setMethod('insertGroup','Mldata'
					,function(.Object,newgrp, position){
						##validation
						if ( !is.character(newgrp) ){
						 stop(paste("[Mldata:insertGroup validation]"
												,"newgrp param should be character string"))
						}
						nameObject <- deparse(substitute(.Object))
						grpsord <- .Object@groupsordered
						if ( position < 0 || position > length(grpsord) ){
						 stop(paste("[Mldata:insertGroup validation]"
							,"param position should be within the length of groupsordered vector "))
						}
						ngrpsord <- append(grpsord, newgrp, position)
						.Object@groupsordered <- ngrpsord
						assign(nameObject,.Object,envir=parent.frame())
					})
### get current ohlc data
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='getCdatedata'
					,function(object,iname,tfvalue,droptime=FALSE){standardGeneric('getCdatedata')})
#' @aliases getCdatedata,Mldata,ANY-method
setMethod('getCdatedata','Mldata'
		,function(object,iname,tfvalue,droptime=FALSE){
			##validation
			if (   tfvalue != 'w1'
					&& tfvalue != 'd1'){
			 stop(paste("[Mldata:getCdatedata validation]"
									," bad tfvalue param value"))
			}
			inames <-  unlist(lapply(object@idata , function(x){ x$name } ))
			iidx <- match(iname, inames)
			fd <- "%Y-%m-%d"
			xohcl <- c()
			if (!is.na(iidx)){
				xsub <- paste("::",base::format(object@cdate,format=fd),sep="")
				d1_xohlc <- object@idata[[iidx]]$d1[xsub]
				if (tfvalue == "d1" ) { 
					xohlc <- to.daily(d1_xohlc,drop.time=droptime)
				}
				if (tfvalue == "w1" ) { 
						w1xohlc <-  to.weekly(d1_xohlc,indexAt='endof',drop.time=droptime) ##w1 dte week end Friday ; set comp tz to nyc 
						colnames(w1xohlc) <- colnames(d1_xohlc)
						xohlc <- w1xohlc								
					}
			}
			return(xohlc)
		})

### get intraday ohlc data window
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='getIntraCdatedata'
					,function(object,iname,dfreq,ndays){standardGeneric('getIntraCdatedata')})
#' @aliases getIntraCdatedata,Mldata,ANY-method
setMethod('getIntraCdatedata','Mldata'
		,function(object,iname,dfreq,ndays){
			##validation
			if (  !is.numeric(ndays)){
			 stop(paste("[Mldata:getIntraCdatedata validation]"
									," ndays is not of type numeric"))
			}
			if ( !is.numeric(dfreq)){
			 stop(paste("[Mldata:getIntraCdatedata validation]"
									," dfreq is not of type numeric"))
			}
			inames <-  unlist(lapply(object@idata , function(x){ x$name } ))
			iidx <- match(iname, inames)
			lfxts <- object@idata[[iidx]]$intraday[[1]]$data
			#browser()
			if ( is.na(iidx)  || 
				 length(object@idata[[iidx]]$intraday[[1]]$data) == 0 ){
			 stop(paste("[Mldata:getIntraCdatedata validation]"
									,iname,"intraday data not properly set"))
			}

			freq_avlb <- sapply(object@idata[[iidx]]$intraday,function(x){x$freq}) 
			i_mtch <- match(dfreq, as.numeric(freq_avlb))
			zxts<-xts()
			## to test this
			is_freq_found<-FALSE
			if(!is.na(i_mtch)){
				zxts <- object@idata[[iidx]]$intraday[[i_mtch]]$data
				is_freq_found <-TRUE
			}else{
				zxts <- object@idata[[iidx]]$intraday[[1]]$data #smallest time frame
			}
			lfmt <- "%Y-%m-%d %H:%M:%S"
			dtend<-object@mpars$cintrdate$idate
			dtstart <-dtend + 3600*24 * -1 * ndays
            strprd<-paste(base::format(dtstart,format=lfmt),"::"
            			 ,base::format(dtend,format=lfmt),sep="")
            tmpxts<-zxts[strprd]			  
            # if(!is_freq_found){
            # 	tmpxts <- to.minutes(tmpxts,dfreq/60) #convert to a custom frame
            # }
            eidx <- match(index(last(tmpxts)), index(zxts))
            period_units <- periodicity(zxts)$units
            period_freq <- periodicity(zxts)$frequency
            sidx <- switch(period_units,
            			"mins" = eidx - ceiling(24*60/ period_freq * ndays)
            			,"hours" = eidx - ceiling(24*60*60/period_freq * ndays)
            			,"days" = eidx - ceiling(24*60*60/period_freq * ndays))
            			
            subxts<-zxts[sidx:eidx]
            if(!is_freq_found){ 
            	subxts <- to.minutes(subxts, dfreq/60) }
            	colnames(subxts) <- colnames(zxts)
            return(subxts)
		})


### set TZ for  daily, weekly data
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='setInterdayTZ'
		,function(.Object,tz="UTC",rebuild=FALSE){standardGeneric('setInterdayTZ')})
#' @aliases setInterdayTZ,Mldata,ANY-method
setMethod('setInterdayTZ','Mldata'
		,function(.Object,tz="UTC",rebuild=FALSE){
			nameObject <- deparse(substitute(.Object))
			if ( missing(tz)){
		 	stop(paste("[Mldata:setInterdayTZ validation]"
								,"missing timezone parameter"))
			}
			#internal function
			setTZ<-function(x,tzval,isreconstr){
				fd<-"%Y-%m-%d"
				#daily
				if(!is.null(x$d1)) {
					if (isreconstr){
						x$d1<-xts(as.data.frame(x$d1)
								,order.by=as.POSIXct(strftime(index(x$d1)
													,format=fd),tz=tzval))
					}else{ 
						indexTZ(x$d1)<-tzval
					}
				}
				#weekly
				if(!is.null(x$w1)){
					if (isreconstr){
						x$w1<-xts(as.data.frame(x$w1)
								,order.by=as.POSIXct(strftime(index(x$w1)
													,format=fd),tz=tzval))
					}else{ 
						indexTZ(x$w1)<-tzval
					}
				}
                #save to meta
                x<-setIdatMetaInterdayTimeZone(x,tzval)
                return(x)
			}
			.Object@idata <- lapply(.Object@idata, FUN=setTZ,tzval=tz,isreconstr=rebuild) 
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		})

### set TZ for  daily, weekly data
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='getInterdayTZ'
		,function(.Object){standardGeneric('getInterdayTZ')})
#' @aliases getInterdayTZ,Mldata,ANY-method
setMethod('getInterdayTZ','Mldata'
		,function(.Object){
           ltz <- lapply(.Object@idata
           				,function(x){list(name=x$name
           					             ,tz=as.character(indexTZ(x$d1)))} )
			return(unlist(ltz))
		})


### user ops functions of Mldata
#' @export
#' @docType methods
setGeneric(name='getcd1',function(object){standardGeneric('getcd1')})
#' @aliases getcd1,Mldata,ANY-method
setMethod('getcd1','Mldata'
					,function(object){ return(getCdatedata(object, getCinstrument(object),'d1')) }
				 )
#' @export
#' @docType methods
setGeneric(name='getcw1',function(object){standardGeneric('getcw1')})
#' @aliases getcw1,Mldata,ANY-method
setMethod('getcw1','Mldata'
					,function(object){ return(getCdatedata(object, getCinstrument(object),'w1')) }
				 )


#' @export
#' @docType methods
setGeneric(name='incrIntradayDate',function(.Object, delta=1,scale=60)
	                      			{standardGeneric('incrIntradayDate')})
#' @aliases incrIntradayDate,Mldata,ANY-method
setMethod('incrIntradayDate','Mldata'
			,function(.Object,delta=1,scale=60)
			{
				nameObject <- deparse(substitute(.Object))

				if( is.null(.Object@mpars$cintrdate)) {
					 stop(paste("[Mldata:incrIntradayDate validation]"
								,"intraday current date is missing"))
				}
				if( !is.POSIXct(.Object@mpars$cintrdate$idate)) {
					 stop(paste("[Mldata:incrIntradayDate validation]"
								,"intraday current date is not in POSIXct format"))
				}
				validscale<-c(1,60,180,300,600,900,1800,3600,86400)
				if (is.na(match(scale,validscale) ) ) {
					 stop(paste("[Mldata:incrIntradayDate validation]"
								,"scale param does not have a valid value:"
								,paste(validscale,collapse=",")
								))

				}
				fmt<-"%Y-%m-%d"
                if (getMode(.Object)== 'historic'){
                	nidate<-.Object@mpars$cintrdate$idate + scale*delta
                	.Object@mpars$cintrdate$idate <- nidate
                	##potential update of the daily date
    				cinstr <- getCinstrument(.Object);
					inames <-  unlist(lapply(.Object@idata 
										,function(x){ x$name } ))
					iidx <- match(cinstr, inames)
					xtsd1 <- .Object@idata[[iidx]]$d1
					idt<- as.POSIXct(format(nidate,format=fmt),tz="UTC" )
					nd<-1 
 				    #treat f last minute of the day
 				    if( as.numeric(format(nidate,format="%H"))*3600 
 				      + as.numeric(format(nidate,format="%M"))*60
 				      + as.numeric(format(nidate,format="%S")) >= 3600*23+60*59) {
 				    	nd <- nd-1 
 				    }				
					while( is.na(match( idt -3600*24*nd   ,index(xtsd1))  ) ){
						nd<-nd+1
					}
 				    .Object@cdate <- idt-3600*24*nd
                }
                else if ( getMode(.Object)== 'live'){
					 fmt<- "%Y-%m-%d %H:%M:%S"
                     .Object@mpars$cintrdate$idate<-
                     	as.POSIXct(format(last(index(z1)), format=fmt), tz="UTC")

				}
				 assign(nameObject,.Object,envir=parent.frame())
				return(invisible())
			}
		) 


#' update method for single instruments (equity)
#' @export
#' @docType methods
setGeneric(name='fetchNextDay'
							 ,function(.Object,n=1){standardGeneric('fetchNextDay')})
#' @aliases fetchNextDay,Mldata,ANY-method
setMethod('fetchNextDay','Mldata'
					,function(.Object,n=1){
						 nameObject <- deparse(substitute(.Object))


			 if (.Object@mode == 'historic' ) {
			 				 isMatched <- FALSE
							 cdidx <- match( .Object@cdate , .Object@dseq )
							 if (!is.na(cdidx)) {isMatched <-TRUE}
							 if ( !is.na(cdidx) && cdidx < length(.Object@dseq) ) {
											.Object@cdate <- .Object@dseq[cdidx+n]
							 }
							 #browser()
							 if (!isMatched){
								cinstr <- getCinstrument(.Object);
								inames <-  unlist(lapply(.Object@idata 
													,function(x){ x$name } ))
								iidx <- match(cinstr, inames)
								xtsd1 <- .Object@idata[[iidx]]$d1
								tidx <- match( .Object@cdate, index(xtsd1))
								if(!is.na(tidx)){
									.Object@cdate <- index(xtsd1[tidx+n])
									isMatched <- TRUE
								}
							 }	

							 if(!isMatched){
							 	.Object@cdate <- last(.Object@dseq) 
							 }
						 }
						 if (.Object@mode == 'live' ) {
								cdt <- Sys.time()
								edate <-  as.POSIXct(strftime( cdt, format="%Y-%m-%d" ))
								edate <-  edate + 3600 * 23 + 60*59 + 59   ## date ends on last second
								.Object@cdate <- edate
								.Object@enddate <- edate
								##update market data up to cdate
								updDataDld(.Object)
							}
							assign(nameObject,.Object,envir=parent.frame())
							return(invisible())
					})

#' @export
#' @docType methods
setGeneric(name='fetchNextWeek'
							 ,function(.Object){standardGeneric('fetchNextWeek')})
#' @aliases fetchNextWeek,Mldata,ANY-method
setMethod('fetchNextWeek','Mldata'
					,function(.Object){
						if (.Object@mode != 'historic'){
						 stop(paste("[Mldata:fetchNextWeek validation]"
												,"method can be called only in data historic mode"))
						}
						 nameObject <- deparse(substitute(.Object))
							 cdidx <- match( .Object@cdate , .Object@dseq )
							 if ( !is.na(cdidx) && cdidx < length(.Object@dseq) ) {
											.Object@cdate <- .Object@dseq[cdidx+5]
							 }else{
								 .Object@cdate <- last(.Object@dseq)
							 }
							assign(nameObject,.Object,envir=parent.frame())
							return(invisible())
					})

#' update method for contract sequence instruments (futures)
#' @export
#' @docType methods
	setGeneric(name='loadUp2dateContracts'
			,function(.Object
								,rollon="volume"
							 ,skipintraday=FALSE
							 ,intrd="now",freq=60 
							 ,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg")
			{standardGeneric('loadUp2dateContracts')})
#' @aliases loadUp2dateContracts,Mldata,ANY-method
	setMethod('loadUp2dateContracts','Mldata'
			,function(.Object
								,rollon="volume"
								,skipintraday=FALSE
								,intrd="now",freq=60 
								,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg"){
				nameObject <- deparse(substitute(.Object))

				if (.Object@mode != 'live'){
					stop(paste("[Mldata:loadUp2dateContracts validation]"
									,"method can be called only in data live mode"))
				}
				#move this  into iniDatacontracts
				cdt <- Sys.time()
				edate <-  as.POSIXct(strftime( cdt, format="%Y-%m-%d" ))
				edate <-  edate + 3600 * 23 + 60*59 + 59   ## date ends on last second
				.Object@cdate <- edate
				.Object@enddate <- edate
				##update market data up to cdate contract sequence!!
				iniDataContracts(.Object
								   ,rollon=rollon
												 ,skipintraday=skipintraday
												 ,intrd=intrd
												 ,freq=freq
												 ,cslistconf=cslistconf)
				assign(nameObject,.Object,envir=parent.frame())
				return(invisible())
			})

#' update method for contract sequence instruments (futures)
#' @export
#' @docType methods
	setGeneric(name='fetchUp2dateContracts'
			,function(.Object,csl,rollon="volume",skipintraday=FALSE)
			{standardGeneric('fetchUp2dateContracts')})
#' @aliases fetchUp2dateContracts,Mldata,ANY-method
	setMethod('fetchUp2dateContracts','Mldata'
			,function(.Object,csl,rollon="volume",skipintraday=FALSE){
				nameObject <- deparse(substitute(.Object))

				if (.Object@mode != 'live'){
					stop(paste("[Mldata:fetchUp2dateContracts validation]"
									,"method can be called only in data live mode"))
				}
				if ( !is.ContractSeqList(csl) ){
					stop(paste("[Mldata:fetchUp2dateContracts validation]"
									,"csl passed param is not of type ContractSeqList"))
				}
				cdt <- Sys.time()
				edate <-  as.POSIXct(strftime( cdt, format="%Y-%m-%d" ))
				edate <-  edate + 3600 * 23 + 60*59 + 59   ## date ends on last second
				.Object@cdate <- edate
				.Object@enddate <- edate

				##Factory to produce data for various providers
				getProviderData <- function(provider,mldata,csl,rollon,skipintraday ){
					imdata <- switch(provider,
							"dtn" = fetchDataContractsDTN(mldata
																					,contractseqlist=csl
																					,rollon=rollon
																					,skipintraday=skipintraday))
					return(imdata)
				}
				dprovider <- .Object@dataprovider

				outl <- getProviderData(dprovider,.Object,csl,rollon,skipintraday)
				

				.Object@idata <- outl$data
				.Object@cdate <- outl$lastqte_date

				assign(nameObject,.Object,envir=parent.frame())
				return(invisible())
			})


	### downloading data methods  for contract sequence (futures )
#' @keywords internal
#' @export
#' @docType methods
	setGeneric(name='iniDataContracts'
						,function(.Object,rollon="volume"
										 ,skipintraday=FALSE,intrd="now",freq=60 
										 ,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg" )
						{standardGeneric('iniDataContracts')})
#' @aliases iniDataContracts,Mldata,ANY-method
	setMethod('iniDataContracts','Mldata'
			,function(.Object,rollon="volume"
										 ,skipintraday=FALSE,intrd="now",freq=60 
										 ,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg"){

				nameObject <- deparse(substitute(.Object))

				enddate <-   .Object@enddate

				##Factory to produce data for various providers
				getProviderData <- function(provider,mldata,rollon,cslistconf
																		,skipintraday,intrd,freq ){
					imdata <- switch(provider,
							"dtn" = loadDataContractsDTN(mldata
																					,rollon=rollon
																					,skipintraday=skipintraday
																					,intrd=intrd
																					,freq=freq
																					,cslistconf=cslistconf) )
					return(imdata)
				}
				dprovider <- .Object@dataprovider

				outl <- getProviderData(dprovider,.Object,rollon,cslistconf
																,skipintraday,intrd,freq)
				

				.Object@idata <- outl$data
				.Object@cdate <- outl$lastqte_date

				assign(nameObject,.Object,envir=parent.frame())
				return ( invisible() )
			})



### downloading data methods for equities
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='iniDataDld',function(.Object,startdate){standardGeneric('iniDataDld')})
#' @aliases iniDataDld,Mldata,ANY-method
setMethod('iniDataDld','Mldata'
					,function(.Object,startdate){

						nameObject <- deparse(substitute(.Object))

					 startdate <- as.POSIXct(startdate, format="%Y-%m-%d")
					 enddate <-   .Object@enddate

					##Factory to produce data for various providers

			getProviderData <- function(provider,mldata,startdate,enddate ){
				#browser()
				imdata <- switch(provider,
						"yahoo" = iniDataDldYahoo(mldata,startdate=startdate,enddate=enddate)
						,"tblox" = iniDataDldTblox(mldata) )
				return(imdata)
						}
			dprovider <- .Object@dataprovider
						.Object@idata <- getProviderData(dprovider, .Object,startdate,enddate)

					#save data to disk
					assign(nameObject,.Object,envir=parent.frame())
					return ( invisible() )
					})



## this method is used for mode=='live' for equities
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='updDataDld',function(.Object){standardGeneric('updDataDld')})
#' @aliases updDataDld,Mldata,ANY-method
setMethod('updDataDld','Mldata'
					,function(.Object){
						nameObject <- deparse(substitute(.Object))

			#check sync between metadata and idata
			instrs <- as.character(.Object@imetadata$IDNAM)
			datinstrs <- unlist ( lapply( .Object@idata, function(x){x$name} ))
			checkMeta <- sort(instrs) == sort(datinstrs)
			crit <- match(FALSE, checkMeta)
			if(!is.na(crit)){
				stop(cat("mistmatch between instruments meta info",instrs, " and instruments data ",datinstrs,"\n"))
			}
			##Factory to produce data for various providers

			updProviderData <- function(provider,mldata){
				#browser()
				imdata <- switch(provider,
						"yahoo" = updDataDldYahoo(mldata)
						,"tblox" = updDataDldTblox(mldata) )
				return(imdata)
			}
			.Object@idata <- updProviderData(.Object@dataprovider, .Object )


			## save data to disk
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		})

## this method is used for mode=='live' for contracts sequences (futures )
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='updDataContracts',function(.Object,rollon="volume",updMeta=T,topdir="~/googledrive/mkdata/dtn/contrseq")
								 {standardGeneric('updDataContracts')})
#' @aliases updDataContracts,Mldata,ANY-method
setMethod('updDataContracts','Mldata'
		,function(.Object,rollon="volume",updMeta=T,topdir="~/googledrive/mkdata/dtn/contrseq"){
			nameObject <- deparse(substitute(.Object))

			#check sync between metadata and idata
			instrs <- as.character(.Object@imetadata$IDNAM)
			datinstrs <- unlist ( lapply( .Object@idata, function(x){x$name} ))
			checkMeta <- sort(instrs) == sort(datinstrs)
			crit <- match(FALSE, checkMeta)
			if(!is.na(crit)){
				stop(cat("mistmatch between instruments meta info",instrs, " and instruments data ",datinstrs,"\n"))
			}
			##Factory to produce data for various providers

			updProviderData <- function(provider,mldata,rollon,updateMeta,topdir){
				#browser()
				imdata <- switch(provider,
						"dtn" = updDataContractsDTN(mldata,rollon=rollon,updMeta=updateMeta,topstoredir=topdir) )
				return(imdata)
			}
			dprovider <- .Object@dataprovider
			.Object@idata <- updProviderData(dprovider,.Object,rollon=rollon,updateMeta=updMeta,topdir=topdir)

			## save data to disk
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		})

## remove last data elements
#' @export
#' @docType methods
setGeneric(name='removeDatatail',function(.Object,rmd=F,n=1,tf="d1"){
					standardGeneric('removeDatatail')})
#' @aliases removeDatatail,Mldata,ANY-method
setMethod('removeDatatail','Mldata'
					,function(.Object,rmd=F,n=1,tf="d1"){
						 nameObject <- deparse(substitute(.Object))
						 instrs <- as.character(.Object@imetadata$IDNAM)
						 datinstrs <- unlist ( lapply( .Object@idata, function(x){x$name} ))
						 cat("data elements to be deleted..\n")
						 for (j  in 1: length(instrs)){
							 cname <- instrs[j]
							 cidx <- match(cname, .Object@imetadata$IDNAM)
							 m <- match(cname, datinstrs)
							 cinst <- .Object@idata[[m]]
							 cat("instrument: ",cname, "\n")
							 if(tf=="w1"){
									 tdat <- cinst$w1
									 print(tail(tdat,n)) ##printing data
									 if (rmd){
											 cinst$w1 <- tdat[1:(nrow(tdat)-n)]
									 }
								}else if(tf=="d1"){
									 tdat <- cinst$d1
									 print(tail(tdat,n)) ##printing data
									 if (rmd){
											 cinst$d1 <- tdat[1:(nrow(tdat)-n)]
									 }
								}else if(tf=="h1"){
									 tdat <- cinst$h1
									 print(tail(tdat,n)) ##printing data
									 if (rmd){
											 cinst$d1 <- tdat[1:(nrow(tdat)-n)]
									 }
								}
							 .Object@idata[[m]]  <- cinst
						 }
						## save data to disk
						 assign(nameObject,.Object,envir=parent.frame())
						return ( invisible() )
				 })



## add remove idata element function
#' @export
#' @docType methods
setGeneric(name='addMarketdata',function(.Object,nam,tkr,typ,lbl,grp){
					standardGeneric('addMarketdata')})
#' @aliases addMarketdata,Mldata,ANY-method
setMethod('addMarketdata','Mldata'
					,function(.Object,nam,tkr,typ,lbl,grp){
						 nameObject <- deparse(substitute(.Object))

						 ##check if new group is present in groupsorted vector
						 if (is.na(match(grp, .Object@groupsordered)) ){
							stop(paste("[Mldata:addMarketdata validation]"
												,"new group", grp,"is missing from grouplist."
												,"Think to add it with setGroups() method."))
						 }
						 imkts <- unlist(lapply(.Object@idata , function(x){x$name } ))
						 imetanames <- .Object@imetadata$IDNAM
						 if (   is.na(match(nam, imkts))
								 && is.na(match(nam, imetanames)) ){
								## add entry to list of datas
								ninstd <- list(name=nam,w1=c(),d1=c() )
								.Object@idata[[ length(.Object@idata)+1]] <- ninstd
								##add entry to metadata  , reorder metaata accorging to groupsordered
								metadata <-  as.data.frame(.Object@imetadata,  stringsAsFactors=FALSE)
								metadata[ nrow(metadata)+1,] <- c(nam,tkr,typ,lbl,grp)
								##sort according to grpsorted
								grpsorted <- .Object@groupsordered
								#colnames(metadata) <- c('IDNAM', 'TKR', 'TYP', 'LBL' ,'GRP')
								metadata <- metadata[with(metadata, order(as.integer(factor(GRP,grpsorted)), IDNAM)), ]
							 .Object@imetadata <- metadata

						 }else{
							 print(paste("Mldata:addMarketdata validation"
												 ,"warning. market name",nam ,"is already in the market data list"
												 , "or in metadata"))
						 }
						 assign(nameObject,.Object,envir=parent.frame())
						return ( invisible() )
					}
					)
#' @export
#' @docType methods
setGeneric(name='rmMarketdata',function(.Object,nam){
					standardGeneric('rmMarketdata')})
#' @aliases rmMarketdata,Mldata,ANY-method
setMethod('rmMarketdata','Mldata'
					,function(.Object,nam){
						nameObject <- deparse(substitute(.Object))
						 imkts <- unlist(lapply(.Object@idata , function(x){x$name } ))
							idx <- match(nam, imkts)
						 if ( !is.na(idx)){
								#remove entry from list of datas
								.Object@idata[[idx]] <- NULL
								##remove line from @imetadata
								metadata <-  as.data.frame(.Object@imetadata,  stringsAsFactors=FALSE)
								mtidx <- match(nam,  metadata$IDNAM )
								metadata <-   metadata[-mtidx,]
								##sort according to grpsorted
								grpsorted <- .Object@groupsordered
								#colnames(metadata) <- c('IDNAM', 'TKR', 'TYP', 'LBL' ,'GRP')
								metadata <- metadata[with(metadata, order(as.integer(factor(GRP,grpsorted)), IDNAM)), ]
							 .Object@imetadata <- metadata
								setInstrumentPosition(.Object, 'first')
						 }else{
							 print(paste("Mldata:rmMarketdata validation"
													 ,"warning. market name",nam ,"was not present the market data list." ))
						 }
						 assign(nameObject,.Object,envir=parent.frame())
						return ( invisible() )
					}
					)

#' @export
#' @docType methods
setGeneric(name='printIdatMeta',function(object,nam,n=4,ctime=Sys.time()){
						standardGeneric('printIdatMeta')})
#' @aliases printIdatMeta,Mldata,ANY-method
setMethod('printIdatMeta','Mldata'
									 ,function(object,nam,n=4,ctime=Sys.time()){
						 idats <- unlist(lapply(object@idata , function(x){x$name } ))
						 idx <- match(nam, idats)
						 if ( !is.na(idx)){
							 cinstd <- object@idata[[idx]]
							 if(is.null(cinstd$meta)) {
								 pmsg <- paste("Instrument",nam, "no meta information stored\n")
								 cat(pmsg)
								 return( invisible() )
							 }
							 #print splits (for equities )
									 csplits <- getIdatMetaSplits(cinstd)
							 if(!is.na(csplits[1])){
								 pmsg<-paste("instrument",nam,"splits:")
								 print(pmsg)
								 print(csplits)
							 }
							 #print dividends (for equities)
							 cdivs <- getIdatMetaDividends(cinstd)
							 if(!is.na(cdivs[1])){
								 pmsg<-paste("instrument",nam,nrow(cdivs),"dividends: (printing first 10 and last 10")
								 print(pmsg)
								 print(head(cdivs,10))
								 print("....")
								 print(tail(cdivs,10))
							 }

							 ##print gluedsequence
							 funrld <- getIdatMetaFrontcontract(object,nam=nam,ctime=ctime,format="dt")
							 if(length(funrld)>0){
							 fcunrld <-funrld$unrolled
							 glseq <- getIdatMetaGluedseq(cinstd)
							 contracts<- unlist(lapply(glseq, function(x){x$cfmonth}))
							 eid <- match(fcunrld, contracts)  ##expected to match always
							 sid <- eid - n
							 #browser()
							 subidx <- sid:eid
							 subidx <- subidx[subidx>0]

							 subglv <- unlist(lapply(glseq[subidx]
												,function(x){
											paste("ctr:", getContractSuffix(dummyContract(nam,x$cfmonth))
													 ," exp:",x$cfexpir
															 ," rold:",x$cfend,sep="")
														 }))
							 glvstr <- paste(subglv,collapse=" ")
							 print(glvstr)
							 }
							 ##print rollon
							 rollon <- getIdatMetaRollon(cinstd)
							 if(!is.na(rollon)){
								 pmsg<-paste("instrument",nam,"contract sequence roll criteria:",rollon)
								 print(pmsg)
							 }
							 ##print volume count
							 volcnt <- getIdatMetaVolccount(cinstd)
							 if(!is.na(volcnt)){
								 pmsg<-paste("instrument",nam,"contracts summed volumes count:",volcnt)
								 print(pmsg)
							 }
						 }
						 return(invisible())
 })

#' @export
#' @docType methods
 setGeneric(name='getIdatMetaRolls',function(object,nam){
			 standardGeneric('getIdatMetaRolls')})
#' @aliases getIdatMetaRolls,Mldata,ANY-method
 setMethod('getIdatMetaRolls','Mldata'
		 ,function(object,nam){
			 idats <- unlist(lapply(object@idata , function(x){x$name } ))
			 idx <- match(nam, idats)
			 rolls<-list()
			 if ( !is.na(idx)){
				 cinstd <- object@idata[[idx]]
				 glseq <- getIdatMetaGluedseq(cinstd)
				 if(length(glseq) > 0){
					arolls <- unlist(lapply(glseq,function(x){x$cfend} ))
					##get rid of last NA
					arolls <-arolls[!is.na(arolls)]
					rolls <- as.list(arolls)
				}
			 }
			 return(rolls)
		 })
#' @export
#' @docType methods
 setGeneric(name='getIdatMetaFrontcontract',function(object,nam,ctime=Sys.time(),format="ch"){
			 standardGeneric('getIdatMetaFrontcontract')})
#' @aliases getIdatMetaFrontcontract,Mldata,ANY-method
setMethod('getIdatMetaFrontcontract','Mldata'
		,function(object,nam,ctime=Sys.time(),format="ch"){
			okformats <-c("ch","dt")
			if( is.na(match(format,okformats))){
				stop(paste("[Mldata:getIdatMetaFrontcontract validation]"
								,"bad value of supplied parameter format"))
			}

			idats <- unlist(lapply(object@idata , function(x){x$name } ))
			idx <- match(nam, idats)
			fc<-list()
			if ( !is.na(idx)){
				cinstd <- object@idata[[idx]]
				glseq <- getIdatMetaGluedseq(cinstd)
				if(length(glseq)>0){
					contracts<- unlist(lapply(glseq, function(x){x$cfmonth}))
					rolls<- unlist(lapply(glseq, function(x){x$cfend}))
					expirs<- unlist(lapply(glseq, function(x){x$cfexpir}))

					rdiff <- as.POSIXct(rolls) - as.POSIXct(ctime)
					exdiff <- as.POSIXct(expirs) - as.POSIXct(ctime)
					##unrolled
					rcrit <- match(TRUE, rdiff >= 0 )
					if(is.na(rcrit)){ ##cctime is more recent than last found expiry
						cid <- first(which( is.na(rolls) == TRUE ))
					}else{
						cid <- rcrit
					}
					cunrld <- contracts[cid]
					if(format =="ch"){ cunrld_str <- getContractSuffix( dummyContract(nam,cunrld))}
					else              {cunrld_str <- cunrld }
					fc$unrolled <- cunrld_str
					##unexpired
					excrit <- match(TRUE, exdiff >= 0 )
					if(is.na(excrit)){ ##cctime is more recent than last found expiry
						cid <- first(which( is.na(expirs) == TRUE ))
					}else{
						cid <- excrit
					}
					cunexp <- contracts[cid]
					if(format =="ch"){ cunexp_str <- getContractSuffix( dummyContract(nam,cunexp)) }
					else             {cunexp_str <- cunexp  }
					fc$unexpired <- cunexp_str
				}
			}
			return( fc )

		})

 #' @export
 #' @docType methods
setGeneric(name='persistMdat'
               ,function(.Object,fname='/tmp/dflt_mldata.rdat')
               			{standardGeneric('persistMdat')})
#' @aliases persist,Mldata,ANY-method
setMethod('persistMdat'
                 ,'Mldata'
                 ,function(.Object,fname='/tmp/dflt_mldata.rdat'){
                   nameObject <- deparse(substitute(.Object))
                   save(nameObject, .Object, file=fname)
                   return(invisible())
                 }
          )


#' @export
#' @docType methods
 setGeneric(name='getIdatMetaRollcrit',function(object,nam){
			 standardGeneric('getIdatMetaRollcrit')})
#' @aliases getIdatMetaRollcrit,Mldata,ANY-method
 setMethod('getIdatMetaRollcrit','Mldata'
		 ,function(object,nam){
			 idats <- unlist(lapply(object@idata , function(x){x$name } ))
			 idx <- match(nam, idats)
			 rollon <- NA
			 if ( !is.na(idx)){
				 cinstd <- object@idata[[idx]]
				 rollon<-getIdatMetaRollon(cinstd)
			 }
			 return(rollon)
		 })


 #' @export
 #' @docType methods
setGeneric(name='addDate'
               ,function(.Object,ny=0,nd=0,hasIntrd=FALSE)
               			{standardGeneric('addDate')})
#' @aliases persist,Mldata,ANY-method
setMethod('addDate'
                 ,'Mldata'
                 ,function(.Object,ny=0,nd=0,hasIntrd=FALSE){
            nameObject <- deparse(substitute(.Object))
			#internal function
			elmodyear<-function(x,ny,nd,hasintrad){
				cat("processing ", x$name, "..\n")
				#daily
				if(!is.null(x$d1)) { 
					x$d1 <- addDtXtsSerie(x$d1,ny=ny,nd=nd) 
				    #resample weekly
					x$w1 <- to.weekly(x$d1,indexAt="lastof")
					#x$w1 <- addDtXtsSerie(x$w1,ny=ny,nd=nd)
				}
				if(hasintrad){
				  #implement intraday data  date shift
                   #save to meta
                   ###x<-setIdatMetaInterdayTimeZone(x,tzval)
                }
                return(x)
			}
			#idata
			.Object@idata <- lapply(.Object@idata, FUN=elmodyear,ny=ny,nd=nd,hasintrad=hasIntrd)
            #dseq
            .Object@dseq <- addDtWkPosixCt(.Object@dseq,ny=ny,nd=nd)
            #cdate
            .Object@cdate <-  addDtWkPosixCt(.Object@cdate, ny=ny,nd=nd)

			assign(nameObject,.Object,envir=parent.frame())  
            return(invisible())
})



###helper functions

#' @keywords internal
setGeneric( name='appendIntraDayInfo'
					 ,function(object){standardGeneric("appendIntraDayInfo")})
setMethod('appendIntraDayInfo', 'Mldata'
					,function(object){
						s1fmt <- '%Y-%m-%d %H:%M:%S'
						df <- object@imetadata
						freqs <- unlist(lapply(object@idata
																	,function(x){
																		return(paste(unlist(lapply(x$intraday
																												,function(y){y$freq}))
																								,collapse=","))

																	 } )) 
						dtstarts <- unlist(lapply(object@idata
												,function(x){
													 minfreq <- min(unlist(lapply(x$intraday
																							,function(y){y$freq})))
													 idmin <- match(as.character(minfreq),
																					unlist(lapply(x$intraday
																								 ,function(y){y$freq})))
													 dtstart <- strftime(first(index(x$intraday[[idmin]]$data))
																							 ,format=s1fmt)
														return(dtstart)
												 } )) 
						dtends <- unlist(lapply(object@idata
												,function(x){
													 minfreq <- min(unlist(lapply(x$intraday
																							,function(y){y$freq})))
													 idmin <- match(as.character(minfreq),
																					unlist(lapply(x$intraday
																								 ,function(y){y$freq})))
													 dtend <- strftime(last(index(x$intraday[[idmin]]$data))
																							 ,format=s1fmt)
														return(dtend)
												 } )) 


						cnames <- colnames(df)
						df<-cbind(df,freqs,dtstarts,dtends)
						colnames(df) <- c(cnames,"intraday freqs"
														 ,"date first"
														 ,"date last")
			
						return(df)

					})



#' @export
setMethod ('show'
					 ,'Mldata'
					 , function(object){
						s1fmt <- '%Y-%m-%d %H:%M:%S'
						dfmt <-'%Y-%m-%d %Z %A'
						cat("*** Type Mldata,*** \n")
						cat("* Downloaded Market data specs:\n")
						 
						cat(paste("dataprovider:", object@dataprovider
											 ,"mode:", object@mode
											 ,"enddate:", strftime(object@enddate, format=s1fmt), "\n"))


						 cat("instrument groups order:  ")
						 cat(object@groupsordered, "\n")
						 cat("instruments:\n ")
						 
						 df <- object@imetadata
						 
						 #main information in a dataframe
						 if ( length(object@idata[[1]]$intraday)>0 ){
									df <- appendIntraDayInfo(object)
						 } 
						 print(df)

						 dlengths <- unlist(lapply(object@idata , function(x){ if(!is.null(x$d1)) nrow(x$d1) else 0 } ))
						 cat("Custom ordered groups:")
						 cat( getGroups(object), "\n")
						 cat("Number of daily data points: ")
						 cat(dlengths)
						 cat("\n")
						 fmt<-"%Y-%m-%d"
						 fdates <-  unlist(lapply(object@idata
																		 ,function(x){
																				if(!is.null(x$d1)){
																								out <- strftime(index(first(x$d1)),format=fmt)
																				}else{ out <- as.character(NA) }
																				return(out)
																			 }
															 ))
						 cat("First downloaded daily data date: ")
						 cat(fdates)
						 cat("\n")
						 ldates <-  unlist(lapply(object@idata
																		 ,function(x){
																				if(!is.null(x$d1)){
																								out <- strftime(index(last(x$d1)),format=fmt)
																				}else{ out <- as.character(NA) }
																				return(out)
																			 }
															 ))
						 cat("Last downloaded daily data date: ")
						 cat(ldates)
						 cat("\n")
						 cat("timezones for daily xts data:","\n")
						 print(getInterdayTZ(object))
  						 cat(paste("current date:", base::format(object@cdate,format=dfmt), "\n" ))
                         if(!is.null(object@mpars$cintrdate)){ 
                         	cat(paste("time:", base::format(getCintradate(object) ,format=s1fmt), "\n" ))
                         }
						 cat("*** Type Mldata End.*** \n")
						}
						)



#' @export
#' @docType methods
setGeneric( name='is.Mldata',function(object){standardGeneric("is.Mldata")})
#' @aliases is.Mldata,Mldata,ANY-method
setMethod('is.Mldata'
					,'Mldata'
					,function(object){
						return ( is(object, 'Mldata') )
					})











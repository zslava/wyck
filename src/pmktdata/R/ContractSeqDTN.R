#' set config from historic data for DTN datasource

#' @docType methods
setGeneric(name='iniDownloadDTN',function(.Object, lstqtime){standardGeneric('iniDownloadDTN')})
#' @aliases iniDownloadDTN,ContractSeq,ANY-method
setMethod('iniDownloadDTN','ContractSeq'
		,function(.Object,  lstqtime ){

			if( missing(lstqtime)){
				stop(paste("ContractSeq::iniDownloadDTN validation"
								,"missing param of last available quote"))
			}

			nameObject <- deparse(substitute(.Object))

			d1fmt<-'%Y-%m-%d'
			today <-  as.POSIXct(strftime(lstqtime, format=d1fmt))
			yday <- today - 86400 * 1
			cdf <- getContractSequence(.Object)
			datasrc <- .Object@datasrc
			iname <- getInstrumentName(.Object)
			iTickerPref <- getTickerPref(.Object)
			d1offset <- getD1offset(.Object)
			##download daily contract data
			expiry <-c()
			contrdata <- list()
			for (j in 1:nrow(cdf)){
				cmonth <- cdf$contr_month[j]
				ccontr <- bareContract(iname, iTickerPref, cmonth )
				cticker <- getContractTicker(ccontr)
				#download data from dtn
				#browser()
				xtsd <- dtnHistDailyFS(cticker,verbose=T)
				upTodaysub<-paste("::",today,sep="")
				xtsd<-xtsd[upTodaysub]
				##strip laste quote with empty volume if it is in the past (DTN problem )
				if ( index(last(xtsd)) < yday  && Vo( xtsd[nrow(xtsd)] ) == 0 ){
					xtsd <- xtsd[1:(nrow(xtsd)-1)]
				}

				expirIndex <- last(index(xtsd))
				if ( as.POSIXct(expirIndex) < yday  ){
					expirDateStr <- strftime(expirIndex, format=d1fmt)
					expiry[j] <- expirDateStr
				}else{
					expiry[j] <- NA
				}
				#browser()
				##find subsets to store d1 data
				if(is.na(expiry[j])  ){
					fnaidx <- match(NA,expiry)
					pexpiry <- strftime (  as.POSIXct(expiry[fnaidx-1] ) - 86400 * d1offset, format=d1fmt  )
					dsubs <- paste(pexpiry,"::",sep="")
					dldstart<-pexpiry
					dldend <-strftime(as.POSIXct(index(last(xtsd))), format=d1fmt)
				}else{
					if(j==1){k <- j   }
					else    {k <- j-1 }
					pexpiry <- strftime (  as.POSIXct(expiry[k]) - 86400 * d1offset, format=d1fmt  )
					dsubs <- paste( pexpiry,"::",expiry[j],sep="")
					dldstart<-pexpiry
					dldend<-expiry[j]
				}
				contrdata[[length(contrdata)+1]] <-list(ticker=cticker
						,month=cmonth
						,expiry=expiry[j]
						#,d1start =dldstart
						#,d1end=dldend
						,d1start = strftime(index(first(xtsd[dsubs])),format=d1fmt)
						,d1end =   strftime(index(last(xtsd[dsubs])),format=d1fmt)
						,bd1=xtsd[dsubs] )
			}##loop on contracts

			d1start <- unlist(lapply(contrdata, function(x){x$d1start}))
			d1end <- unlist(lapply(contrdata, function(x){x$d1end}))
			cdf <- cbind(as.matrix(cdf), d1start)
			cdf <- cbind(as.matrix(cdf), d1end)

			expiry <- expiry
			cdf <- cbind(as.matrix(cdf), expiry)

			cdf <- as.data.frame(cdf, stringsAsFactors = FALSE)

			## update lastDataDaily field
			.Object@lastDataDaily <- max(as.POSIXct(cdf$d1end))


			setContractSequence(.Object) <- cdf
			.Object@data <- contrdata ## assign contract data

			##add ref continuos contract data
			contTicker <- paste(.Object@tickerPref,.Object@contSuff,sep="")
				xtsd <- dtnHistDailyFS(contTicker,verbose=F)
				upTodaysub<-paste("::",today,sep="")
				xtsd<-xtsd[upTodaysub]
			.Object@contrefdata$bd1 <- xtsd


			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		})
###!!!!!! reset
#' initial of download intraday data

#' @docType methods
setGeneric(name='iniDownloadIntraDTN',function(.Object, lstqtime
	                                          ,interval=3600,lfactor=1.33,startContract=NA,endContract=NA
	                                          ,verbose=TRUE,reset=FALSE)
	      {standardGeneric('iniDownloadIntraDTN')})
#' @aliases iniDownloadIntraDTN,ContractSeq,ANY-method
setMethod('iniDownloadIntraDTN','ContractSeq'
		,function(.Object,  lstqtime
			     ,interval=3600,lfactor=1.33,startContract=NA,endContract=NA
			     ,verbose=TRUE,reset=FALSE ){

	if( missing(lstqtime)){
		stop(paste("ContractSeq::iniDownloadIntraDTN validation"
						,"missing param of last available quote"))
	}

	nameObject <- deparse(substitute(.Object))

	d1fmt<-'%Y-%m-%d'
	s1fmt <- '%Y-%m-%d %H:%M:%S'
	cdf <- getContractSequence(.Object)
	iname <- getInstrumentName(.Object)
	iTickerPref <- getTickerPref(.Object)
    contracts <- cdf$contr_month
    #starting contract
    sidx <- match(startContract, contracts)
    if(is.na(sidx)) { sidx <- 1 }
    #ending contract
    eidx <- match(endContract, contracts)
    if(is.na(eidx)){ 
    	# find next to front contract for lstqtime date
    	eidx <- match( getUnexpiredContract(.Object, ctime=lstqtime, n=2) ,contracts)
       if(is.na(eidx)) { eidx <- length(contracts) }
   }
    dldcontracts <- contracts[sidx:eidx]
    dldexpiries <-  cdf$expiry[sidx:eidx]

    #find startDt and endDt arrays for data periods of dldcontracts
    endDt <- as.POSIXct( cdf$expiry[sidx:eidx], format = d1fmt)
    endDt <- as.POSIXct(sapply(endDt, function(x){min(x,as.POSIXct(lstqtime))}) 
    	                 ,origin='1970-01-01')                                  

    endDt <- endDt + 86399  # ensure 23:59:59 
    endDt[is.na(endDt)] <- as.POSIXct(lstqtime)
    dldPeriod <-  sapply( as.numeric(strftime(as.POSIXct(dldcontracts)
    	                    	             ,format="%m"))
                         ,FUN=getEvalContractPeriod
                         ,contrYearSched = .Object@yrContracts)
    startDt <- endDt - ceiling( lfactor * dldPeriod * 86400) 
    # ensure that all startDt are less then Sys.time() ,relevant for  contracts after front month
    startDt <- as.POSIXct(sapply(startDt, function(x){min(x,as.POSIXct(lstqtime)-86400)}) 
    	                 ,origin='1970-01-01')                                  

    frontMonthIdx <- length(dldcontracts) -1 
    if(reset){
    	intrdata <- list()
    }else{
    	intrdata <- .Object@intrdata 
    }
    #browser()    
    for( j  in 1:length(dldcontracts) ){
    	cmonth <- dldcontracts[j]
    	cexpiry <- dldexpiries[j]
		ccontr <- bareContract(iname, iTickerPref, cmonth )
		cticker <- getContractTicker(ccontr)
		sdt <- Sys.time()
		xtsdi <- dtnHistIntervalFS(instrument=cticker,intervalinsec=interval
			                      ,startdatetime=strftime(startDt[j], format=s1fmt)
			                      ,enddatetime=strftime(endDt[j], format=s1fmt)
			                      ,verbose=T)
		edt <- Sys.time()

        msg <- paste(cticker, "intraday", interval, "data downloaded in"
        	        ,formatC(as.numeric(edt-sdt),digits=3), "secs", sep=" " ) 
        if(verbose){ loginfo(msg, logger=getLoggername(.Object)) }	  
        #browser()      		
		##strip laste quote with empty volume (DTN problem )
		if ( Vo( xtsdi[nrow(xtsdi)] ) == 0 ){
			xtsdi <- xtsdi[1:(nrow(xtsdi)-1)]
		}
        #we can put here some data check
 		## make intraday data in sync with daily data
 		cpos <- match(cmonth, unlist(lapply(.Object@data, function(x){x$month})))
 		xtsd1 <- .Object@data[[cpos]]$bd1
 		if(j < frontMonthIdx){
 			syncIndx <-syncWithDayIndex(xtsd1, xtsdi,isContractUnexpired=FALSE)
 		}else{
 			syncIndx <-syncWithDayIndex(xtsd1, xtsdi,isContractUnexpired=TRUE)			
 		}
 		xtsdi_sync <- xtsdi[syncIndx]
        #list of properties
        nl <-  list(ticker=cticker,month=cmonth, idat=list())
        intrvaldat <- list(freq=as.character(interval)
        	              ,dtstart=strftime(index(first(xtsdi_sync)),format=s1fmt,tz=indexTZ(xtsdi_sync))
        	              ,dtstop=strftime(index(last(xtsdi_sync)),format=s1fmt,tz=indexTZ(xtsdi_sync))
        	              ,xdat=xtsdi_sync)
        nl$idat[[ length(nl$idat) +1 ]] <- intrvaldat
        intrdata[[ length(intrdata)+1]] <- nl
    }
    .Object@intrdata <- intrdata
	assign(nameObject,.Object,envir=parent.frame())
	return ( invisible() )
     
})
# to be moved to idatOps
#' @keywords internal
syncWithDayIndex<-function(xtsdaily, xtsintraday,isContractUnexpired ){

	cr1 <- as.Date(index(xtsintraday)) %in% as.Date(index(xtsdaily))
	crit <- cr1
    if  (isContractUnexpired) {
    	cr2 <- as.Date(index(xtsintraday)) == as.Date(index(last(xtsintraday)))
    	crit <- cr1 | cr2
    }
    syncIndex <- which(crit ==TRUE)
    return(syncIndex)
}	

# # to be moved to idatOps
# #' @keywords internal
# labelXtsData<-function(datl, xtsd, interval){
# 	switch( as.character(interval),
# 			 "60"    = datl$b60   <- xtsd,
# 			 "180"   = datl$b180  <- xtsd,
# 			 "300"   = datl$b300  <- xtsd, 
# 			 "600"   = datl$b600  <- xtsd,
# 			 "900"   = datl$b900  <- xtsd,
# 			 "1200"  = datl$b1200 <- xtsd, 
# 			 "1800"  = datl$b1800 <- xtsd, 
# 			 "3600"  = datl$b3600 <- xtsd, 
# 			 "7200"  = datl$b7200 <- xtsd 
# 	)
# 	return (datl)
# }



#' update intraday data

#' @docType methods
setGeneric(name='updateIntraDTN',function(.Object, lstqtime=Sys.time()
	                                      ,interval=3600
	                                      ,startContract=NA,endContract=NA
	                                      ,verbose=FALSE)
	      {standardGeneric('updateIntraDTN')})
#' @aliases updateIntraDTN,ContractSeq,ANY-method
setMethod('updateIntraDTN','ContractSeq'
		,function(.Object,  lstqtime=Sys.time()
			      ,interval=3600, startContract=NA,endContract=NA,verbose=FALSE ){

	if( missing(lstqtime)){
		stop(paste("ContractSeq::updateIntraDTN validation"
						,"missing param of last available quote"))
	}
	nameObject <- deparse(substitute(.Object))
	d1fmt<-'%Y-%m-%d'
	s1fmt <- '%Y-%m-%d %H:%M:%S'
	cdf <- getContractSequence(.Object)
	iname <- getInstrumentName(.Object)
	iTickerPref <- getTickerPref(.Object)
    contracts <- cdf$contr_month
    #starting contract
    frontMonthContract <- getUnexpiredContract(.Object, ctime=lstqtime, n=1)
    if(is.na(startContract)){
       sidx <- match(frontMonthContract, contracts)	
    }else{
    	sidx <- match(startContract, contracts)
    }
    # find next to front contract for lstqtime date
    if(is.na(endContract)){
     eidx <- match( getUnexpiredContract(.Object, ctime=lstqtime, n=1) ,contracts)	
    }else{
    	eidx <- match(endContract, contracts)
    }
    if(is.na(eidx)){
		stop(paste("ContractSeq::updateIntraDTN validation"
						,"object sequences missing endContract param or data in front contract"))

    }
    dldcontracts <- contracts[sidx:eidx]
    serieOffset4upd <- 4
    for( j  in 1:length(dldcontracts) ){
    	cmonth <- dldcontracts[j]
		ccontr <- bareContract(iname, iTickerPref, cmonth )
		cticker <- getContractTicker(ccontr)
       #locate month and interval indices of data to be appended
       icontridx <- match(cmonth
       	                ,unlist(lapply(.Object@intrdata
       	                	          ,function(x){x$month})))
       intrvalidx <-match(as.character(interval)
       	                ,unlist(lapply(.Object@intrdata[[icontridx]]$idat
       	                	           ,function(x){x$freq})))
       if( is.na(icontridx) || is.na(intrvalidx)){
       		wmsg <- paste("updateIntraDTN: intraday data not found for instrument"
       			        ,iname,"contract:", "interval:",interval
       			        ,"contract:",cmonth,sep=" ")
       		logwarn(wmsg,logger=getLoggername(.Object))
       		next        		
        }
        ixdat <- .Object@intrdata[[icontridx]]$idat[[intrvalidx]]$xdat
        ds <-   index( last(ixdat, serieOffset4upd)[1])
        de <- as.POSIXct(lstqtime)
	    sdt <- Sys.time()

        xdat2apnd <- dtnHistIntervalFS(instrument=cticker
       	                             ,intervalinsec=interval
			                     	 ,startdatetime=strftime(ds,format=s1fmt)
			                         ,enddatetime=strftime(de, format=s1fmt)
			                         ,verbose=verbose)
       	edt <- Sys.time()
       	#browser()
    	ixdat <- appendXtsSerie(ixdat, xdat2apnd,cticker,type='bar'
                                ,endoffset=3, dts=sdt,dte=edt,verbose=verbose)
    	#assign appended serie
        .Object@intrdata[[icontridx]]$idat[[intrvalidx]]$xdat <- ixdat
        .Object@intrdata[[icontridx]]$idat[[intrvalidx]]$dtstart <- strftime(index(first(ixdat))
        	                                                                ,format=s1fmt
        	                                                                ,tz=indexTZ(ixdat))
        .Object@intrdata[[icontridx]]$idat[[intrvalidx]]$dtstop <- strftime(index(last(ixdat))
        																	,format=s1fmt
        																	,tz=indexTZ(ixdat))

     }
    
	assign(nameObject,.Object,envir=parent.frame())
	return ( invisible() )
     
})


#' update daily data EOD for currently unexpired contracts
#' @export
#' @docType methods
setGeneric(name='updateEODDTN',function(.Object, scontract=NA, ctime=Sys.time() ){standardGeneric('updateEODDTN')})
#' @aliases updateEODDTN,ContractSeq,ANY-method
setMethod('updateEODDTN','ContractSeq'
	,function(.Object,  scontract=NA,ctime=Sys.time() ){

		nameObject <- deparse(substitute(.Object))

		d1fmt<-'%Y-%m-%d'
		today <-  as.POSIXct(strftime(ctime, format=d1fmt))
		yday <- today - 86400 * 1
		cdf <- getContractSequence(.Object)
		datasrc <- .Object@datasrc
		iname <- getInstrumentName(.Object)
		iTickerPref <- getTickerPref(.Object)
		xdata <- .Object@data
		d1offset <- getD1offset(.Object)
		datamonths <- unlist(lapply(xdata,function(x){x$month}))
        
        #determine all ( latest expired + unexpired contracts which need to be updated)
		if ( is.na(scontract)) {
          fc <- getUnexpiredContract(.Object,ctime=ctime,n=0)
        }else {
          fc <- strftime(as.Date(scontract))
        }
        fcid <- match(fc, cdf$contr_month )
        unexpiredIdx <- fcid:nrow(cdf)

		unexpiredContr <- cdf$contr_month[unexpiredIdx]
		lastExpiredId <- first(unexpiredIdx)-1
		lastExpiry <- cdf$expiry[lastExpiredId]
		##download daily contract for unexpired contracts


		for (j in 1:length(unexpiredContr)){
			cmonth <- unexpiredContr[j]
			ccontr <- bareContract(iname, iTickerPref, cmonth )
			cticker <- getContractTicker(ccontr)
			rowid <- match(cmonth,cdf$contr_month)
			#download data from dtn
				xtsd <- dtnHistDailyFS(cticker,verbose=T)
				upTodaysub<-paste("::",today,sep="")
				xtsd<-xtsd[upTodaysub]

			##find subsets to store d1 data
			pexpiry <- strftime (  as.POSIXct(lastExpiry) - 86400 * d1offset, format=d1fmt  )
			dsubs <- paste(pexpiry,"::",sep="")
			#dldstart<-pexpiry
			dldstart<-strftime(as.POSIXct(index(first(xtsd))), format=d1fmt)
			dldend <-strftime(as.POSIXct(index(last(xtsd))), format=d1fmt)

			ccdat <- list(ticker=cticker ##store contract data for unexpired
					,month=cmonth
					,expiry=NA
					,d1start=dldstart
					,d1end=dldend
					,bd1=xtsd[dsubs] )

			didx <- match( cmonth, datamonths )
			if(!is.na(didx)){
		  	   xdata[[ didx  ]] <- ccdat
		    }else{
				xdata [[ length(xdata)+1 ]] <- ccdat
			}
		}##loop on unexpired contracts

		#browser()
		d1start <- unlist(lapply(xdata, function(x){x$d1start}))
		d1end <- unlist(lapply(xdata, function(x){x$d1end}))
		cdf$d1start <- d1start
		cdf$d1end <- d1end
		.Object@contrseq <- cdf
		.Object@data <- xdata

		##add ref continuos contract data
		contTicker <- paste(.Object@tickerPref,.Object@contSuff,sep="")
			xtsd <- dtnHistDailyFS(contTicker,verbose=T)
			upTodaysub<-paste("::",today,sep="")
			xtsd<-xtsd[upTodaysub]
		.Object@contrefdata$bd1 <- xtsd
		## update lastDataDaily field
		.Object@lastDataDaily <- max(as.POSIXct(cdf$d1end))

		assign(nameObject,.Object,envir=parent.frame())
		return ( invisible() )
})

#' update daily data EOD for currently unexpired contracts
#' @export
#' @docType methods
setGeneric(name='iniDownloadIDDTN',function(.Object, intval=3600, startdate='2008-01-01', verbose=F ){standardGeneric('iniDownloadIDDTN')})
#' @aliases iniDownloadIDDTN,ContractSeq,ANY-method
setMethod('iniDownloadIDDTN','ContractSeq'
	,function(.Object,  intval=3600, startdate='2008-01-01', verbose=F ){

    ##input validation
    okintvals <- c(60,300,3600)
    if( !(intval %in% okintvals) ){
		stop(paste("ContractSeq:iniDownloadIDDTN", "correct values for intval param are 60,300,3600"))    	
    }

	nameObject <- deparse(substitute(.Object))

    xdata <- .Object@data
    datamonths <- unlist(lapply(xdata,function(x){x$month}))

	d1fmt<-'%Y-%m-%d'
	idfmt<-'%Y-%m-%d %H:%M:%S'
	today <-  as.POSIXct(strftime(Sys.time(), format=d1fmt))
	start <-  as.POSIXct(strftime(startdate, format=d1fmt))

	cdf <- getContractSequence(.Object)
	datasrc <- .Object@datasrc
	iname <- getInstrumentName(.Object)
	iTickerPref <- getTickerPref(.Object)
  
    ##find first and last  rows in contract seq data
    #first contract     
    expirs <- as.POSIXct(cdf$expiry, format=d1fmt)
    stdif <- expirs - start
    startid <- match(TRUE, stdif > 0 )  ##find nearest expiry date to the startdate
    if(is.na(startid)){
		stop(paste("ContractSeq:iniDownloadIDDTN", "cannot find nearest contract for specified start date", startdate))
    }
    #last contract = currently unexpired + 1
    fc <- getUnexpiredContract(.Object,ctime=strftime(today, format=d1fmt),n=1)
    fcid <- match(fc, cdf$contr_month )
    if(is.na(fcid)){
		print(paste("ContractSeq:iniDownloadIDDTN", "could not find current unexpired contract, check  ContractSeq object"))
		next
    }
    lastid <- fcid
    if(nrow(cdf)>fcid) { lastid <- fcid + 1 }  # next month
    ###loop between startid and lastid
    for(j in startid:lastid) {
    	cmonth <- cdf$contr_month[j]
		ccontr <- bareContract(iname, iTickerPref, cmonth )
		cticker <- getContractTicker(ccontr)
		dstart <- cdf$d1start[j]
		dendt <-  as.POSIXct( cdf$d1end[j] )  + 86399 ## make sure full last day is accounted
		dend <- strftime(dendt, format=idfmt)
		idxts <- dtnHistIntervalFS(cticker, intval, startdatetime=dstart, enddatetime=dend, verbose=verbose)
		didx <- match( cmonth, datamonths )
        if (is.na(didx)){
        	print(paste("contract", cmonth, "does not seem to have donloaded daily data. skipping.."))
        	next ## skip to next loop interation
        }else{	
        	ccdat <- xdata[[didx]]
        }
 
        if      (intval == 60) { ccdat$bm1 <- idxts }
        else if (intval == 300) { ccdat$bm5 <- idxts }
        else if (intval == 3600) { ccdat$bh1 <- idxts }

        ##assign contract into data list
	  	   xdata[[ didx  ]] <- ccdat
    }
    .Object@data <- xdata

	assign(nameObject,.Object,envir=parent.frame())
})

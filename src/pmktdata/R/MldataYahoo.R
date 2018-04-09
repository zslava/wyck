#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='iniDataDldYahoo',function(.Object,startdate,enddate,droptime=TRUE){standardGeneric('iniDataDldYahoo')})
#' @aliases iniDataDldYahoo,Mldata,ANY-method
setMethod('iniDataDldYahoo','Mldata'
		,function(.Object,startdate,enddate,droptime=TRUE){

		tz <- "UTC"

		startdate <- as.POSIXct(startdate, format="%Y-%m-%d")
		enddate <-   as.POSIXct(enddate, format="%Y-%m-%d")		
		
		instrs <- as.character(.Object@imetadata$IDNAM)
		idata <- .Object@idata
		for (j  in 1: length(instrs)){
			cname <- instrs[j]
			cidx <- match(cname, .Object@imetadata$IDNAM)
			ctkr <- as.character(.Object@imetadata$TKR[cidx])
			ctyp <- as.character(.Object@imetadata$TYP[cidx])
			
			#cinstd <- list(name=cname)
			cinstd <- mkbareIdat(cname)
			cinstd <- setIdatDtype(cinstd,ctyp)
			cdd1 <- yahooHistDailyFS(ctkr,fromd=startdate, tod=enddate, itype=ctyp,tz=tz)
			#cinstd$d1 <- cdd1
			cinstd <- setIdatD1(cinstd,cdd1)
			if (nrow(cdd1) > 0 ) {
				cdw1 <- to.weekly(cdd1, indexAt='lastof',drop.time=droptime) ##startof old value (set timezone to NYC)
				index(cdw1) <- as.POSIXct( index( cdw1) )
				colnames(cdw1) <- colnames(cdd1)
				#cinstd$w1 <- cdw1
				cinstd <- setIdatW1(cinstd,cdw1)
			}
			##set dividends & splits meta info
			if(ctyp =="eq"){
				csplits <- dldSplitsYahoo(ctkr,from=index(first(cdd1)), to=index(last(cdd1)))
				if(!is.na(csplits[1]))  {  cinstd <- setIdatMetaSplits(cinstd,csplits)  }
				
				cdivs <- dldDividendsYahoo(ctkr,from=index(first(cdd1)), to=index(last(cdd1)))
				if(!is.na(cdivs[1]))  {  cinstd <- setIdatMetaDividends(cinstd,cdivs)  }
			}
			##append new list object to the end of its container list
			idata[[  length(idata)+1 ]]  <- cinstd
		}#loop over instrument list
				

		return ( idata )			
})

#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='updDataDldYahoo',function(.Object,droptime=TRUE){standardGeneric('updDataDldYahoo')})
#' @aliases updDataDldYahoo,Mldata,ANY-method
setMethod('updDataDldYahoo','Mldata'
		,function(.Object,droptime=TRUE){
			histd1 <- 10000 #daily data depth
			nenddate <- .Object@cdate
			instrs <- as.character(.Object@imetadata$IDNAM)
			idata <- .Object@idata
			datinstrs <- unlist ( lapply( idata, function(x){x$name} ))
			for (j  in 1: length(instrs)){
				cname <- instrs[j]
				cidx <- match(cname, .Object@imetadata$IDNAM)
				ctkr <- as.character(.Object@imetadata$TKR[cidx])
				ctyp <- as.character(.Object@imetadata$TYP[cidx])
				
				m <- match(cname, datinstrs)
				cinstd <- idata[[m]]
				
				print(paste('update download yahoo market data to date', nenddate
								,'for instrument',cinstd$name, '...'))		          
				if ( !hasIdatD1(cinstd) ){	
					dd1 <- yahooHistDailyFS(ctkr
							,fromd=.Object@enddate-histd1 * 86400
							,tod=.Object@enddate, ctyp)
					dd1 <- dd1[-nrow(dd1)] #strip off last element
				    cinstd <- setIdatD1(cinstd,dd1)
				}
				od1 <- getIdatD1(cinstd)
				lstohlc <- last(od1)
				lstdate <- index(lstohlc)
				fmt <- "%Y-%m-%d"
				if ( nenddate > lstdate) {
					xupd1 <- yahooHistDailyFS(ctkr
							,fromd=lstdate, to=nenddate, ctyp)
				}
				
				if(   nrow(xupd1) > 0
						&& strftime(lstdate, format=fmt)==strftime(index(xupd1[1]),format=fmt)){
					xupd1 <- checkYahooDblquoteDaily(xupd1)## check and fix last double daily yahoo quote
					d1appended <- rbind(od1, xupd1[-1] )
					cinstd <- setIdatD1(cinstd,  d1appended)
					uw1 <- to.weekly(d1appended,indexAt='lastof',drop.time=droptime) ##update w1  old value startof
					index(uw1) <- as.POSIXct( index( uw1) )
					colnames(uw1) <- colnames(d1appended)
		            cinstd <-setIdatW1(cinstd,uw1)
				}
				idata[[m]]  <- cinstd   ## store updated cinstd into its holding container
			}
			return(idata)			
		})

#' @keywords internal
dldSplitsYahoo<-function(ticker, fromd,tod=Sys.time()){
	d1fmt<-"%Y-%m-%d"
    fromd <- strftime( as.POSIXct(fromd), format = d1fmt )
	tod <- strftime( as.POSIXct(tod), format = d1fmt )
	
	splits <- getSplits(ticker, from=fromd,to=tod)
	if (is.na(splits) || length(splits) == 0){
		return (NA)
	}else{
		return(splits) # xts object
	}	
}		
#' @keywords internal
dldDividendsYahoo<-function(ticker, fromd,tod=Sys.time()){
	d1fmt<-"%Y-%m-%d"
	fromd <- strftime( as.POSIXct(fromd), format = d1fmt )
	tod <- strftime( as.POSIXct(tod), format = d1fmt )
	
	dvds <- getDividends(ticker, from=fromd,to=tod)
	if (is.na(dvds) || length(dvds) == 0){
		return (NA)
	}else{
		return(dvds) # xts object
	}	
}		
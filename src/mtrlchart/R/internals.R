##internal functions  to  manage xts records

#' @keywords internal
addxtsrec  <-function(xtsobj, xtsrec, dtfmt="%Y-%m-%d"){

	if( length(xtsrec) == 0  ||
		nrow(xtsrec)  > 1 ){
		stop(paste("addxtsrec:validation. bad xtsrec param, expect xts object with 1 row"))
	}

	if( nrow(xtsobj) > 0 && 
	    !is.na( match(FALSE, colnames(xtsrec)== colnames(xtsobj)))){
		stop(paste("addxtsrec:validation. colnames of xtsrec do not match colnames of xtsobj"
			      ,colnames(xtsrec),collapse=" "))
	}

	if( is.null(nrow(xtsobj))) ## empty xts
	{
           xtsobj <- xtsrec
	}else{
		#check for duplicate element
		 if ( strftime(index(xtsrec),format=dtfmt) %in% strftime(index(xtsobj),format=dtfmt)  ){
		 	print(paste("attempt to add xtsrec with duplicate timestamp. Ignored..", index(xtsrec)))
		 }else{ # add non  duplicate element
		 	xtsobj <- rbind(xtsobj,xtsrec)
		 }
	}
	return(xtsobj)
}

#' @keywords internal
rmxtsrec  <-function(xtsobj, datetime, dtfmt="%Y-%m-%d")
{
	if( length(xtsobj) == 0 ) 
    {
		print(paste("rmxtsrec:validation. attempt to remove element from  an empty xts container. Ignored.."))
		return (xtsobj)
	}
    ##check if index of xtsrec exists
    if(!(strftime(as.POSIXct(datetime),format=dtfmt) %in% strftime(index(xtsobj),format(dtfmt)))){
		print(paste("rmxtsrec:validation. datetime index",datetime, "is not present in xts container"))
    }else{
       xtsobj <- xtsobj[ strftime(index(xtsobj),format=dtfmt) != strftime(as.POSIXct(datetime), format=dtfmt) ]   
    }
    return(xtsobj)
}

#' @kewords internal
forceTz <- function(posixctval, periodFrequency, tzval){
	dfmt <- "%Y-%m-%d %H:%M:%S"
	if (periodFrequency %in% c(86400,604800)){
		dfmt <- "%Y-%m-%d"
	}
	return(as.POSIXct(strftime(posixctval,format=dfmt),tz=tzval))
}

################################################
########### charting internal functions
################################################
#'function to get a left & right date subset for a chart

#' @keywords internal
chartDatesRange  <-function(edate=Sys.time(), dsubset=20, nbp=1, scf=1,tz='UTC' ){
    cwidth <- dsubset
    cdate <- as.POSIXct(edate,tz=tz)
    subrdate <- cdate - (nbp-1)* scf * cwidth * 86400
    subldate <- cdate  -  nbp * scf * cwidth * 86400
    return( list(leftdt=subldate, rightdt=subrdate))
}

#'function to get a left & right date subset for a chart as a string
chartDatesRange2String <- function(edate, dsubset, nbp, scf,tz='UTC'){
	fmt="%Y-%m-%d"
	dts <-chartDatesRange(edate=edate, dsubset=dsubset, nbp=nbp, scf=scf,tz=tz)
	out <- paste( strftime(dts$leftdt, format=fmt,tz=tz),'::', strftime(dts$rightdt, format=fmt,tz=tz),sep="")
	return(out)
}

#'function to convert to seconds the xts periodicity for bars from 1 min
getXtsSecondsFrequency <- function(xtsobj){

 p<-periodicity(xtsobj)
 if (p$units == 'mins'){
 	return (p$frequency * 60)
 }else{
 	return (p$frequency)
 }

}


#'function to get number of days of data window for different 
# intradaty time frames

getIntraDayWindowDays <-function(dfreq, isToint=FALSE){

    if(missing(dfreq)) { 
      	stop(paste("getIntraDayWindowDays:validation]"
					,"missing required parameter dfreq"))

    }
	ndayDefault <- 10
	dfokValues <- c(60,180,300,600,900,1800,3600,7200,14400,86400,604800)
	if (!dfreq %in% dfokValues){
		nday<-ndayDefault
		return(nday)
	}
	nday <- switch(as.character(dfreq),
			"60" = 0.125,  #  0.125 days = 3 hours of 1 min
			"180" = 0.4166 , # 0.4166 days = 10 hours of 3 min
			"300" = 1.2, #1.2 days  29 hours of 5 min
			"600" =  2.0,  # 2 days for  10min data
			"900" = 3.0,  # 3 days for 15 min data
			"1800" = 5.0,  #5 days for 30 mins data
			"3600" = 10.0, # 10 days for h1 data
			"7200" = 15.0, # 15 days for h2 data
			"14400" = 30.0,  # 30 days of h4 data
			"86400" = 250.0, # 250 days for d1 data
			"604800" = 1596  #  228 weeks, 4 years of w1 data
	 	)
	if(isToint){  nday <- ceiling(nday) }
	return (nday)
}

#' function to return ok values for data frequency
getDataFreqValidValues<-function(){
	dfokValues <- c(60,180,300,600,900,1800,3600,7200,14400,86400,604800)
	return (dfokValues)	
}

#' function to return a timeframe string 
getTimeFrameLabel<-function(xtsobj){
	dfreq<-getXtsSecondsFrequency(xtsobj)
	dfokValues <- getDataFreqValidValues()
	outStr <- ""
    defaultOutStr<-"NA"
	if (!dfreq %in% dfokValues){
		return(defaultOutStr)
	}
    outStr <- switch(as.character(dfreq),
    		"60"  = "m1",
 			"180" = "m3", 
			"300" = "m5",
			"600" =  "m10",
			"900" = "m15",
			"1200" = "m20",
			"1800" = "m30",
			"3600" = "h1",
			"7200" = "h2",
			"14400" = "h4", 
			"86400" = "d1",
			"604800" = "w1",
			"NA"
	 	)
    return(outStr)
}

#' adjEODmins is used in ctotperf  and blotterPnl
adjEODmins <-function(date,tz){
	pdate <- as.POSIXct(date,tz=tz)
	if ( as.numeric(strftime(pdate,format='%H')) 
	    +  as.numeric(strftime(pdate,format='%M'))
	    + as.numeric(strftime(pdate,format='%S')) ==0 ){
	          pdate <- pdate + 86399
	}
	return(pdate)
}

#'function to compute a unit of number of bars for intraday charting
getNbarsUnit <-function(xtsobj){

	dfreq <- getXtsSecondsFrequency(xtsobj)
	nday <- getIntraDayWindowDays(dfreq,isToint=FALSE)
	nbars <- floor(nday*24*3600/dfreq)
	return(nbars)
}
#'function  for d1 xtsobj  modifies posixDate for last EOD secod
chkDtModEodelta <-function(xtsobj, posixDate,dfreq=0){
	if(dfreq==0) { freq <- getXtsSecondsFrequency(xtsobj) }
	else  {freq <- dfreq }	
	xtz <- indexTZ(xtsobj)
	dfmt <- "%Y-%m-%d"
	outPosixDate <- posixDate
	if(freq == 86400){
   		#outPosixDate <- as.POSIXct( base::format(posixDate,format=dfmt), tz=xtz ) + (3600*23 + 60*59 + 0*1)
   		outPosixDate <- as.POSIXct( base::format(posixDate,format=dfmt) ) + (3600*23 + 60*59 + 0*1)
 
	}
	#browser()
	return(outPosixDate)
}



# pmktdata pkge
# get Set operations on list which contains istrument data + meta data
#
# instrument data list can have following fields:
#$name - instrument name
#$type  - instrument type,  equity, future
#$d1  - xts  daily data
#$w1  - xts weekly data
#$h1   - xts h1 data
#$m10  - xts m10
#$m1   - xts m1 data
#$meta - list() for meta dta
#$meta$divident
#$meta$split
#$meta$rollon  'expiry', 'volume'
#$meta$csequence
###############################################################################

#' @keywords internal
mkbareIdat<-function(iname){
	idl <- list()
	idl$name <- iname
	return(idl)
}

#' @keywords internal
setIdatDtype<-function(idl,dtype){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	okdtypes<-c("ix","eq","fut","opt")
	if(is.na(match(dtype,okdtypes))){
		stop(paste("idatOps:validation. input param dtype is of wrong value",dtype))
	}
	idl$dtype <- dtype
	return(idl)
}
#' @keywords internal
setIdatD1<-function(idl,xtsd){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.xts(xtsd)){
		stop(paste("idatOps:validation. input data param  is not of type xts"))
	}
	dfreq <- periodicity(xtsd)$frequency
	if(dfreq != 86400){
		stop(paste("idatOps:validation. supplied xts object is not of daily periodicity"))
	}
	idl$d1 <- xtsd
	return(idl)
}
#' @keywords internal
getIdatD1<-function(idl){
	if(!is.null(idl$d1)){
		return( idl$d1)
	}else{
		return(NA)
	}
}
#' @keywords internal
hasIdatD1<-function(idl){
	res <- TRUE
	if(is.null(idl$d1)) res <- FALSE
	if(length(idl$d1)==0) res <- FALSE
	return(res)
}
#' @keywords internal
setIdatW1<-function(idl,xtsd){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.xts(xtsd)){
		stop(paste("idatOps:validation. input data param  is not of type xts"))
	}
	dfreq <- periodicity(xtsd)$frequency
	#browser()
	if(dfreq != 604800){
		stop(paste("idatOps:validation. supplied xts object is not of weekly periodicity"))
	}
	idl$w1 <- xtsd
	return(idl)
}
#' @keywords internal
getIdatW1<-function(idl){
	if(!is.null(idl$w1)){
		return( idl$w1)
	}else{
		return(NA)
	}
}
#' @keywords internal
setIdatH1<-function(idl,xtsd){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.xts(xtsd)){
		stop(paste("idatOps:validation. input data param  is not of type xts"))
	}
	dfreq <- periodicity(xtsd)$frequency
	if(dfreq != 3600){
		stop(paste("idatOps:validation. supplied xts object is not of hourly periodicity"))
	}
	idl$h1 <- xtsd
	return(idl)
}
#' @keywords internal
setIdatM1<-function(idl,xtsd){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.xts(xtsd)){
		stop(paste("idatOps:validation. input data param is not of type xts"))
	}
	dfreq <- periodicity(xtsd)$frequency
	dlabel <- periodicity(xtsd)$label
	if( !(dfreq ==  1 && dlabel == "minute") ) {
		stop(paste("idatOps:validation. supplied xts object is not of minute periodicity"))
	}
	idl$h1 <- xtsd
	return(idl)
}


#' @keywords internal
setIdatMetaSplits<-function(idl,xtsi){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.xts(xtsi)){
		stop(paste("idatOps:validation. input data param  is not of type xts"))
	}
	if(nrow(xtsi)==0){
		stop(paste("idatOps:validation. empty supplied data "))
	}
    if(is.null(idl$meta))
	{
		idl$meta <- list()
	}
	idl$meta$split <- xtsi
	return(idl)
}
#' @keywords internal
getIdatMetaSplits<-function(idl){
	if(!is.null(idl$meta$split)){
		return( idl$meta$split)
	}else{
		return(NA)
	}
}
#' @keywords internal
setIdatMetaDividends<-function(idl,xtsi){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.xts(xtsi)){
		stop(paste("idatOps:validation. input data param  is not of type xts"))
	}
	if(nrow(xtsi)==0){
		stop(paste("idatOps:validation. empty supplied data "))
	}
	if(is.null(idl$meta))
	{
		idl$meta <- list()
	}
	idl$meta$dividend <- xtsi
	return(idl)
}

#' @keywords internal
setIdatMetaInterdayTimeZone<-function(idl,tz){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}

	if(is.null(idl$meta))
	{
		idl$meta <- list()
	}
	idl$meta$interdaytimezone <- tz
	return(idl)
}
#' @keywords internal
setIdatMetaIntradayTimeZone<-function(idl,tz){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}

	if(is.null(idl$meta))
	{
		idl$meta <- list()
	}
	idl$meta$intradaytimezone <- tz
	return(idl)
}


#' @keywords internal
getIdatMetaDividends<-function(idl){
	if(!is.null(idl$meta$dividend)){
		return( idl$meta$dividend)
	}else{
		return(NA)
	}
}
#' @keywords internal
setIdatMetaRollon<-function(idl,rollon){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.character(rollon)){
		stop(paste("idatOps:validation. input data param  is not of type character"))
	}

	if(is.null(idl$meta))
	{
		idl$meta <- list()
	}
	idl$meta$rollon <- rollon
	return(idl)
}

#' @keywords internal
getIdatMetaRollon<-function(idl){
	if(!is.null(idl$meta$rollon)){
		return( idl$meta$rollon)
	}else{
		return(NA)
	}
}

#' @keywords internal
setIdatMetaVolccount<-function(idl,dblvol){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.logical(dblvol)){
		stop(paste("idatOps:validation. input data param  is not of type logical"))
	}
	if(is.null(idl$meta))
	{
		idl$meta <- list()
	}
	if(dblvol){
	  idl$meta$volccount <- 2
  }else{
	  idl$meta$volccount <- 1
  }
	return(idl)
}

#' @keywords internal
getIdatMetaVolccount<-function(idl){
	if(!is.null(idl$meta$volccount)){
		return( idl$meta$volccount)
	}else{
		return(NA)
	}
}

#' @keywords internal
setIdatMetaGluedseq<-function(idl,gseq){
	if(!is.list(idl)){
		stop(paste("idatOps:validation. input param is not of type list"))
	}
	if(!is.list(gseq)){
		stop(paste("idatOps:validation. input data param  is not of type list"))
	}

	if(is.null(idl$meta))
	{
		idl$meta <- list()
	}
	idl$meta$gluedcontracts <- gseq
	return(idl)
}
#' @keywords internal
getIdatMetaGluedseq<-function(idl){
	if(!is.null(idl$meta$gluedcontracts)){
		return( idl$meta$gluedcontracts)
	}else{
		return( list() )
	}
}

#'Read spreadsheet data from Google Docs to R workspace
#'Author: Kay Cichini
#' @export
google_ss <- function(gid = NA, key = NA)
    {
    if (is.na(gid)) {stop("\nWorksheetnumber (gid) is missing\n")}
    if (is.na(key)) {stop("\nDocumentkey (key) is missing\n")}
    require(RCurl)
    url <- getURL(paste("https://docs.google.com/spreadsheet/pub?key=", key,
                        "&single=true&gid=", gid, "&output=csv", sep = ""),
                  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    b<- read.csv(textConnection(url), header = T, sep = ",", stringsAsFactors=FALSE)
    return(b)
    }

#' @keywords internal
stripquotes <-function(df, quotepattern="\'" ){

   for(j in 1:ncol(df) ){
       ccol <- df[,j]
       scol <-  gsub(quotepattern,"", ccol )
       df[,j] <- scol
   }
   return(df)
}

#' returns an estimation for a contract period in days
#' based on a yarly list of contract months
#' x is a month of a contract to compute its data period  
#
#' @keywords internal
getEvalContractPeriod <-function(x, contrYearSched){
   contrYearSched_s  <- c(0, contrYearSched[-length(contrYearSched)] )
   contrDiff <-  contrYearSched -  contrYearSched_s
   midx <- match(x, contrYearSched)
   if(is.na(midx)) { midx <- 1}
   return (31 * contrDiff[midx] )  

}

#' called from appendXtsSerie
#
#' @keywords internal
isEqualOhlcBar <-function(b1, b2, chkVol=TRUE)
{
  equal <- FALSE
  b1 <- try.xts(b1)
  b2 <- try.xts(b2)

  if ( as.numeric(b1$High) == as.numeric(b2$High) &&
       as.numeric(b1$Low) == as.numeric(b2$Low) &&
       as.numeric(b1$Close) == as.numeric(b2$Close) )
        { equal <- TRUE }
  if (chkVol)
  {
      if ( as.numeric(b1$Volume) != as.numeric(b2$Volume))
           equal <- FALSE
  }
  return(equal)
}

#' called from ContractSeqList:nCSLfromcfg , Mldata
#' parser of configuration file for ContractSeqList
#'
#' @keywords internal
	parseCSListCfg <-function(fn){
		if( !file.exists(fn)){
			msg <- paste("contrat sequence list config ",fn,"is missing")
	        stop(paste("parseCSListCfg: validation",msg))
	    }
	    df <- read.table(file=fn,sep="="
	    	           ,col.names=c('key','value')
	    	           ,strip.white=TRUE
	    	           ,na.strings="NA"
	    	           ,stringsAsFactors=FALSE)
	    inames <- df$value[which(df$key == 'inam')]
	    datadir <- df$value[which(df$key =='datdir')]
	    datsrc <- df$value[which(df$key =='dsrc')]
	    return( list(inames=inames,datadir=datadir) )
	}


#' append xts serie
#' works on bars daily, h1, m1
#' @keywords internal
appendXtsSerie <-function(xdat, nxdat ,seriename, type='bar', endoffset=3
	                      ,dts=NA,dte=NA,verbose=FALSE)
{

    if ( periodicity(xdat)$frequency != periodicity(nxdat)$frequency ||
        periodicity(xdat)$units  != periodicity(nxdat)$units ) {

 		print(paste("warning: appendXtsSerie"
				  ,"xts data to be appended has element gaps."))
		if( min(as.numeric(index(nxdat)[-1] - index(nxdat)[-length(index(nxdat))])) !=
		    min(as.numeric(index(xdat)[-1] - index(xdat)[-length(index(xdat))])) ){
 		stop(paste("appendXtsSerie: validation"
				  ,"xts data to be appended has a different minimum periodicity"))
		}		   	
    }
    if(is.na(match(index(last(xdat)), index(nxdat)))){
 		stop(paste("appendXtsSerie validation"
				  ,"xts data and new xts data appear to be disjoined series.")) 	
    }

   	s1fmt <- '%Y-%m-%d %H:%M:%S'
   	interval <- periodicity(xdat)$frequency
    flastindex <- index(last(xdat))

    cserieIndex1  <- index(last(xdat ,endoffset)[1]) #last -2
    cserieIndex2  <- index(last(xdat ,endoffset)[2]) #last -1


    newseriePos1 <- match(cserieIndex1, index(nxdat) )
    newseriePos2 <- match(cserieIndex2,index(nxdat)  )
 
    foundGluePoint <- FALSE
    wmsg <- ''
    if (!is.na(newseriePos1) &&  !is.na(newseriePos2)  ) {
        if(type == 'bar'){     
          if (    isEqualOhlcBar(xdat[cserieIndex1], nxdat[newseriePos1],chkVol=TRUE)
              &&  isEqualOhlcBar(xdat[cserieIndex2], nxdat[newseriePos2],chkVol=TRUE)){

          	foundGluePoint <- TRUE
          }else{
          	foundGluePoint <- FALSE
          	wmsg <- paste('mismatch or unequal bars in a xts serie', seriename
          		          ,'to be appended at glue points'
          		          ,strftime(cserieIndex1,format=s1fmt)
          		          ,strftime(cserieIndex2,format=s1fmt))
          }
        }
        if(foundGluePoint)
        {
            appndxts <- nxdat[(newseriePos2+1):nrow(nxdat)]
            cseriesub <- paste('::', cserieIndex2,sep='')
            #browser()
            curdata <- xdat[cseriesub]
            xdat <- rbind(curdata, appndxts)
            nlastindex <- index(last(xdat))

            if(verbose)
            {
                msg <-  paste('updated bar serie for ', seriename, interval
                           ,'from' , flastindex ,'to', nlastindex
                           ,'by adding', nrow(appndxts), 'elements' )

          		if (!is.na(dts) && !is.na(dte)) 
          		msg <- paste(msg, "in",formatC(as.numeric(dte-dts),digits=3), "secs",sep=" ")
          		print(msg)
          	}
        }
    }
    if( !foundGluePoint && verbose){
    	print (wmsg)
    }
    return(xdat)
}


#' substract a year from xts keep same week days 
#' 
#' @keywords internal
addDtWkPosixCt <-function(dtct, ny = 0, nd=0 )
{
    odtct <- dtct + dyears(ny) + days(abs(ny)%%7) +days(nd)
	return(odtct)
}


#' substract a year xts serie
#' 
#' @keywords internal
addDtXtsSerie <-function(xtso, ny = 0, nd=0 )
{
	if(!is.xts(xtso)){
		stop(paste("subsYrXtsSerie: validation"
				  ,"xtso param is not of type xts"))
	}

    xidx <-index(xtso)
    nxidx <- addDtWkPosixCt(xidx, ny=ny, nd=nd)
	return(xts(xtso, order.by=nxidx))
}

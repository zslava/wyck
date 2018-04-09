#' Class for instrument's contract sequence market data for futures markets
#'
#' S4 container which can hold daily and  | or intraday market data for several
#' data providers. Contains methods to manage the list of instruments and to manage the
#' associated market data
#' @rdname ContractSeq-class
#' @name ContractSeq
#' @exportClass ContractSeq
setClass(Class="ContractSeq"
			,representation(instName="character"
						   ,tickerPref="character"
						   ,contSuff="character"
						   ,yrContracts="numeric"
						   ,topstoredir="character"
						   ,offsetd1fixed="numeric"
						   ,datasrc="character"
						   ,lastDataDaily="POSIXct"
						   ,cslogger="character"
						   ,contrseq="data.frame"
						   ,contrefdata="list"
						   ,data="list"
						   ,intrdconf="list"
						   ,intrdata="list"
						   ,tkdata="list"
						   ,rollrule="list"
		   )
			,prototype(instName= 'undefined instrument'
					 ,tickerPref='undefined ticker'
					 ,contSuff="#"
					 ,yrContracts=c(3,6,9,12)
					 ,topstoredir="/tmp"
					 ,offsetd1fixed=30
					 ,datasrc="undefined"
					 ,lastDataDaily=as.POSIXct(NA)
					 ,cslogger=as.character(NA)
					 ,contrseq=as.data.frame(matrix(NA)
					 ,contrefdata=list()
					 ,data=list()
					 ,intrdconf=list(NA)
					 ,intrdata=list(NA)
					 ,tkdata=list(NA)
					 ,rollrule=list(NA) )
			)
			,validity=function(object){
			   #cat("~~~ ContractSeq: validity inspector ~~~ \n")
			   if(!is.character(object@instName)){
				  stop("[ContractSeq: validation] instName is not a character")
			   }else if(nrow(object@contrseq) < 1){
				  stop("[ContractSeq: validation] contrseq dataframe is empty")
			   }
			   return(TRUE)
			}
	   )


###############
# user function constructor of contractSeq of contrseq from startYear to endYear
##############
#' @export
crBareContractSeq <-function(name, ticker, ycontracts, cstart, cend
	                        ,datasrc='dtn',topdir='/tmp',contsuff="#"){

	o <- new("ContractSeq", instName=name, tickerPref=ticker
			,yrContracts=ycontracts, contSuff=contsuff)

	yearStart <- as.numeric( strftime(as.POSIXct(cstart),format="%Y"))
	monthStart <- as.numeric( strftime(as.POSIXct(cstart),format="%m"))
	contractStart <- as.POSIXct( paste(yearStart,monthStart,"1",sep="-")  )
	##dfutEnd <- as.POSIXct(cend) + 86400 * 30 * 7 ## 7 months forward in the future
	dfutEnd  <- as.POSIXct(cend)
	yearEnd <- as.numeric(strftime(dfutEnd,format="%Y"))
	monthEnd <- as.numeric(strftime(dfutEnd,format="%m"))
	contractEnd <- as.POSIXct( paste(yearEnd,monthEnd,"1",sep="-")  )

	#browser()

	cchain <- mkContractChain(yearStart,yearEnd,ycontracts)
	pcchain <- as.POSIXct(cchain)
	##apply left  , right limits
	scchain <- pcchain[ pcchain >=  contractStart ]
	scchain <- scchain[ scchain <= contractEnd ]
	contractmonths <- strftime(scchain, format="%Y-%m-%d")

	inst<-  rep(o@instName, length(contractmonths))
	m <- cbind( inst , contractmonths )
	df <- as.data.frame(m,stringsAsFactors=F)
	colnames(df) <-c("inst", "contr_month")
	o@contrseq  <- df
	o@topstoredir<-topdir
	o@datasrc<-datasrc

	##set logger to default level info
	resetLogger(o, "INFO")

	return(o)
}

#' internal function to create a sequence of futures contracts
#' @keywords internal
mkContractChain <-function(yrstart, yrend, yrcontracts){
	d1fmt<-'%Y-%m-%d'
	chain <- c()
	for (y  in yrstart:yrend)
	{
		for (j in 1:length(yrcontracts))
		{
			strContrMonth <- paste(y,yrcontracts[j],"1",sep="-")
			chain <- c(chain, strContrMonth)
		}
	}
	return(chain)
}


#' @docType method
#' @export
setGeneric( name='addFutureContracts',function(.Object, nmonths=6  ){standardGeneric("addFutureContracts")})
#' @aliases addFutureContracts,ContractSeq,ANY-method
setMethod('addFutureContracts', 'ContractSeq',function(.Object, nmonths=6 ){
			nameObject <- deparse(substitute(.Object))

			d1fmt<-'%Y-%m-%d'

			yrctrs <- .Object@yrContracts

			cdf <- getContractSequence(.Object)
			iname <- getInstrumentName(.Object)

			lcontract <- cdf$contr_month[nrow(cdf)]
			futlimitday <- as.POSIXct(lcontract) + 86400 * 31 * nmonths  ## 6 months forward from last contract month
 
			lctrYear <- as.numeric(strftime(as.POSIXct(lcontract),format="%Y"))
			futlimYear <- as.numeric(strftime(futlimitday,format="%Y"))
			cchain <- mkContractChain(lctrYear,futlimYear,yrctrs)
			pcchain <- as.POSIXct(cchain)
			scchain <- pcchain[ pcchain > as.POSIXct(lcontract)]
			scchain <- scchain[ scchain < futlimitday ]

			if(length(scchain)>0){
				ncols <- ncol(cdf)
				nnas <- ncols -2
				for(j  in 1:length(scchain) ){
					cdfline <- c( iname, strftime(scchain[j],format=d1fmt), rep(NA,nnas) )
					cdf <- rbind(cdf,cdfline)
				}
			}
			setContractSequence(.Object)<-cdf

			assign(nameObject,.Object,envir=parent.frame())
			return (invisible())

		})


###############
# initialize dtn
##############
#' @export
dtnConf  <- function(wenv="vm")
{
	iqfrunner <- ''
	if (wenv =='local') #case for win machine which runs iqfeed
	{
		iqConf();
		print(paste("applying iqfeedconfig for local win machine"))
	}
	if (wenv == 'cern')
	{
		iqfrunner<-"lemvig.cern.ch"
		iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
		print(paste("applying iqfeedconfig for ", iqfrunner))
	}
	if (wenv == 'home')
	{
		iqfrunner<-"192.168.1.4"
		iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
		print(paste("applying iqfeedconfig for ", iqfrunner))
	}
	if (wenv == 'vm')
	{
		iqfrunner<-"192.168.50.129"
		iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
		print(paste("applying iqfeedconfig for ", iqfrunner))
	}

	if (wenv == 'vmp1')
	{
		iqfrunner<-"10.211.55.3"
		iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
		print(paste("applying iqfeedconfig for ", iqfrunner))
	}
	if (wenv == 'vmp2')
	{
		iqfrunner<-"10.211.55.4"
		iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
		print(paste("applying iqfeedconfig for ", iqfrunner))
	}

}



#' loads saved contractSeq object from rdat file
#'
#' @export
loadseq <-function(rdatfile){
	load(rdatfile)
	return(.Object)
}

#' @export
readcs<-function(iname,topstoredir="~/googledrive/mkdata/dtn/contrseq"){

	fn<-paste(topstoredir,"/",iname,"/","rdat","/",iname,".rdat",sep="")
	if( !file.exists(fn)){
		msg <- paste("instrument",iname,"missing contract seq rdat file",fn)
		print(msg)
		cs <- NA
	}else{
		cs <- loadseq(fn)
		resetLogger(cs,"INFO")
		chkMeta(cs)
	}
	return(cs)
}


#' loa sequence structure and separately contracts data
#'
#' @export
nreadcs<-function(iname,topstoredir="~/googledrive/mkdata/dtn/contrseq"
				 ,freq=60,intrd="now",rollon="volume",verbose=T){
	cs <- NA
	fn<-paste(topstoredir,"/",iname,"/","rdat","/",iname,"_nodata",".rdat",sep="")
	if( !file.exists(fn)){
		msg <- paste("instrument",iname,"missing contract seq nodata rdat file",fn)
		stop(paste("nreadcs: validataion",msg))
	}
	csbare <- loadseq(fn)
	resetLogger(csbare,"INFO")
	chkMeta(csbare)
	## load monthly contract data
	itickerPref <- getTickerPref(csbare)
	isqdir <- paste(csbare@topstoredir, iname, sep="/") 
	datadir <- paste(isqdir,'contracts',sep="/")  
	cdf <- getContractSequence(csbare)
	metacmonths <- as.character(cdf$contr_month)
	dfreq <- "86400"
	ndata <- list()
	for(j in 1:length(metacmonths) ){
	   cmonth <- metacmonths[j]
	   ccontr <- bareContract(iname, itickerPref, cmonth )
	   contrsuff <- getContractSuffix(ccontr)
	   contractdir <- paste(datadir, contrsuff, sep="/")
	   cfn <- paste(contractdir,"/",iname,"_",contrsuff,"_",dfreq,".rdat",sep="")
	   if(!file.exists(cfn)){
		   wmsg <- paste("no d1 data rdat file found for contract",cmonth)
		   logwarn(wmsg,logger=getLoggername(csbare))
		   next
		}
		load(cfn)
		ndata[[ length(ndata)+1 ]] <- d1contrdat
		imsg <- paste("d1 data loaded from rdat file  for contract",cmonth)
		if(verbose) { loginfo(imsg,logger=getLoggername(csbare)) }
	}
	csbare@data <- ndata 

	##new dev also load intraday data
	if(intrd == "all"){
		loadIntraday(csbare,interval=freq,alldata=TRUE,verbose=verbose)
	}else if(length(grep("::",intrd))>0){
	   icontracts <- unlist(strsplit(intrd,"::"))
	   #browser()
	   loadIntraday(csbare,interval=freq
					 ,ctr_start = icontracts[1]
					 ,ctr_end  = icontracts[2])    	
	}else{  ##intrd=="now"
		nmnths <- 3 #lenght of data history in months
		noffset <- floor(length(csbare@yrContracts) / nmnths)
		loadIntraday(csbare,interval=freq
					 ,ctr_start = getUnexpiredContract(csbare, ctime = Sys.time(), n = 1-noffset)
					 ,ctr_end  = getUnexpiredContract(csbare, ctime = Sys.time(), n = 2))
	}    

    ##set ref intraday continuous serie
    setRefContIntradaySerie(csbare,rollon=rollon)

	return(csbare)  
}

#' @export
csfromcfg<-function(fname, cfgdir="~/googledrive/mkdata/dtn/cfgseq"){
	fn<-paste(cfgdir,"/",fname,sep="")
	if( !file.exists(fn)){
		msg <- paste("contrat sequence config ",fn,"is missing")
		stop(paste("csfromcfg: validation",msg))
	}
	t <- read.table(file=fn,sep="=", row.names=1, strip.white=TRUE
				   ,na.strings="NA", stringsAsFactors=FALSE)
	
	dsrc  <- t['dsrc',1]
	stored <- t['dstore',1]
	iname <- t['name',1]
	tkpref <- t['tkrpref',1]
	yrcontrstr <- t['yrcontr',1]
	#browser()
	yrcontr <- as.numeric(strsplit(yrcontrstr,",")[[1]])
	scontr <- t['scontr',1]
	econtr <- t['econtr',1]
	roll_wnd <- as.numeric( t['roll_wnd',1] )
	roll_vpct <- as.numeric( t['roll_volth',1] )
	n_futmonths <- as.numeric( t['nfuturemonths',1] )
   

	cs <- crBareContractSeq (name=iname, ticker=tkpref, ycontracts=yrcontr, cstart=scontr, cend=econtr, datasrc=dsrc,topdir=stored,contsuff="#")

	#set roll rule
	setRegularRollRule(cs, rollprd=roll_wnd, fmvolpct=roll_vpct  )

	
	addFutureContracts(cs, nmonths=n_futmonths)
	 
	#intraday config params is factored in separate function
	cs@intrdconf <- intradaycfg(fname=fname,cfgdir=cfgdir)

	return (cs)
 
}

#' @export
intradaycfg<-function(fname, cfgdir="~/googledrive/mkdata/dtn/cfgseq"){
	fn<-paste(cfgdir,"/",fname,sep="")
	if( !file.exists(fn)){
		msg <- paste("contrat sequence config ",fn,"is missing")
		stop(paste("intradaycfg: validation",msg))
	}
	t <- read.table(file=fn,sep="=", row.names=1, strip.white=TRUE
				   ,na.strings="NA", stringsAsFactors=FALSE)
 
	intradayfreqArray <- t['intrdfreq',1]
	intrdfreq<-  as.numeric(strsplit(intradayfreqArray,",")[[1]])
	intrddaydoublevolumeCrit <-t['intrddaydoublevolume',1]
	intrdoutcryWind <- t['intrdoutcry',1]
	intrdoutcryTimezone <- t['intrdoutcrytz',1]

	tk_siz <- as.numeric(t['tick_size',1])
	tk_pric <- as.numeric(t['tick_price',1])
	imnt_curr <- t['imnt_curr',1]

	intrdconf <- list()
	intrdconf$intradayfreq <- intrdfreq
	if(intrddaydoublevolumeCrit == 'yes') 
		{intrdconf$daydoublevolume <-TRUE}
	else                               
		{intrdconf$daydoublevolume <-FALSE}

	intrdconf$outcrysession <- intrdoutcryWind
	intrdconf$outcrytimezone <- intrdoutcryTimezone
	 
	intrdconf$tick_size <- tk_siz
	intrdconf$tick_price <- tk_pric
	intrdconf$imnt_currency <- imnt_curr


	return(intrdconf)
}


###getters and setters

#####instName
setGeneric( name='getInstrumentName'
		   ,function(object){standardGeneric("getInstrumentName")})
setMethod('getInstrumentName', 'ContractSeq'
		  ,function(object){
			return(object@instName)
		  })

  #####tickerPref
  setGeneric( name='getTickerPref'
		  ,function(object){standardGeneric("getTickerPref")})
  setMethod('getTickerPref', 'ContractSeq'
		  ,function(object){
			  return(object@tickerPref)
		  })



  setGeneric( name='getLoggername'
		  ,function(object){standardGeneric("getLoggername")})
  setMethod('getLoggername', 'ContractSeq'
		  ,function(object){
			  return(object@cslogger)
		  })

#' @docType method
#' @export
  setGeneric( name='getTopstoredir'
		  ,function(object){standardGeneric("getTopstoredir")})
#' @aliases getTopstoredir,ContractSeq,ANY-method
  setMethod('getTopstoredir', 'ContractSeq'
		  ,function(object){
			  return(object@topstoredir)
		  })
#' @export
#' @docType methods
  setGeneric(name='setTopstoredir<-'
		  ,function(object,value){standardGeneric('setTopstoredir<-')})
#' @aliases setTopstoredir,ContractSeq,ANY-method
  setReplaceMethod('setTopstoredir'
		  ,'ContractSeq'
		  ,function(object,value){
			  if (!file.exists(value) ){
				  stop(paste("[ContractSeq:setTopstoredir validation]"
								  ,"supplied param for topstoredir does not exist."))
			  }
			  object@topstoredir <- value
			  return(object)
		  })

  #' @docType methods
  setGeneric(name='setRollrule<-'
		  ,function(object,value){standardGeneric('setRollrule<-')})
#' @aliases setRollrule,ContractSeq,ANY-method
  setReplaceMethod('setRollrule'
		  ,'ContractSeq'
		  ,function(object,value){
			  if (!is.list(value) ){
				  stop(paste("[ContractSeq:setRollrule validation]"
								  ,"supplied value is not of type list"))
			  }
			  object@rollrule <- value
			  return(object)
		  })


#' @export
  setGeneric( name='setRegularRollRule'
		  ,function(.Object,rollprd,fmvolpct){standardGeneric("setRegularRollRule")})
#' @aliases mkRegRollRule,ContractSeq,ANY-method
  setMethod('setRegularRollRule', 'ContractSeq'
		  ,function(.Object,rollprd,fmvolpct){
			  if(missing(rollprd) ){
				  stop("[ContractSeq:setRegularRollRule validation] missing rollprd param")
			  }
			  if(missing(fmvolpct) ){
				  stop("[ContractSeq:setRegularRollRule validation] missing fmvolpct param")
			  }
			  nameObject <- deparse(substitute(.Object))
			  ##internal func
			  mkregroll<-function(x,prd,vpct){
				  l<-list(cmonth=x
						 ,rollprd=prd
						 ,fmvolpct=vpct )
				 #browser()
				 return(l)
			  }
			  rules <-lapply(as.list(.Object@yrContracts), FUN=mkregroll, prd=rollprd,vpct=fmvolpct )
			  .Object@rollrule <- rules
			  assign(nameObject,.Object,envir=parent.frame())
			  return (invisible())
		  })


#' @docType method
#' @export
  setGeneric( name='resetLogger',function(.Object,level="INFO"){standardGeneric("resetLogger")})
#' @aliases resetLogger,ContractSeq,ANY-method
  setMethod('resetLogger', 'ContractSeq',function(.Object,level="INFO"){
			  nameObject <- deparse(substitute(.Object))
			  oklevels=c("WARNING","INFO","DEBUG")
			  if(is.na(match(level,oklevels)) ){
				  stop("[ContractSeq:setLogger validation] bad value for level logging param")
			  }
			  ##logfilename
			  iname <- getInstrumentName(.Object)
			  isqdir <- paste(.Object@topstoredir, iname, sep="/") #topdir for sequence
			  logdir <- paste(isqdir,"/","log", sep="")

			  if( !file.exists(logdir)) { dir.create(logdir,recursive=TRUE) }
			  logfname <- paste(logdir, "/", iname, ".log",sep="")

			  logid <- paste(iname,"logger",sep="_")

			  ##redefine Logger object
			  lg <- getLogger(logid )
			  lg$setLevel(level)
			  lg$addHandler(writeToFile, file=logfname, level=level)
			  lg$addHandler(writeToConsole, level=level)

			  .Object@cslogger <- logid

			  assign(nameObject,.Object,envir=parent.frame())
			  return (invisible())
})


#####d1 contract data offset
#' set config from historic data for DTN datasource
#' @keywords internal
#' @docType methods
setGeneric( name='getD1offset'
		  ,function(object){standardGeneric("getD1offset")})
#' @aliases getD1offset,ContractSeq,ANY-method
setMethod('getD1offset', 'ContractSeq'
		  ,function(object){
			  d1offset <- ceiling(365/length(object@yrContracts)) + object@offsetd1fixed
			  return(d1offset)
})



####contrseq
setGeneric( name='getContractSequence'
		   ,function(object){standardGeneric("getContractSequence")})
setMethod('getContractSequence', 'ContractSeq'
		  ,function(object){
			return(object@contrseq)
		  })
setGeneric(name="setContractSequence<-"
		  ,function(object,value){standardGeneric("setContractSequence<-")})
setReplaceMethod( f="setContractSequence"
				  ,signature="ContractSeq"
				  ,definition=function(object,value){
					object@contrseq <-as.data.frame(value,stringsAsFactors = FALSE)
					##validate input
					if ( nrow(as.data.frame(value)) < 1) {
					  stop("[ContractSeq:setContractSequence validation] check contract sequence data.frame")
					}
					return (object)
				  })

#' set  ref glued serie intraday
#' @export
#' @docType methods
setGeneric(name='setRefContIntradaySerie'
		   ,function(.Object, rollon="volume")
		   {standardGeneric("setRefContIntradaySerie")})
#' @aliases setRefContIntradaySerie,ContractSeq,ANY-method
setMethod('setRefContIntradaySerie','ContractSeq'
	,function(.Object, rollon="volume"){
		nameObject <- deparse(substitute(.Object))
		
	    reffreq <- .Object@intrdconf$intradayfreq[1]   

	    start_idx <- match( as.character(reffreq)
	    	              ,unlist(lapply(.Object@intrdata[[1]]$idat
	    	              	            ,function(x){x$freq})))
	    startdt <- 	index(first(.Object@intrdata[[1]]$idat[[start_idx]]$xdat))
	    eid <- length(.Object@intrdata)              	
	    end_idx <- match( as.character(reffreq)
	    	              ,unlist(lapply(.Object@intrdata[[eid]]$idat
	    	              				,function(x){x$freq})))
	    enddt <- 	index(last(.Object@intrdata[[eid]]$idat[[end_idx]]$xdat))
        refIntradaySerie<-  getGluedserie(.Object,stime=startdt,etime=enddt
						  					,freq=reffreq,rollon=rollon
						  					,dblvol=FALSE,idxchk=FALSE)
        .Object@contrefdata$intraday <- list(freq=reffreq,xdat=refIntradaySerie)
		 assign(nameObject,.Object,envir=parent.frame())
		 return ( invisible() )
})


#' get  ref glued serie intraday
#' @export
#' @docType methods
setGeneric( name='getRefContserie'
		   ,function(object,freq){standardGeneric("getRefContserie")})
setMethod('getRefContserie', 'ContractSeq'
		  ,function(object,freq ){
		  	if(missing(freq)){
				stop(paste("ContractSeq:getRefContserie validation:
				     ,missng freq param"))		  		
		  	}
		  	freqs <- object@intrdconf$intradayfreq
			if( is.na(match(freq,c(freqs,86400)))) {
				stop(paste("ContractSeq:getNorefIntraContserie validation:
				     ,suplied freq param is not within configured values"
				     ,paste(freqs,collapse=" ")))
			}
			if(freq == 86400){
				return(object@contrefdata$bd1)
			}
			refintraserie <- object@contrefdata$intraday$xdat 
			intraserie <- to.minutes(refintraserie ,k=freq/60)
           	return(intraserie)
			})


##biz methods
###### get any specified contract from contrseq
#' @export
#' @docType methods
setGeneric( name='getContract'
		   ,function(object,contrmonth){standardGeneric("getContract")})
setMethod('getContract', 'ContractSeq'
		  ,function(object,contrmonth ){
			if(missing(contrmonth)){
				stop("ContractSeq:getContract validation] contrmonth unspecified")
			}
			cdf <- object@contrseq
			rowid <-  match(contrmonth, cdf$contr_month)
			if (is.na(rowid)){
				return(NA)
			}
			cc <- bareContract(object@instName, object@tickerPref, contrmonth)
			setExpiry(cc) <- cdf$expiry[rowid]
			setOnVolumeRoll(cc)<-cdf$volroll[rowid]
			return(cc)

		  })


##' get contract with closest future expiry date from ctime
#'
#' @export
#' @docType methods
setGeneric( name='getUnexpiredContract'
		   ,function(object,ctime=Sys.time(),n=1){standardGeneric("getUnexpiredContract")})
#' @aliases getUnexpiredContract,ContractSeq,ANY-method
   setMethod('getUnexpiredContract', 'ContractSeq'
		  ,function(object,ctime=Sys.time(), n=1 ){

			cdf <- object@contrseq
			cday <- strftime( as.POSIXct(ctime) , format="%Y-%m-%d" )
			tdif <- as.POSIXct(cdf$expiry) - as.POSIXct(cday )
			crit <- match(TRUE, tdif >= 0 )
			if(is.na(crit)){ ##cday is more recent than last found expiry
			  rowid <- first(which( is.na(cdf$expiry) == TRUE )) + (n-1)
			}else{
			  rowid <- crit	+ (n-1)
			}
			cmonth <- cdf$contr_month[rowid]
			return(cmonth)
		  })

##' get expected expiry date for an unexpired contract
#'
#' @export
#' @docType methods
  setGeneric( name='getUnexpiredExpectedExpiry'
		  ,function(object,ctime=Sys.time()){standardGeneric("getUnexpiredExpectedExpiry")})
#' @aliases getUnexpiredExpectedExpiry,ContractSeq,ANY-method
  setMethod('getUnexpiredExpectedExpiry', 'ContractSeq'
		  ,function(object,ctime=Sys.time() ){
			  d1fmt<-"%Y-%m-%d"
			  cdf <- object@contrseq
			  ccunexp<-getUnexpiredContract(object,ctime=ctime,n=1)
			  rowid<-match(ccunexp, cdf$contr_month)
			  lstexpired <- cdf$expiry[rowid-1]
			  if(is.na(lstexpired)){
				  stop(paste("ContractSeq::getUnexpiredExpectedExpiry validation"
				  ,"Previous contract expiry can not be determined. Check contracts data."))
			  }
			  expectExpir <- as.POSIXct(lstexpired) + 86400 * ceiling(365/length(object@yrContracts))
			  expectExpirStr <- strftime(expectExpir,format=d1fmt)
			  return(expectExpirStr)
		  })



#' print liqidity data over last 6 days v2
#'
#' @export
#' @docType methods
		  setGeneric( name='printLiquidity'
				  ,function(object,ctime=Sys.time(),n=6){standardGeneric("printLiquidity")})
#' @aliases printLiquidity,ContractSeq,ANY-method
		  setMethod('printLiquidity', 'ContractSeq'
				  ,function(object,ctime=Sys.time(), n=10 ){
					  d1fmt<-"%Y-%m-%d"
					  iname <- getInstrumentName(object)
					  cdf <- getContractSequence(object)
					  contxdat <- object@contrefdata$bd1
					  datamonths <- unlist(lapply(object@data,function(x){x$month}))

					  eidx <- match( strftime(as.POSIXct(ctime),format=d1fmt), strftime(index(contxdat),format=d1fmt) )
					  if(is.na(eidx)){
						  eidx <- nrow(contxdat)  ##last element
					  }
					  sidx <- eidx -n
					  refprd <- contxdat[sidx:eidx]
					  refxts<-xts(rep(NA,nrow(refprd)), order.by=index(refprd)  )
					  stime <- strftime( index(contxdat)[sidx], format=d1fmt)

					  dsub<-paste(strftime(index(first(refxts)),format=d1fmt)
							,"::",strftime(index(last(refxts)),format=d1fmt),sep="")

					  cms <- getUnexpiredContract(object,ctime=stime,n=1)
					  cme <- getUnexpiredContract(object,ctime=ctime,n=1)
					  cmenext <- getUnexpiredContract(object,ctime=ctime,n=2)
					  cids <- ( which(cdf$contr_month == cms) : which(cdf$contr_month == cmenext) )

					  volblock <-c()
					  for(j in 1:length(cids) ){
						  cid<-cids[j]
						  cc<-cdf$contr_month[cid]
						  clbl<-  paste("Vo", getContractSuffix( dummyContract(iname,cc)), sep="." )
						  cdat <- object@data[[ which(datamonths == cc) ]]$bd1
						  tcvo <-merge(refxts, Vo(cdat))
						  tcvo <-tcvo[dsub]
						  cvo <- tcvo[,2]
						  cvo[is.na(cvo)] <- 0
						  colnames(cvo) <-clbl

						  volblock <- cbind(volblock,cvo)
					  }
					  ##calculte vtotal
					  vtotvals <- rowSums(volblock)
					  vtot <- xts(vtotvals, order.by=index(volblock))
					  colnames(vtot)<-"Vo.TOT"
					  volblock <- cbind(volblock,vtot)

					  ##add pct volume values
					  roundprec <- 2
					  for(j in 1:length(cids) ){
						  cid<-cids[j]
						  cc<-cdf$contr_month[cid]
						  clbl<-  paste("pVo", getContractSuffix( dummyContract(iname,cc)), sep="." )
						  pvo <- round( volblock[,j] / vtot, roundprec)
						  colnames(pvo) <- clbl
						  volblock <- cbind(volblock,pvo)
					  }
					  #browser()
					  print(volblock)
					  return(invisible())
				  })
#' nearly deprecated
#' @keywords internal
#' @docType methods
setGeneric( name='chkprintEODroll'
		,function(object,ctime=Sys.time(),n=6){standardGeneric("chkprintEODroll")})
#' @aliases chkprintEODroll,ContractSeq,ANY-method
setMethod('chkprintEODroll', 'ContractSeq'
	   ,function(object,ctime=Sys.time(), n=6 ){
				  iname <- getInstrumentName(object)
				  cdf <- getContractSequence(object)
				  xdata <- object@data
				  datamonths <- unlist(lapply(xdata,function(x){x$month}))

		  cm1 <- getUnexpiredContract(object,ctime=ctime,n=1)
		  cm2 <-  getUnexpiredContract(object,ctime=ctime,n=2)
		  cm3 <-  getUnexpiredContract(object,ctime=ctime,n=3)

		  cm1l <- getContractSuffix( dummyContract(iname,cm1))
		  cm2l <- getContractSuffix( dummyContract(iname,cm2))
		  cm3l <- getContractSuffix( dummyContract(iname,cm3))

		  cm1d <- xdata[[ which(datamonths == cm1) ]]$bd1
		  cm2d <- xdata[[ which(datamonths == cm2) ]]$bd1
		  cm3d <- xdata[[ which(datamonths == cm3) ]]$bd1

		  dsub<-paste("::",ctime,sep="")
		  cm1d <- cm1d[dsub]
		  cm2d <- cm2d[dsub]
		  cm3d <- cm3d[dsub]

		  tcm1d <- tail(cm1d,n)
		  tcm2d <- tail(cm2d,n)
		  tcm3d <- tail(cm3d,n)

		  tvo1 <- Vo(tcm1d)
		  tvo2 <- Vo(tcm2d)
		  tvo3 <- Vo(tcm3d)

		  tvtot <- Vo(tcm1d) + Vo(tcm2d) + Vo(tcm3d)

		  roundprec <- 2
		  pvo1 <- round( Vo(tcm1d) / tvtot, roundprec)
		  pvo2 <- round(Vo(tcm2d) / tvtot, roundprec)
		  pvo3 <- round(Vo(tcm3d) / tvtot, roundprec)

		  dvolthresh <- 0.10
		  tvolthresh <- 0.40
		  crit1 <- pvo1 - pvo2 < dvolthresh  & tvo2 / tvtot > tvolthresh  # combine 2 criterias
		  crit2 <- pvo1 - pvo3 < dvolthresh  & tvo3 / tvtot > tvolthresh  # combine 2 criterias
		  #browser()
		  chk1 <- which(crit1 == TRUE)
		  chk2 <- which(crit2 == TRUE)
		  if(length(chk1)>0){
			  idx <- chk1[1]
			  msg<- paste("check for possible contract roll between",cm1l, "and",cm2l,"around", index(tcm1d[idx]))
			  logwarn(msg,logger=getLoggername(object))
		  }
		  if(length(chk2)>0){
			  idx <- chk2[1]
			  msg<- paste("check for possible contract roll between",cm1l, "and",cm3l,"around", index(tcm1d[idx]))
			  print(msg)
			  logwarn(msg,logger=getLoggername(object))
		  }

		  return( invisible() )
									  })
##################ContractSeq##################
###############################################

#' set config from historic data for DTN datasource
#' @export
#' @docType methods
setGeneric(name='fixMetafromcsv',function(.Object, persist=T){standardGeneric('fixMetafromcsv')})
#' @aliases fixMetafromcsv,ContractSeq,ANY-method
setMethod('fixMetafromcsv','ContractSeq'
		,function(.Object,persist=T){

			nameObject <- deparse(substitute(.Object))
			iname <- getInstrumentName(.Object)
			cdf <- getContractSequence(.Object)
			isqdir <- paste(.Object@topstoredir, iname, sep="/") #topdir for sequence
			metafn<- paste(isqdir, "/meta/", iname, "_seq.csv", sep="")
			if( !file.exists(metafn)){
				msg <- paste(iname,"missing meta sequence scsv file", metafn)
				logwarn(msg,logger=getLoggername(.Object))
				stop(paste("ContractSeq:fixMetafromcsv",msg))
			}
			meta <- read.csv(metafn,header=TRUE,stringsAsFactors=FALSE)
			setContractSequence(.Object) <- meta
			if(persist){
				persistSeq(.Object)
			}
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		})

#' check meta data frame betwn csv stored file and object's instance
#' @export
#' @docType methods
setGeneric(name='chkMeta',function(object){standardGeneric('chkMeta')})
#' @aliases chkMeta,ContractSeq,ANY-method
setMethod('chkMeta','ContractSeq'
		,function(object){
			iname <- getInstrumentName(object)
			cdf <- getContractSequence(object)
			isqdir <- paste(object@topstoredir, iname, sep="/") #topdir for sequence
			metafn<- paste(isqdir, "/meta/", iname, "_seq.csv", sep="")
			if( !file.exists(metafn)){
				msg <- paste(iname,"missing meta sequence scsv file", metafn)
				logwarn(msg,logger=getLoggername(object))
				stop(paste("ContractSeq:chkMeta",msg))
			}
			meta <- read.csv(metafn,header=TRUE,stringsAsFactors=FALSE)
			##nrow check
			if(nrow(meta)!=nrow(cdf)){
				msg<-paste("instrument",iname
						,"mismatch in number of rows between stored and current object meta info ")
				loginfo(msg,logger=getLoggername(object))
			}
			##expries check
			cctrs <- cdf$contr_month
			vec2 <- cdf$expiry
			vec1 <- meta$expiry
			idx <- which(vec1 %in% vec2 == FALSE )
			if(length(idx)>0){
				badvals <- cctrs[idx]
				badvalss <-paste(badvals,collapse=" ")
				msg<-paste("instrument",iname
						,"mismatch in expries between stored and current meta for contract(s)"
						,badvalss)
				loginfo(msg,logger=getLoggername(object))
			}
			vec2 <- cdf$volroll
			vec1 <- meta$volroll
			idx <- which(vec1 %in% vec2 == FALSE )
			#browser()
			if(length(idx)>0){
				badvals <- cctrs[idx]
				badvalss <-paste(badvals,collapse=" ")
				msg<-paste("instrument",iname
						,"mismatch in volume rolls between stored and current meta for contract(s)"
						,badvalss)
				loginfo(msg,logger=getLoggername(object))
			}
			return( invisible() )
		})


#' check meta data frame betwn csv stored file and object's instance
#' @export
#' @docType methods
setGeneric(name='chkData',function(object){standardGeneric('chkData')})
#' @aliases chkData,ContractSeq,ANY-method
setMethod('chkData','ContractSeq'
		,function(object){
			d1fmt  <-'%Y-%m-%d'
			iname <- getInstrumentName(object)
			cdf <- getContractSequence(object)
			xdata <- object@data
			mcontrMonths <- cdf$contr_month


			metContrStart <- cdf$d1start
			metContrEnd  <- cdf$d1end


			datContrMonths <- unlist(lapply(xdata,function(x){x$month}))

			datContrStart <- unlist(lapply(xdata
										  ,function(x){
											strftime(index(first(x$bd1))
													,format=d1fmt)}))
			datContrEnd <- unlist(lapply(xdata
										  ,function(x){
											strftime(index(last(x$bd1))
													,format=d1fmt)}))
			 startDiff <- as.POSIXct(metContrStart) - as.POSIXct(datContrStart)
			 endDiff   <- as.POSIXct(metContrEnd)   - as.POSIXct(datContrEnd)

			 startIdx <- which(startDiff != 0)
			 endIdx   <- which(endDiff != 0)

			 #browser()

			 if( length(startIdx > 0 )){
				msgarr <- paste(mcontrMonths[startIdx]
							   ,metContrStart[startIdx]
							   ,datContrStart[startIdx],sep=":") 
				msg <- paste("start date data mismatch for contract months\n"
							,"contract : meta start : data start\n"
							, paste(msgarr,collapse="\n"), sep=" ")
				logwarn(msg,logger=getLoggername(object))
			 }
			 if( length(endIdx > 0 )){
				msgarr <- paste(mcontrMonths[endIdx]
							   ,metContrEnd[endIdx]
							   ,datContrEnd[endIdx],sep=":") 

				msg <- paste("end date data mismatch for contract months\n"
							,"contract : meta start : data start\n"
							, paste(msgarr,collapse="\n"), sep=" ")             		
				logwarn(msg,logger=getLoggername(object))
			 }
			return( invisible() )
		})




#' initial donwload of daily historic data 
#'
#' @export
#' @docType methods
setGeneric(name='iniDownload'
		   ,function(.Object, lstqtime=Sys.time(),persist=T
					,findvroll=T){standardGeneric('iniDownload')})
#' @aliases iniDownload,ContractSeq,ANY-method
setMethod('iniDownload','ContractSeq'
			,function(.Object,  lstqtime=Sys.time(),persist=T,findvroll=T){

	 nameObject <- deparse(substitute(.Object))

	 datasrc <- .Object@datasrc
	 resetLogger(.Object, "INFO")

	 switch(datasrc,
			 "dtn" = iniDownloadDTN(.Object,lstqtime=lstqtime ))

	 ##persist after actual data download before sanitycheck() & findrollonvol()
	 if(persist){
	  persistSeq(.Object)
	  msg <-paste("instrument", .Object@instName, "initial persist of row data")
	  loginfo(msg, logger=getLoggername(.Object))
	 }  
	 #browser()
	 if(findvroll){
	  ##check integrity of data
	  ok2Persist <- sanityCheckMeta(.Object)  #### !!!!!
	  if(ok2Persist){
		 findRollonvol(.Object,decr=T,mstore=persist)
	  }
	  if (persist) { persistSeq(.Object)  }	  
	 }
	  

	 assign(nameObject,.Object,envir=parent.frame())
	 return ( invisible() )
})

#' initial download of intrday data for DTN datasource
#'
#' @export
#' @docType methods
setGeneric(name='iniDownloadIntra',function(.Object, lstqtime=Sys.time()
											,interval=60,lfactor=1.33,startContract=NA,endContract=NA
											,verbose=TRUE,reset=FALSE,persist=FALSE)
		   {standardGeneric('iniDownloadIntra')})
#' @aliases iniDownloadIntra,ContractSeq,ANY-method
setMethod('iniDownloadIntra','ContractSeq'
			,function(.Object,lstqtime=Sys.time()
					 ,interval=60,lfactor=1.33,startContract=NA,endContract=NA
					 ,verbose=TRUE,reset=FALSE,persist=FALSE) {

	 nameObject <- deparse(substitute(.Object))

	 datasrc <- .Object@datasrc
	 resetLogger(.Object, "INFO")

	 switch(datasrc,
			 "dtn" = iniDownloadIntraDTN(.Object,lstqtime=lstqtime
										 ,interval=interval,lfactor=lfactor
										 ,startContract=startContract,endContract=endContract
										 ,reset=reset,verbose=verbose ))

	 ##persist after actual data download before sanitycheck() & findrollonvol()
	 if(persist){
	  persistIntra(.Object,interval)
	  msg <-paste("instrument", .Object@instName
				,"persisisted intrdaday contract data"
				,"for interval", interval)
	  loginfo(msg, logger=getLoggername(.Object))
	  }  
	  

	 assign(nameObject,.Object,envir=parent.frame())
	 return ( invisible() )
})

#' load intraday data prviously stored
#'
#' @export
#' @docType methods
setGeneric(name='loadIntraday'
		   ,function(.Object, interval=3600
					,ctr_start=getUnexpiredContract(.Object,ctime=Sys.time(), n=0)
					,ctr_end=getUnexpiredContract(.Object,ctime=Sys.time(), n=1)
					,alldata=NA
					,verbose=TRUE)
		   {standardGeneric('loadIntraday')})
#' @aliases loadIntraday,ContractSeq,ANY-method
setMethod('loadIntraday','ContractSeq'
			,function(.Object,interval=3600
					,ctr_start=getUnexpiredContract(.Object,ctime=Sys.time(), n=0)
					,ctr_end=getUnexpiredContract(.Object,ctime=Sys.time(), n=1)
					,alldata=NA
					,verbose=TRUE) {

	nameObject <- deparse(substitute(.Object))
	s1fmt <- '%Y-%m-%d %H:%M:%S'	
	iname <- .Object@instName
	itickerPref <- getTickerPref(.Object)
	isqdir <- paste(.Object@topstoredir, iname, sep="/") 
	datadir <- paste(isqdir,'contracts',sep="/")  
	dfreq <- as.character(interval)
	cdf <- getContractSequence(.Object)
	if (!is.na(alldata)){
		metacmonths <- cdf$contr_month
	}else{
		six <- match(ctr_start, cdf$contr_month)
		eix <- match(ctr_end, cdf$contr_month)
		metacmonths <- cdf$contr_month
		if(!is.na(six) && !is.na(eix)){
			metacmonths <- cdf$contr_month[six:eix]
		}
	}
	intrdata <-list()
	for(j in 1:length(metacmonths) ){
		cmonth <- metacmonths[j]
		ccontr <- bareContract(iname, itickerPref, cmonth )
		contrsuff <- getContractSuffix(ccontr)
		cticker <- getContractTicker(ccontr)
		contractdir <- paste(datadir, contrsuff, sep="/")
		fn <- paste(datadir,"/",contrsuff,"/",iname,"_",contrsuff,"_",dfreq,".rdat",sep="")
		if( !file.exists(fn)) { next} #skip t next iteration
		load(fn)
		ldfreq <- icontrdat$eldat$freq
		if( ldfreq != dfreq){
		  msg <- paste("loadIntraday: file", fn,"found data frequency:"
					   ,ldfreq, "expected:",dfreq,sep=" ")
		  if(verbose) { logwarn(msg,logger=getLoggername(.Object))  } 
		  next
		}
		xdat <- icontrdat$eldat$xdat
		nl <-  list(ticker=cticker,month=cmonth, idat=list())
		intrvaldat <- list(freq=ldfreq
						  ,dtstart=strftime(index(first(xdat)),format=s1fmt,tz=indexTZ(xdat))
						  ,dtstop=strftime(index(last(xdat)),format=s1fmt,tz=indexTZ(xdat))
						  ,xdat=xdat)
		nl$idat[[ length(nl$idat) +1 ]] <- intrvaldat
		intrdata[[ length(intrdata)+1]] <- nl

	}	 
	.Object@intrdata <- intrdata 
	 assign(nameObject,.Object,envir=parent.frame())
	 return ( invisible() )
})




#' update daily data EOD for currently unexpired contracts
#' @export
#' @docType methods
		setGeneric(name='updateEOD'
				  ,function(.Object, scontract=NA, ctime=Sys.time()
						   , updMeta=FALSE, persist=FALSE){standardGeneric('updateEOD')})
#' @aliases updateEOD,ContractSeq,ANY-method
		setMethod('updateEOD','ContractSeq'
				,function(.Object,  scontract=NA, ctime=Sys.time()
						 ,updMeta=FALSE,persist=FALSE ){

					nameObject <- deparse(substitute(.Object))

					datasrc <- .Object@datasrc
					iname <- getInstrumentName(.Object)
					##logger needs to reset
					resetLogger(.Object, "INFO")
					###update data values
					switch(datasrc,
							"dtn" = updateEODDTN(.Object,scontract=scontract, ctime=ctime) )

 
					#search for volroll on unexpired contracts
					###chkprintEODroll(.Object,ctime=ctime,n=6)

					if(updMeta){
						updateMeta(.Object,ctime=ctime,persist=persist ) #will attempt to update expiries, volroll
					}
					##check integrity of data
					##ok2Persist <- sanityCheckMeta(.Object)

#					if(ok2Persist &&  persist){
					if(persist){
						persistSeq(.Object)
					}
					assign(nameObject,.Object,envir=parent.frame())
					return ( invisible() )
				})



#' update of intrday data 
#'
#' @export
#' @docType methods
setGeneric(name='updateIntra',function(.Object,interval=60,lstqtime=Sys.time()
											,startContract=NA,endContract=NA
											,verbose=TRUE,persist=FALSE)
		   {standardGeneric('updateIntra')})
#' @aliases updateIntra,ContractSeq,ANY-method
setMethod('updateIntra','ContractSeq'
			,function(.Object,interval=60,lstqtime=Sys.time()
					 ,startContract=NA,endContract=NA
					 ,verbose=TRUE,persist=FALSE) {

	 nameObject <- deparse(substitute(.Object))

	 datasrc <- .Object@datasrc
	 resetLogger(.Object, "INFO")

	 switch(datasrc,
			 "dtn" = updateIntraDTN(.Object,lstqtime=lstqtime
										 ,interval=interval
										 ,startContract=startContract
										 ,endContract=endContract
										 ,verbose=verbose ))

	 ##persist after actual data download before sanitycheck() & findrollonvol()
	 if(persist){
	  persistIntra(.Object,interval)
	  msg <-paste("instrument", .Object@instName
				,"persisisted intrdaday contract data"
				,"for interval", interval)
	  loginfo(msg, logger=getLoggername(.Object))
	  }  
	  

	 assign(nameObject,.Object,envir=parent.frame())
	 return ( invisible() )
})



#' return a ist of intraday contracts of specific interval
#' @keywords internal
#' @docType methods
setGeneric( name='getIntraIntervalDataIndex'
		  ,function(object,interval=3600){standardGeneric("getIntraIntervalDataIndex")})
#' @aliases getIntraIntervalDataIndex,ContractSeq,ANY-method
setMethod('getIntraIntervalDataIndex', 'ContractSeq'
		  ,function(object,interval=3600){

			intv_ix <- unlist(lapply(object@intrdata
									,function(x){ 
										match(as.character(interval)
											,unlist(lapply(x$idat
														  ,function(y){y$freq} ))  
											 ) 
									}
							  ))
			index_pos <- intv_ix[1]
			#browser()
			if(!isTRUE(all.equal(intv_ix, rep(intv_ix[1], length(intv_ix)) ))){
				msg <- paste("getIntraIntervalDataIndex:", getInstrumentName(.Object)
							,"intraday contracts for interval",interval
							,"are not on the same index:", paste(intv_ix, collapse=" "))
				logwarn(msg,logger=getLoggername(object))
				return( NA)
			}
			if ( !is.na(match(NA,intv_ix)) ){  #intra data of this interval does not exist
				#so return next value
				index_pos <- length(object@intrdata[[1]]$idat) + 1
			}
			return(index_pos)
		   })


#####get list of intraday contracts of specific interval
#' return a ist of intraday contracts of specific interval
#' @keywords internal
#' @docType methods
setGeneric( name='getIntraIntervalData'
		  ,function(object,interval=3600){standardGeneric("getIntraIntervalData")})
#' @aliases getIntraIntervalData,ContractSeq,ANY-method
setMethod('getIntraIntervalData', 'ContractSeq'
		  ,function(object,interval=3600){

				intv_pos <- getIntraIntervalDataIndex(object, interval=interval)
				xtsl <- lapply( object@intrdata
							   ,function(x){ list(month=x$month,data=x$idat[[intv_pos]]$xdat)})
				return(xtsl)
		   })


#' convert and store intrday data from one periodicity to a lower one
#'
#' @export
#' @docType methods
setGeneric(name='toPrdIntra',function(.Object,sinterval=3600,tointerval=7200)
										{standardGeneric('toPrdIntra')})
#' @aliases toPrdIntra,ContractSeq,ANY-method
setMethod('toPrdIntra','ContractSeq'
			,function(.Object,sinterval=3600,tointerval=7200){
	 if(tointerval<sinterval){
		stop("toPrdIntra validation:", "target periodicity", tointerval
			,"expected to exceed the source periodicity",sinterval)
	 }
	 nameObject <- deparse(substitute(.Object))
	 #browser()
	 s_xtsl <- getIntraIntervalData(.Object, interval=sinterval)
	 t_xtsl <- lapply(s_xtsl,function(x){list(month=x$month
											 ,data=to.minutes(x$data,k=tointerval/60))})
	 targ_pos <- getIntraIntervalDataIndex(.Object, interval=tointerval)

	 ##set target xts data
	 #internal funciton
	 setcontrdata <-function(x,ldata, interval,intvalpos){
		s1fmt <- '%Y-%m-%d %H:%M:%S'
		idx <- match(x$month,  unlist(lapply(ldata,function(y){y$month})) )
		cxts <- ldata[[idx]]$data
		x$idat[[intvalpos]] <- list(freq=as.character(interval)
									,dtstart=strftime(index(first(cxts)),format=s1fmt,tz=indexTZ(cxts))
									,dtstop=strftime(index(last(cxts)),format=s1fmt,tz=indexTZ(cxts))
									,xdat=cxts)
		return(x)
	 }
	 intrdata <- .Object@intrdata 

	 intrdata <- lapply(intrdata, FUN=setcontrdata, ldata=t_xtsl
												  , interval=tointerval,intvalpos=targ_pos)
	 .Object@intrdata <- intrdata
	 assign(nameObject,.Object,envir=parent.frame())
	 return ( invisible() )
})


#' convert and store intrday data for all intervals from object@intradconf
#'
#' @export
#' @docType methods
setGeneric(name='depr_allPrdIntra',function(.Object)
										{standardGeneric('depr_allPrdIntra')})
#' @aliases depr_allPrdIntra,ContractSeq,ANY-method
setMethod('depr_allPrdIntra','ContractSeq'
			,function(.Object){
	 nameObject <- deparse(substitute(.Object))
	 sintval <- .Object@intrdconf$intradayfreq[1]
	 tointvals <- .Object@intrdconf$intradayfreq[-1]
		  
	 for(j in  index(tointvals)){
		toPrdIntra(.Object,sinterval=sintval,tointerval=tointvals[j] )
	 }
	 assign(nameObject,.Object,envir=parent.frame())
	 return ( invisible() )
})


#' @export
#' @docType methods
setGeneric(name='allPrdIntra',function(.Object, contracts=NA)
										{standardGeneric('allPrdIntra')})
#' @aliases allPrdIntra,ContractSeq,ANY-method
setMethod('allPrdIntra','ContractSeq'
			,function(.Object,contracts=NA){
	 nameObject <- deparse(substitute(.Object))

	 sintval <- .Object@intrdconf$intradayfreq[1]
	 tointvals <- .Object@intrdconf$intradayfreq
	 intrdata <- .Object@intrdata
	 if(is.na(contracts[1])){
		contracts <- unlist(lapply(.Object@intrdata, function(x){x$month}))
	 } 
	 proc_freq  <-function(y,sintv, idat){
		s1fmt <- '%Y-%m-%d %H:%M:%S'
		src_pos <- match(as.character(sintv), unlist(lapply(idat,function(x){x$freq})))
		targ_pos <- match(as.character(y), unlist(lapply(idat,function(x){x$freq})))
		if(is.na(targ_pos)){ targ_pos <- length(idat)+1 } #target frequency does not  exist
 
		if(targ_pos == src_pos){ return (idat[[src_pos]]) } #do not convert for source freq

		new_xts <- to.minutes(idat[[src_pos]]$xdat ,k=y/60)
		el <- list(freq=as.character(y)
				  ,dtstart=strftime(index(first(new_xts)),format=s1fmt,tz=indexTZ(new_xts))
				  ,dtstop=strftime(index(last(new_xts)),format=s1fmt,tz=indexTZ(new_xts))
				  ,xdat=new_xts)
		return(el)
	 }
	 proc_contract <-function(x,contracts,sintv,tointvs){
		contr_idx <- match(x$month, contracts)
		if(is.na(contr_idx)) { return (x)} #skip current contract for processing
		idatl <- lapply(tointvs, FUN=proc_freq, sintv=sintv,idat=x$idat)
		#browser()
		x$idat <- idatl

		return(x)
	 }
	 intrdata <- lapply(intrdata, FUN=proc_contract, contracts=contracts
									  ,sintv=sintval,tointvs=tointvals)    
	 .Object@intrdata <- intrdata
	 assign(nameObject,.Object,envir=parent.frame())
	 return ( invisible() )
})




#' sanity check
#' @keywords internal
#' @docType methods
setGeneric(name='sanityCheckMeta',function(.Object){standardGeneric('sanityCheckMeta')})
#' @aliases sanityCheckMeta,ContractSeq,ANY-method
setMethod('sanityCheckMeta','ContractSeq'
		,function(.Object ){
			nameObject <- deparse(substitute(.Object))
			shouldPersist <- TRUE
			cdf <- getContractSequence(.Object)
			iname <- getInstrumentName(.Object)

			expiries <- cdf$expiry
			expirs <- expiries[!is.na(expiries)] # get rid of NA
			if (length(expirs[duplicated(expirs)]) > 0 ){
				wmsg <- paste(iname,"duplicated expiries dates found."
						,"Problem with downloaded data." )

				duels <- expirs[duplicated(expirs)]
				cat(duels, "\n")
				rows <- which(expirs == duels)
				cat("for contracts ", cdf$contr_month[rows],"\n")
				badcontracts <- cdf$contr_month[rows]
				for(i in 1:length(badcontracts)){
					wmsg <- paste(wmsg, badcontracts[i])
				}
				logerror(wmsg,logger=getLoggername(.Object))
				shouldPersist <- FALSE

			}
			## na for d1start and d1end
			if (  !is.na( match(TRUE, is.na(cdf$d1start)))){
				logwarn(paste(iname,"NA present in contract download start dates"),logger=getLoggername(.Object))
				shouldPersist <- FALSE
			}
			if (  !is.na( match(TRUE, is.na(cdf$d1end)))){
				logwarn(paste(iname,"NA present in contract download end dates"),logger=getLoggername(.Object))
				shouldPersist <- FALSE
			}

			##mixed NA in expiry and volrol
			if ( !is.null(cdf$expiry)){
			 expirIdx <- which(!is.na(cdf$expiry))
			 unexpirIdx <- which(is.na(cdf$expiry))
						 if(length(unexpirIdx) >0 ){
			   if( last(expirIdx) > first(unexpirIdx) ){
				logwarn(paste(iname,"mixed empty non-empty expiry dates"),logger=getLoggername(.Object))
				shouldPersist <- FALSE
			   }
						}
			   }
			if( !is.null(cdf$volroll)){
			  rolledIdx <- which(!is.na(cdf$volroll))
			  unrolledIdx <- which(is.na(cdf$volroll))
			  if( last(rolledIdx) > first(unrolledIdx) ){
				 logwarn(paste(iname,"mixed empty non-empty rollonvol dates"),logger=getLoggername(.Object))
				 shouldPersist <- FALSE
			  }
			}

			##check for monotonicity of expiries and volrolls
			if ( !is.null(cdf$expiry)){
				expirIdx <- which(!is.na(cdf$expiry))
				if(length(expirIdx)>0){
					texpr <- as.POSIXct( cdf$expiry[expirIdx] )
					tdiff <- texpr[2:length(texpr)]-texpr[1:(length(texpr)-1)]
					crit <- match(TRUE, tdiff < 0)
					if(!is.na(crit)){
						badcc <- paste( cdf$contr_month[crit], collapse=" ")
						msg <- paste(iname,"non monotonous expiry dates. Offending contracts", badcc)
						logwarn(msg,logger=getLoggername(.Object))
						shouldPersist <- FALSE
					}
				}
			}
			if ( !is.null(cdf$volroll)){
				rolledIdx <- which(!is.na(cdf$volroll))
				if(length(rolledIdx)>0){
					troll <- as.POSIXct( cdf$volroll[rolledIdx] )
					tdiff <- troll[2:length(troll)]-troll[1:(length(troll)-1)]
					crit <- match(TRUE, tdiff < 0)
					if(!is.na(crit)){
						badcc <- paste( cdf$volroll[crit], collapse=" ")
						msg <- paste(iname,"non monotonous volroll dates. Offending contracts", badcc)
						logwarn(msg,logger=getLoggername(.Object))
						shouldPersist <- FALSE
					}
				}
			}

			return( shouldPersist)
		})

#' find roll on volume dates, for expired contracts for which data exists
#' saves it to meta data
#' @export
#' @docType methods
setGeneric(name='findRollonvol',function(.Object,ctime=Sys.time(), decr=TRUE, mstore=FALSE){standardGeneric('findRollonvol')})
#' @aliases findRollonvol,ContractSeq,ANY-method
setMethod('findRollonvol','ContractSeq'
		,function(.Object,ctime=Sys.time(),decr=TRUE,mstore=FALSE){
			nameObject <- deparse(substitute(.Object))
			cdf <- getContractSequence(.Object)
			xdata <- .Object@data

			contracts <- cdf$contr_month
			expiries <- cdf$expiry
			
	
			#expirs <- expiries[!is.na(expiries)]
			ccexprd <- getUnexpiredContract(.Object,ctime=ctime,n=0)
			if (is.na(ccexprd)){
				expirs <- expiries[1:(length(expiries)-1)]
			}else{
				expirs <- expiries[1: match(ccexprd,contracts)]
			}
	
			#rollrule
			rollrule<-.Object@rollrule
			#browser()
			##find contract pairs for existing expiries (internal func)
			mkcpair<-function(x, seqdf, mdata){
				datamonths <- unlist(lapply(mdata,function(x){x$month}))
				fcidx <- which(seqdf$expiry == x)
				fcmoexp <- x
				fcmo <- seqdf$contr_month[fcidx]
				ncmo <-seqdf$contr_month[fcidx+1]
				#browser()
				fcd1 <- mdata[[ which(datamonths == fcmo)  ]]$bd1
				ncd1 <- mdata[[ which(datamonths == ncmo)  ]]$bd1
				xncd1 <- ensureXtsindex(ncd1,fcd1) #make sure index match between fc and nc
				return(list(frontc=fcmo, nextc=ncmo, frontexp=fcmoexp, fcd1=fcd1, ncd1=ncd1))
			}
			cpairs <- lapply(expirs, FUN=mkcpair, seqdf=cdf, mdata=xdata)


			##actuall volume roll finding
			cpairs <- lapply(cpairs, FUN=findroll, rdecr=decr, rule=rollrule )  ## add volrolls

			#browser()

			##save rolls info to meta df
			contracts <- unlist(lapply(cpairs, function(x){x$frontc}))
			foundrolls <- unlist(lapply(cpairs, function(x){x$volrol}))
			volroll <- rep(NA, nrow(cdf))
			id <- match(contracts, cdf$contr_month)

			volroll[id] <- foundrolls
			if(is.na(match('volroll', colnames(cdf)))){
			  cdf <- cbind(as.matrix(cdf), volroll)
			}else{
				cdf$volroll <- volroll
			}
			cdf <- as.data.frame(cdf, stringsAsFactors = FALSE)

			if(mstore){
				setContractSequence(.Object) <- cdf
				cat('Warning. findRollonvol() Metadata is  modified\n')
			}else{
				print(cdf)
				cat('Warning. findRollonvol() Metadata is not modified\n')
			}
			assign(nameObject,.Object,envir=parent.frame())

			return( invisible())
		})

#' @keywords internal
##internal func
findroll<-function(cpair,rdecr,rule){

	d1fmt  <-'%Y-%m-%d'
	fcexp <- cpair$frontexp #front month expiry
	fcdat <- cpair$fcd1
	ncdat <- cpair$ncd1

	fcmonth <- cpair$frontc   #front month
	nm <- as.numeric( strftime(as.POSIXct(fcmonth),format="%m") )
	rulemonths <- unlist(lapply(rule,function(x){x$cmonth}))
	idx <- match(nm,rulemonths)

	##if (fcmonth == "2013-09-01") browser()

	if(is.na(idx)){
		stop(paste("findroll validation","missing contract month",nm, "in contract sequence rollrule"))
	}
	##roll period  and front month volume percentagge specific for  each contract
	offset <-rule[[idx]]$rollprd
	vpct <-  rule[[idx]]$fmvolpct

	#browser()

	#find expiry day data index
	expidx <-match( as.POSIXct(fcexp), index(fcdat))
	offexpidx <- expidx - offset
	seekrolldsub <- paste(strftime(index(fcdat[offexpidx]),format=d1fmt)
			,"::",fcexp,sep="")
	v1<-Vo(fcdat[seekrolldsub])
	v2<-Vo(ncdat[seekrolldsub])
	vtot <- v1 + v2
	vcrit <- v1 - v2 < 0
	pctcrit <- v1 / vtot < vpct
	dblcrit <- vcrit & pctcrit
	vcbars <- which( dblcrit == TRUE)


	if (length(vcbars) == 0){
		rolldate <- fcexp
	}else{  #vcbars is an array
		idx <- first(vcbars) ## closest to expiry
		rollbaridx <- index(dblcrit[idx])
		rolldate <- strftime( rollbaridx , format=d1fmt )
	}

	if(rdecr){  ##optionally deincrement rolldate by 1 day
		idx <- match( as.POSIXct(rolldate), index(fcdat) )
		drolldate <- strftime( index(fcdat[idx-1]), format=d1fmt)
		cpair$volrol <- drolldate
	}else{
		cpair$volrol <- rolldate
	}

	return(cpair)
}



#' updates expiries and rolldates on EOD
#' @export
#' @docType methods
		setGeneric(name='updateMeta',function(.Object ,  ctime=Sys.time(),persist=F ){standardGeneric('updateMeta')})
#' @aliases updateMeta,ContractSeq,ANY-method
		setMethod('updateMeta','ContractSeq'
				,function(.Object,  ctime=Sys.time(), persist=F ){

					nameObject <- deparse(substitute(.Object))
					iname <- getInstrumentName(.Object)
					cdf <- getContractSequence(.Object)

					##here is a place to seek an expiry update for unexpired from data
					updateDataExpiries(.Object)

					#make sure number of rolled is not less than number of expired
					#specific check
					cdf <- getContractSequence(.Object)
					expiredIdx <- which( is.na(cdf$expiry) == FALSE )
					rolledIdx <- which( is.na(cdf$volroll) == FALSE )
					if(length(expiredIdx) > length(rolledIdx)){
						loginfo(paste(iname,"found more expired dates than rolled dates"
										,"rerunning findRollOnvol()" ),logger=getLoggername(.Object))
						findRollonvol(.Object,mstore=persist)
					}

					##here is a place to seek rollonVol for a first unexpired contract !!!!!
					### (depr ) updateRollonvol(.Object, ctime=ctime) 


					assign(nameObject,.Object,envir=parent.frame())
					return ( invisible() )
				})


#' update roll on volume dates  (deprecated)
#
#' @keywords internal
#' @docType methods
setGeneric(name='updateRollonvol',function(.Object,ctime=Sys.time() ){standardGeneric('updateRollonvol')})
#' @aliases updateRollonvol,ContractSeq,ANY-method
setMethod('updateRollonvol','ContractSeq'
		 ,function(.Object,ctime=Sys.time() ){
				nameObject <- deparse(substitute(.Object))
				cdf <- getContractSequence(.Object)
				iname <- getInstrumentName(.Object)
				xdata <- .Object@data
				datamonths <- unlist(lapply(xdata,function(x){x$month}))
				d1fmt  <-'%Y-%m-%d'

				ccfmunexp <- getUnexpiredContract(.Object, ctime=ctime,n=1)
				ccnmunexp <- getUnexpiredContract(.Object, ctime=ctime,n=2)
				expFutexpir <- getUnexpiredExpectedExpiry(.Object,ctime=ctime)

				rowid<- match(ccfmunexp, cdf$contr_month)
				rolloffset <- floor (365 /length(.Object@yrContracts) / 7 )
				fcd1 <- xdata[[ which(datamonths == ccfmunexp)  ]]$bd1
				ncd1 <- xdata[[ which(datamonths == ccnmunexp)  ]]$bd1
				##make sure to search for volroll only near expected expiration
				startSearchRoll <- as.POSIXct(expFutexpir) - 86400 * rolloffset
				#browser()
				if ( as.POSIXct(ctime) > startSearchRoll ){
					seekrolldsub <- paste(strftime(as.POSIXct(startSearchRoll),format=d1fmt)
							,"::",strftime(as.POSIXct(ctime),format=d1fmt),sep="")
					v1<-Vo(fcd1[seekrolldsub])
					if(nrow(v1)==0){ ## not values yet in seekrolldsub period
						return( invisible() )
					}
					v2<-Vo(ncd1[seekrolldsub])
					vcrit <- v1 - v2 < 0
					vcbars <- which(vcrit == TRUE)
					if(length(vcbars) > 0 ){
						rollbaridx <- vcbars[1]
						rolldate <- strftime( index(vcrit)[rollbaridx] , format=d1fmt )
						cdf$volroll[rowid]<-rolldate
						loginfo(paste(iname,"found new volroll date",rolldate
									 ,"for contract",ccfmunexp ),logger=getLoggername(.Object))
					}
				}
				setContractSequence(.Object) <- cdf

				assign(nameObject,.Object,envir=parent.frame())
				return( invisible())
	})



#' update expiries of unexpired contracts
#' @keywords internal
#' @docType methods
	setGeneric(name='updateDataExpiries',function(.Object){standardGeneric('updateDataExpiries')})
#' @aliases updatekExpiries,ContractSeq,ANY-method
	setMethod('updateDataExpiries','ContractSeq'
			,function(.Object ){
				nameObject <- deparse(substitute(.Object))
				iname <- getInstrumentName(.Object)
				cdf <- getContractSequence(.Object)
				xdata <- .Object@data
				datamonths <- unlist(lapply(xdata,function(x){x$month}))

				unexpiredIdx <- which( is.na(cdf$expiry) == TRUE )
				d1ends <- cdf$d1end[unexpiredIdx]
				#if all expiries dates were filled (from google spreadsheets)
				if(length(d1ends) == 0){ 
					#assign(nameObject,.Object,envir=parent.frame())
					return(invisible)
				}
				lastd1end <- d1ends[length(d1ends)]
				if( d1ends[length(d1ends)-1]  != lastd1end ){
					stop(paste("ContractSeq::updateDataExpiries validation"
									,"different data ends for last 2 unexpired contracts."
									,"More contracts should be added to the sequence."))

				}
				for(j in 1:(length(d1ends)-1)){
					curd1end <- d1ends[j]
					if (curd1end != lastd1end ){
						#new expiry found
						rowid <- match(curd1end, cdf$d1end)
						cdf$expiry[rowid] <- curd1end
						cmonth <- cdf$contr_month[rowid]
						xdata[[ which(datamonths == cmonth) ]]$expiry <- curd1end
						loginfo(paste(iname,"found new expiry date",curd1end
										,"for contract",cmonth ),logger=getLoggername(.Object))

					}
				}
				cdf <- as.data.frame(cdf, stringsAsFactors = FALSE)
				.Object@contrseq <- cdf
				.Object@data <- xdata
				assign(nameObject,.Object,envir=parent.frame())
				return(invisible())
			})

#' read expiries from google spreadsheet
#' @export
#' @docType methods
setGeneric(name='fillExpiries',
		  function(.Object,persist=FALSE
					,kkeys="0AuWXa53znQ3YdHU0QWFqNFFZTmhuUzdPdGctMk90aVE")
				   {standardGeneric('fillExpiries')})
#' @aliases updatekExpiries,ContractSeq,ANY-method
setMethod('fillExpiries'
		  ,'ContractSeq'
	  ,function(.Object,persist=FALSE
	  			,kkeys="0AuWXa53znQ3YdHU0QWFqNFFZTmhuUzdPdGctMk90aVE" ){
			  keys<-google_ss(gid=0,key=kkeys)
			  keys<-stripquotes(keys)
			  iname <- getInstrumentName(.Object)
			  id <- match(iname, keys$ins )
			  if(is.na(id) ){
				  stop(paste("ContractSeq:fillExpiries validation"
							 ,iname,"not found in expiries keys google spreadsheet"))
			  }
			  ikey <- keys$key[id]
			  googcs <- google_ss(gid=0, key=ikey)
			  if(!is.data.frame(googcs)){
				  stop(paste("ContractSeq:fillExpiries validation"
							 ,"failed to read expiries for",iname,"with key",ikey))
			  }
			  googcs<-stripquotes(googcs ) ## instruments expiries are loaded here
			  print("expiries from google spreadsheet")
			  print(googcs)
		  nameObject <- deparse(substitute(.Object))
		  cdf <- getContractSequence(.Object)
			  naexpIdx <- which( is.na(cdf$expiry) == TRUE )
			  if( length(naexpIdx) == 0 ){ ## exit on absence of NA in expiries
				  return( invisible() )
			  }
			  for(j in 1:length(naexpIdx)){
				  cid <- naexpIdx[j]
				  ccontr <- cdf$contr_month[cid]
				  gx <- which( googcs$contr_month == ccontr )
				  if(length(gx) != 1 ){
					  msg <- paste("instrument", iname, "contract", ccontr, "mismatch in google stored expiries")
					  logwarn(msg, logger=getLoggername(.Object))
					  next
				  }
				  expirval <- googcs$expiry[gx]
				  cdf$expiry[cid] <- expirval
				  msg <- paste("set expiry for contract", ccontr, "as", expirval )
				  loginfo(msg, logger=getLoggername(.Object))
			  }
			  setContractSequence(.Object) <- cdf
			  if(persist){ persistSeq(.Object) }

		  assign(nameObject,.Object,envir=parent.frame())
			  return(invisible() )
		   })
#' getGluedSequence
#' @export
#' @docType methods
		setGeneric(name='getGluedsequence'
				,function(.Object,stime='1998-01-01',etime=Sys.time(), rollon='volume')
												   {standardGeneric('getGluedsequence')})
#' @aliases getGluedsequence,ContractSeq,ANY-method
		setMethod('getGluedsequence','ContractSeq'
				,function(.Object,stime='1998-01-01',etime=Sys.time(), rollon='volume' ){
					nameObject <- deparse(substitute(.Object))
					cdf <- getContractSequence(.Object)
					xdata <- .Object@data
					d1fmt<-"%Y-%m-%d"

					if(rollon == 'expiry'){
						contrend <- cdf$expiry
					}else if (rollon == 'volume'){
						contrend <- cdf$volroll
					}else{
						stop("ContractSeq:: getGluedsequence  validation bad rollon parameter")
					}


					etime <- as.POSIXct(etime)
					tdiff <- etime - as.POSIXct(contrend)
					ptdiff <- tdiff[tdiff>0]
					ptdiff <- ptdiff[!is.na(ptdiff)]
					idx <- which(tdiff == min(ptdiff))
					erowid <- idx + 1
					#browser()
					##datastart
					stime<- as.POSIXct(stime)
					dends <- as.POSIXct(cdf$d1end) #expiries

					tsdiff <- stime - dends
					negtsdiff <-tsdiff[tsdiff < 0]
					srowid <- match(max(negtsdiff),tsdiff)
					#browser()
					sglued <- list()
					datamonths <- unlist(lapply(xdata,function(x){x$month}))
					##first contract
					for(j in srowid:erowid){
						cmonth <- cdf$contr_month[j]
						c2month <- cdf$contr_month[j+1]
						c3month <- cdf$contr_month[j+2]
						cfexpir <- cdf$expiry[j]
						ctrend <- contrend[j]
						fcd1 <- xdata[[ which(datamonths == cmonth)  ]]$bd1
						if (j == srowid){ ##fist contract
							sdt<-as.POSIXct(strftime(stime,format=d1fmt))
							edt<- as.POSIXct(strftime(ctrend,format=d1fmt)) + 86399 ##make sure to include 23:59:59
						}else if(j == erowid){  #last contract
							pctrend <- contrend[j-1]
							sidx <- match( as.POSIXct(pctrend), index(fcd1) )
							##browser() ### debugging last contract dsub
							sdt <-as.POSIXct(strftime( index(fcd1)[sidx+1],format=d1fmt))
							if(is.na(sdt)){
								sdt <- as.POSIXct( strftime(pctrend, format=d1fmt)) + 86401 ## make sure to include 1 second of next day
							}
							edt<-as.POSIXct(etime)
						}else{
							pctrend <- contrend[j-1]
							sidx <- match( as.POSIXct(pctrend), index(fcd1) )
							sdt <-as.POSIXct(strftime( index(fcd1)[sidx+1],format=d1fmt))
							edt<- as.POSIXct(strftime(ctrend,format=d1fmt)) + 86399 ##make sure to include 23:59:59
						}
						dsub <- paste( sdt,edt,sep="::")

						xcdat <- fcd1[dsub]
						sglued[[length(sglued)+1]] <- list(cfmonth=cmonth
														  ,c2month=c2month
														  ,c3month=c3month
														  ,cfexpir=cfexpir
														  ,cfend=ctrend
														  ,datasub=dsub)
					}
					return(sglued)
				})

#' getLastQuoteDate
#' @export
#' @docType methods
setGeneric(name='getLastQuoteDatePosix'
				,function(object)
		  {standardGeneric('getLastQuoteDatePosix')}) 
#' @aliases getLastQuoteDatePosix,ContractSeq,ANY-method
setMethod('getLastQuoteDatePosix','ContractSeq'
		,function(object ){
				lst_avail_dt_char <- object@data[[length(object@data)]]$d1end
				ccontr <- getUnexpiredContract(object, ctime=lst_avail_dt_char)
				ccontr_idx <- match( ccontr,  unlist(lapply(object@data
											   ,function(x){x$month}))) 
				lst_avail_dt <- last(index(object@data[[ccontr_idx]]$bd1))
				if( !is.na(object@intrdata[1])){
					ccontr_iidx <- match(ccontr, unlist(lapply(object@intrdata
											   ,function(x){x$month})))
					freq_idx <- match( as.character(min(object@intrdconf$intradayfreq))
									  ,unlist(lapply(object@intrdata[[ccontr_iidx]]$idat
											  ,function(y){y$freq})))
					lst_avail_dt <- last(index(object@intrdata[[ccontr_iidx]]$idat[[freq_idx]]$xdat))				 	
				}
				return(lst_avail_dt)	
		})

#' getSerieFromGluedsequence
#' @export
#' @docType methods
setGeneric(name='getSerieFromGluedsequence'
				,function(.Object,cseq,freq=86400,dblvol=FALSE,idxchk=FALSE,volcol=5)
						 {standardGeneric('getSerieFromGluedsequence')})
#' @aliases getSerieFromGluedsequence,ContractSeq,ANY-method
setMethod('getSerieFromGluedsequence','ContractSeq'
		,function(.Object, cseq,freq=86400,dblvol=FALSE,idxchk=FALSE,volcol=5  ){
				nameObject <- deparse(substitute(.Object))

				resetLogger(.Object,"INFO")
				iname <-getInstrumentName(.Object)
				cdf <- getContractSequence(.Object)
				xdata <- .Object@data
				datamonths <- unlist(lapply(.Object@data,function(x){x$month}))

				glueseq <- cseq
				 
				xd1 <- c()
				xintrd <- c()
				datamonths <- unlist(lapply(.Object@data,function(x){x$month})) 
				#browser()
				for(j in 1:length(glueseq)){ ##### !!!!!!!!  in 2:length
					cfm <- glueseq[[j]]$cfmonth
					cnm <- glueseq[[j]]$c2month
					dsub <- glueseq[[j]]$datasub
					#daily in any case
					fcd1 <- .Object@data[[ which(datamonths == cfm) ]]$bd1
					xcd1<- fcd1[dsub]
					ixxcd1<-index(xcd1) ## probably do not need this line
					if( nrow(xcd1) == 0 &&  j == length(glueseq) )  { next } ## assert zero data can be legitime only on last contract
					if(dblvol){ # add volume of 2nd contract only for daily
						ncd1 <- .Object@data[[ which(datamonths == cnm) ]]$bd1
						nxcd1 <- ensureXtsindex(ncd1[dsub],xcd1)
						volfn <- Vo(xcd1) + Vo(nxcd1)
						xcd1[,volcol] <- volfn
					}
					
					xd1 <- rbind(xd1, xcd1)
					##intraday 
					if(freq < 86400  && freq > 0){
						cmnth_idx <- match(cfm,  unlist(lapply(.Object@intrdata,function(x){x$month})))
						freq_idx  <- match(as.character(freq)
										  ,unlist(lapply(.Object@intrdata[[cmnth_idx]]$idat
										  ,function(y){y$freq})))
						fcintrad <- .Object@intrdata[[cmnth_idx]]$idat[[freq_idx]]$xdat	                    
						xfcintrad <- fcintrad[dsub]
						xintrd <- rbind(xintrd,xfcintrad)
					}
				}
				###verify index with continuos contract d1 data
				if(idxchk){
				 contxdat <- .Object@contrefdata$bd1
				 contxdat <- contxdat[paste( index(first(xd1)),index(last(xd1)),sep="::")]
				 indexcrit <- index(xd1) %in% index(contxdat)
				 res <- which(indexcrit == FALSE)
				 if(length(res)>0){
					badpoints <- paste(index(contxdat)[res],collapse=" ")
					wmsg <- paste("[ContractSeq:getGluedserie]"
							,"instrument",iname
							,"mismatch between glued and continuous contract index",badpoints)
					logwarn(wmsg,logger=getLoggername(.Object))

				 }
				}
				#browser()
				if(freq == 86400){
					  return ( xd1 )
				  }else if(freq < 86400 && freq > 0){
					  return ( xintrd)
				  }else{
					return (NA)
				  }
				})


#' getGluedSerie
#' @export
#' @docType methods
setGeneric(name='getGluedserie'
				,function(.Object,stime='1998-01-01',etime=Sys.time()
						  ,freq=86400,rollon='volume',dblvol=F,idxchk=F,volcol=5)
						 {standardGeneric('getGluedserie')})
#' @aliases getGluedserie,ContractSeq,ANY-method
setMethod('getGluedserie','ContractSeq'
		,function(.Object,stime='1998-01-01',etime=Sys.time()
				 ,freq=86400,rollon='volume',dblvol=F,idxchk=F,volcol=5  ){

				 glueseq <- getGluedsequence(.Object, stime=stime,etime=etime,rollon=rollon)

                 return(  getSerieFromGluedsequence(.Object,cseq=glueseq
                 	                                ,freq=freq,dblvol=dblvol
                 	                                ,idxchk=idxchk,volcol=volcol) )
				  
				})



#' @keywords internal
ensureXtsindex<-function(xts, refxts){
	txts <- xts(rep(0,nrow(refxts) ),order.by=index(refxts) )
	mxts <- merge(txts,xts)
	mxts[is.na(mxts)] <- 0
	mxts <- mxts[,-1]
	oxts <-mxts[index(refxts)]
	return(oxts)
}

#' @docType method
#' @keywords internal
  setGeneric( name='getDatIndex'
		  ,function(object, contract, isTickerPref=TRUE){standardGeneric("getDatIndex")})
#' @aliases getDatIndex,ContractSeq,ANY-method
  setMethod('getDatIndex', 'ContractSeq'
		  ,function(object,contract, isTickerPref=TRUE){
			if(isTickerPref) {
				dtkrcontracts <- unlist(lapply(object@data, function(x){x$ticker}))
			}else{
				dtkrcontracts <- unlist(lapply(object@data, function(x){x$month}))
			}
			didx <- as.numeric(NA)
			didx <- match(contract,dtkrcontracts)		  	
			  return(didx)
		  })


#' store on disk
#' @export
#' @docType methods
setGeneric(name='persistSeq',function(.Object){standardGeneric('persistSeq')})
#' @aliases persistSeq,ContractSeq,ANY-method
setMethod('persistSeq','ContractSeq'
		,function(.Object ){
   nameObject <- deparse(substitute(.Object))
   iname <- getInstrumentName(.Object)
   itickerPref <- getTickerPref(.Object)
   cdf <- getContractSequence(.Object)

   isqdir <- paste(.Object@topstoredir, iname, sep="/") #topdir for sequence
   if( !file.exists(isqdir)) { dir.create(isqdir)}

   resetLogger(.Object, "INFO")
   ##save  full sequence object to rdat --deprecated
   isqrdatdir <- paste(isqdir, 'rdat',sep='/')
   if( !file.exists(isqrdatdir)) { dir.create(isqrdatdir)}
   # rdatfile<- paste(isqrdatdir, "/", iname, ".rdat", sep="")
   # save(nameObject, .Object, file=rdatfile )
   # msg <- paste("saved sequence to", rdatfile)
   # loginfo(msg, logger=getLoggername(.Object))

   ##save sequence metadata to csv file
   isqmetadir <- paste(isqdir, 'meta',sep='/')
   if( !file.exists(isqmetadir)) { dir.create(isqmetadir)}
   metasqfile<-paste(isqmetadir, "/",iname,"_seq.csv",sep="")
   write.csv(cdf, file=metasqfile,quote=F, row.names=FALSE)
   msg<-paste("saved sequence meta to", metasqfile)
   loginfo(msg, logger=getLoggername(.Object))

   ###save contract data
   datadir <- paste(isqdir,'contracts',sep="/")
   if( !file.exists(datadir)) { dir.create(datadir)}
   metacmonths <- as.character(cdf$contr_month)
   datacmonths <- unlist(lapply( .Object@data, function(x){x$month}))
   csvd1msg <- "saved individual contracts daily data: "
   for(j in 1:length(metacmonths) ){
	   cmonth <- metacmonths[j]
	   ccontr <- bareContract(iname, itickerPref, cmonth )
	   cidx <- getDatIndex(.Object, cmonth, isTickerPref=FALSE)
	   if(!is.na(cidx)){
		   contrsuff <- getContractSuffix(ccontr)
		   d1xts <- .Object@data[[cidx]]$bd1
		   dfreq <- periodicity(d1xts)$frequency

		   ##d1  csv   depricated
		   # d1barsdir <- paste(datadir, contrsuff, "bars", "d1", sep="/")
		   # if( !file.exists(d1barsdir)) { dir.create(d1barsdir, recursive=TRUE)}
		   # basefname <- paste(iname,"_",contrsuff,"_",dfreq, ".csv",sep="")
		   # dtnWriteBarsCsv(d1xts,dirname=d1barsdir, filename=basefname)
		   # csvd1msg <- paste(csvd1msg,".",sep="")
	 
		   #new dev
		   #save single contract data in individual .rdat file !!!
		   contractdir <- paste(datadir, contrsuff, sep="/")
		   if(!file.exists(contractdir)){ dir.create(contractdir,recursive=TRUE)}
		   nameObj <- paste(contrsuff,"_",dfreq,sep="")
		   fn <- paste(datadir,"/",contrsuff,"/",iname,"_",contrsuff,"_",dfreq,".rdat",sep="")
		   d1contrdat <-list(ticker = .Object@data[[cidx]]$ticker
							,month = .Object@data[[cidx]]$month
							,expiry = .Object@data[[cidx]]$expiry
							,d1start = .Object@data[[cidx]]$d1start
							,d1end = .Object@data[[cidx]]$d1end
							,bd1 = .Object@data[[cidx]]$bd1
							)
		   save(nameObj, d1contrdat, file=fn )
		   #browser()
	   }else{
		   wmsg <- paste("no d1 data found for contract",cmonth)
		   logwarn(wmsg,logger=getLoggername(.Object))
	   }
	   ### here we can add  storing intraday bars
   }
   loginfo(csvd1msg,logger=getLoggername(.Object))

   ##save continuos ref data to csv -deprecated
   # contdir <- paste(isqdir,'cont',sep="/")
   # if( !file.exists(contdir)) { dir.create(contdir)}
   # d1xts <- .Object@contrefdata$bd1
   # dfreq <- periodicity(d1xts)$frequency
   # basefname <- paste(iname,"_cont_",dfreq, ".csv",sep="")
   # dtnWriteBarsCsv(d1xts,dirname=contdir, filename=basefname)

   ##save contractseq with no data but with contrefdata daily + intraday
   .Object@data <- list()
   .Object@intrdata <- list()
   rdatnodatafile<- paste(isqrdatdir, "/", iname, "_nodata",  ".rdat", sep="")
   save(nameObject, .Object, file=rdatnodatafile )
   msg <- paste("saved sequence with no data to", rdatnodatafile)
   loginfo(msg, logger=getLoggername(.Object))


   return(invisible())
})


#' store on disk intrday contracts data
#'
#' @export
#' @docType methods
setGeneric(name='persistIntra',function(.Object,interval){standardGeneric('persistIntra')})
#' @aliases persistIntra,ContractSeq,ANY-method
setMethod('persistIntra','ContractSeq'
		,function(.Object,interval ){
   if(missing(interval) ){
	  stop("[ContractSeq:persistIntra validation] missing interval param")
   }
   iname <- getInstrumentName(.Object)
   itickerPref <- getTickerPref(.Object)
   cdf <- getContractSequence(.Object)
   isqdir <- paste(.Object@topstoredir, iname, sep="/") #topdir for sequence
   datadir <- paste(isqdir,'contracts',sep="/")

   if( !file.exists(datadir)) { 
	wmsg <- paste("ContractSeq:persistIntra validation"
				  ,"could not find an expected data storage folder", datadir)
	stop(wmsg)
   }
   dfreq <- as.character(interval)
   stor_contr <- unlist(lapply(.Object@intrdata, function(x){x$month})) 
   for (j in 1:length(stor_contr) ) {
	   cmonth <- stor_contr[j]
	   ccontr <- bareContract(iname, itickerPref, cmonth )
	   contrsuff <- getContractSuffix(ccontr)

	   contractdir <- paste(datadir, contrsuff, sep="/")
	   # namStorObj <- paste(contrsuff,"_",dfreq,sep="")
	   fn <- paste(datadir,"/",contrsuff,"/",iname,"_",contrsuff,"_",dfreq,".rdat",sep="")

	   intrvalidx <-match(as.character(interval)
						,unlist(lapply(.Object@intrdata[[j]]$idat
									   ,function(x){x$freq})))
	   if( is.na(intrvalidx)){
			wmsg <- paste("persistIntra: intraday data not found for instrument"
						,iname, "interval:",interval
						,"contract:",cmonth,sep=" ")
			logwarn(wmsg,logger=getLoggername(.Object))
			next        		
		}
		el <- .Object@intrdata[[j]]$idat[[intrvalidx]]
		icontrdat <- list(tickerpref=itickerPref,
						  month=cmonth,
						  contrsuff=contrsuff,
						  intr_contr_idx=j,
						  intr_intval_idx=intrvalidx,
						  eldat=el)
		save(icontrdat,file=fn)
		msg <- paste('stored dfreq data for contract',cmonth,'in',fn)
		loginfo(msg,logger=getLoggername(.Object))
   }

   return(invisible())
})


#' @export
#' @docType methods
setGeneric( name='cshelp',function(object){standardGeneric("cshelp")})
#' @aliases cshelp,ContractSeq,ANY-method
setMethod('cshelp'
	 ,'ContractSeq'
	 ,function(object){
	  cat("######## Contract Sequence object operations functions help #######\n")
	  cat("cshelp(o) ::\t  show help on operational methods \n")

	  cat("\n****** cs creation functions  ******\n\n")

	  cat("o <- crBareContractSeq(iname, tkpref, yrcontracts,  cstart, cend, datasrc,topdir)\n")
	  cat("\t\t::\t create contract sequence; cstart first contract, cend - last contract\n")

	  cat("setRegularRollRule(o, rollprd,fmvolpct)\n")
	  cat("\t\t ::\t set the same roll rule for each contract in a year \n")
	  cat("\t\t rollprd - number of days prior to expiry to make a window to search a volume based roll \n")
	  cat("\t\t fmvolpct - percent threshold of front month volume to seek a roll date when volume goes below it \n")

	  cat("setRollRule(o,value)<- ::\t set specific role rule \n")
	  cat("findRollonvol(o,decr=T) \n")
	  cat("\t\t::\t find volume roll dates using  rollrule property after initial data donwload\n")

	  cat("\n**** cs data donwload functions ***\n\n")
 
	  cat("dtnConf(host) ::\t  set dtn Iqfeed config for host (values: vm, cern,home) \n")
	  cat("iniDownload(o, lstqtime, persist=T)\n")
	  cat("\t\t::\t initial daily data download upto lstqtime  date \n")

	  cat("iniDownloadIntra(o, lstqtime, interval=3600,lfactor=1.33,startContract=NA,persist=F)\n")
	  cat("\t\t::\t initial intraday data download of specified upto lstqtime  date \n")

	  cat("toPrdIntra(o, sinterval=3600,tointerval=7200)\n")
	  cat("\t\t::\t convert intraday data to a higher frequency \n")

	  cat("allPrdIntra(o,contracts=NA)\n")
	  cat("\t\t::\t convert all intraday data frequencies according to o@intradconf vector  on contracts vector (NA = all)\n")


	  cat("updateEOD(o,scontract=Sys.time(),ctime=Sys.time(),updMeta=T,persist=F) \n")
	  cat("\t\t::\t  download update data in unexpired contracts up to date \n")
	  cat("\t\t updMeta if true tries to  fill expiries and find volume roll in current unexpired contract (to be tested!  \n")
	  cat("\t\t if scontract != Sys.time() (re)download the data starting from a specified contract  \n")
 
	  cat("updateIntra(o,lstqtime=Sys.time(),interval=3600,startcontract=NA,verbose=T,persist=F) \n")
	  cat("\t\t::\t  download intraday update data in unexpired contracts or from startContract up to date \n")
 

	  cat("chkData(o) ::t check  that meta information on contract start end data are in sync with real data \n")
   

	  cat("\n**** cs monthly operations functions ***\n\n")

	  cat("addFutureContracts(o, nmonths=6) ::\t append contracts to the last contract for forward 6 months \n")
	  cat("fillExpiries(o,persist=T,kkeys ) \n")
	  cat("\t\t::\t fills NA expiries  from google spreadsheets using a kkeys for _expirs_keyset sheet\n")

	  cat("updateDataExpiries(o) \n")
	  cat("\t\t::\t fills NA expiries  from sequence downloaded daily market data \n")

	  cat("\n******* cs daily operation functions (single instrument)  ******\n\n")

	  cat("o<-readcs(iname,topstoredir) ::\t  Read contract sequence for instrument from default topstoredir \n")

	  cat("o<-nreadcs(iname,topstoredir) ::\t  Read contract sequence metadata and separate contracts data files \n")
	  cat("loadIntraday(o,interval=3600,ctr_start,ctr_end,alldata=NA,verbose=T) ::\t  load intraday data \n")
	  cat("\t\t::\t  from separate files, either for all contracts in sequence (alldata=1) or for specified contracts\n")
   

	  cat("getUnexpiredContract(o,ctime=Sys.time(),n=1) ::\t  get nth front month contract for ctime \n")
	  cat("printLiquidity(o,ctime=Sys.time(),n=10) ::\t  print contracts volume liquidity for ctime back to n days \n")
	  cat("chkprintEODroll(o,ctime=Sys.time(),n=6) ::\t  attempt to detect and print possible volume roles in last n days\n")
	  cat("chkMeta(o) ::\t  check sync betwen objects contractseq data frame and contents of data frame from meta csv file \n")
	  cat("fixMetafromcsv(o,persist=T) ::\t set meta information from a csv file under topstoredir contract meta subfolder\n")
	  cat("updateMeta(o,ctime=Sys.time() ) ::\t  attempt to find volume roll in a current unexpired contract \n")
	  cat("getGluedsequence(o,stime,etime,rollon)\n")
	  cat("\t\t ::\t  get a sequence for glueing contract serie based on rollon param (volume, expiry)\n")
	  cat("getGluedserie(o,stime,etime,freq,rollon,dblvol,idxchk,volcol) ::\t  get a glueed contract serie\n")
	  cat("\t\t rollon is (volume|expiry) , dblvol is (TRUE|FALSE) whether to  sum front and next month volumes \n")
	  cat("\t\t idxchk (TRUE|FALSE) whether to verify date index with ref continuous serie supplied by data provider \n")
	  cat("*** cs storage functions  ***\n")

	  cat("persistSeq(o) ::\t  save contract sequence data to disk, (rdat + csv ) \n")
	  cat("persistIntra(o,interval) ::\t  save itraday contracts data sequence in separate rdat files \n")

	  cat("\n*** csl contract seq list functions  ***\n\n")

	  cat("o<-readCSL(topstoredir) ::\t  read all contract sequence from specified or default topstoredir \n")
	  cat("getInstruments(o) ::\t get a vector of instruments of current contract sequence list \n")
	  cat("updEOD(o, ctime=Sys.time(),updMeta=T,persist=T) :\t  wrapper to run updateEOD() on every contract sequence in a list \n")
	  cat("printliq(o, ctime=Sys.time(),n=10) :\t  wrapper to run printLiquidity() on every contract sequence in a list \n")
	  cat("persistcsl(o ) :\t  wrapper to run persistSeq() on every contract sequence in a list \n")

	  return( invisible() )
	  })


#' @keywords internal
setGeneric( name='appendIntraDayMeta'
		   ,function(object){standardGeneric("appendIntraDayMeta")})
setMethod('appendIntraDayMeta', 'ContractSeq'
		  ,function(object){
			s1fmt <- '%Y-%m-%d %H:%M:%S'
			df<- getContractSequence(object)

			ia <- rep(NA, length(df$contr_month))
			intrdatmnths <- unlist(lapply(object@intrdata,function(x){x$month}))
			crit1 <- df$contr_month %in% intrdatmnths
			idx1 <-  which(crit1 ==TRUE)
			intr_months <- ia
			intr_months[idx1] <- intrdatmnths

			lo_intval <- min(unlist(lapply(object@intrdata
										  ,function(x){
											 min(unlist(lapply(x$idat
															   ,function(y){y$freq}
															   )))
										  })))
			dt_start <- unlist(lapply(object@intrdata
									,function(el){
									  intvidx <- match(as.character(lo_intval)
													 ,unlist(lapply(el$idat
															 ,function(y){y$freq})))
									  ixdat <- el$idat[[intvidx]]$xdat
									  dts <-  strftime(index(first(ixdat))
#        		                      	               ,format=s1fmt,tz=indexTZ(ixdat))
													   ,format=s1fmt)
									  return(dts)             	     	
									}))
			intr_strt <- ia
			intr_strt[idx1] <- dt_start
			dt_end  <-  unlist(lapply(object@intrdata
									,function(el){
									  intvidx <- match(as.character(lo_intval)
													 ,unlist(lapply(el$idat
															 ,function(y){y$freq})))
									  ixdat <- el$idat[[intvidx]]$xdat
									  dte <-  strftime(index(last(ixdat))
#        		                      	              ,format=s1fmt,tz=indexTZ(ixdat))
													  ,format=s1fmt)
									  return(dte)             	     	
									}))
			intr_end <- ia
			intr_end[idx1] <- dt_end
			#browser()
			cnames <- colnames(df)
			df<-cbind(df,intr_months,intr_strt,intr_end)
			colnames(df) <- c(cnames,paste("intrday_",as.character(lo_intval),sep=""),"dt_strt","dt_end")
	  
			return(df)
		  })


###helper functions
setMethod ('show'
		,'ContractSeq'
		, function(object){
			

			cat("*** Type ContractSeq *** \n")
			df<- getContractSequence(object)
			print(paste("first contract:", df$contr_month[1]
							,"last contract:", df$contr_month[nrow(df)] ))
			icfg <- object@intrdconf
			if(length(icfg)>0 ){
				cat("* intraday config \n")
				print(paste('outcry session hours:',icfg$outcrysession, icfg$outcrytimezone))
				print(paste('conf for intraday frequncies:'
							,paste(icfg$intradayfreq, collapse=" ")))
				print(paste('tick size:',icfg$tick_size
						   ,'tick contract price:',icfg$tick_price
						   ,'contract size:',icfg$tick_price / icfg$tick_size
						   ,'currency:',icfg$imnt_currency))
				print(paste('#### glued serie daily double volume:'
							,icfg$daydoublevolume,'####'))
			}


			cat("* contract roll rule \n")
			cm<- unlist(lapply(object@rollrule,function(x)x$cmonth))
			prd<- unlist(lapply(object@rollrule,function(x)x$rollprd))
			vpct<- unlist(lapply(object@rollrule,function(x)x$fmvolpct))
			rdf <-cbind(cm,prd,vpct)
			colnames(rdf)<-c("contr_month", "roll_window", "front_volume_pct")
			print(as.data.frame(rdf))

			cat("* contract sequence \n")

			#new dev

			if(length(object@intrdata)>0){
				df <- appendIntraDayMeta(object)
			}

			print(df)

			
			cat("sequence has ", length(object@data), "  contracts with daily data\n" )
			if(length(object@intrdata)>0){  
					cat("sequence also has ", length(object@intrdata)
						, " contracts with intraday data with frequencies: "
						,paste(object@intrdconf$intradayfreq,collapse=" "),"\n") 
			}

			print(paste("* Instrument specs:", object@instName, object@tickerPref, object@topstoredir ))
			print(paste("last daily data", object@lastDataDaily))
			cat("******************************* \n")
		})

#' version of show(object) to print current contracts as of ctime
#' 
#' @export
#' @docType methods
setGeneric(name='showcur'
				,function(object,ctime=Sys.time()){standardGeneric('showcur')}) 
#' @aliases showcur,ContractSeq,ANY-method
setMethod('showcur','ContractSeq'
		,function(object,ctime=Sys.time() ){
			df<- getContractSequence(object)

			icfg <- object@intrdconf
			if(length(icfg)>0 ){
				print(paste('tick size:',icfg$tick_size
						   ,'tick contract price:',icfg$tick_price
						   ,'contract size:',icfg$tick_price / icfg$tick_size
						   ,'currency:',icfg$imnt_currency))
				print(paste('#### glued serie daily double volume:'
							,icfg$daydoublevolume,'####'))
			}

			cat("* contract sequence \n")
			if(length(object@intrdata)>0){
				df <- appendIntraDayMeta(object)
			}
			if(length(object@intrdata)>0){
				df <- appendIntraDayMeta(object)
			}
			# two last expired contract as of ctime
			exprdMon <- getUnexpiredContract(object, ctime=ctime,n=-1)
			sidx <- match(exprdMon, df[,2])
			print(df[sidx:nrow(df),])
			print(paste("* Instrument specs:", object@instName, object@tickerPref, object@topstoredir ))
			print(paste("last daily data", object@lastDataDaily))
			cat("******************************* \n")

})

#' @export
#' @docType methods
setGeneric( name='is.ContractSeq',function(object){standardGeneric("is.ContractSeq")})
#' @aliases is.ContractSeq,ContractSeq,ANY-method
setMethod('is.ContractSeq'
		,'ContractSeq'
		,function(object){
			return ( is(object, 'ContractSeq') )
		})


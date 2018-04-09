
#'
#' contracts seq list. Proxy object for a list of contract sequences
#'
#' S4 container which can hold daily and  | or intraday market data for several
#' data providers. Contains methods to manage the list of instruments and to manage the
#' associated market data
#' @rdname ContractSeqList-class
#' @name ContractSeqList
setClass(Class='ContractSeqList'
		,representation(csl='list'
				,topstoredir='character'
                ,loggername='character'
)
		,prototype(csl=list()
                ,topstoredir=as.character(NA)
                ,loggername='CSL'
		)
)



## to  helper function to instantiate CSL from persistent storage
#' @export
nreadCSL<-function(ivec=NA,topstoredir="~/googledrive/mkdata/dtn/contrseq"
	              ,freq=60,intrd="now"){

	o <- new('ContractSeqList')
	##resetting logger
	setLogger(o)
	if(is.na(ivec[1])){
    	ivec <- list.files(topstoredir)
    }
    proc_nread <- function(x,td,fr,ntrd){
    	 cs <- nreadcs(x,topstoredir=td,freq=fr,intrd=ntrd)
    	return(cs)
    } 
    lvec <-as.list(ivec)
    csl <- lapply(lvec,FUN=proc_nread,td=topstoredir,fr=freq,ntrd=intrd)

    o@csl <- csl
 	o@topstoredir <- topstoredir
	return(o)
}


## to  helper function to instantiate CSL from persistent storage
#' @export
nCSLfromcfg<-function(cfgfn,freq=60,intrd="now"){
	lcfg <- parseCSListCfg(cfgfn)
	o <- nreadCSL(ivec=lcfg$inames, topstoredir=lcfg$datadir,freq=freq,intrd=intrd)
	return(o)
}

## to be deprecated helper function to instantiate CSL from persistent storage
#' @export
readCSL<-function( topstoredir="~/googledrive/mkdata/dtn/contrseq"){

	o <- new('ContractSeqList')
	##resetting logger
	setLogger(o)
    instvec <- list.files(topstoredir)

	for(j in 1:length(instvec)){
		iname <- instvec[j]
		fn<-paste(topstoredir,"/",iname,"/","rdat","/",iname,".rdat",sep="")
		if( !file.exists(fn)){
			msg <- paste("instrument",iname,"missing contract seq file",fn)
			logwarn(msg,logger=o@loggername)
		}else{
		 ccs <- readcs(iname,topstoredir=topstoredir)
		 o@csl[[ length(o@csl)+1 ]] <- ccs
	    }
	}
	o@topstoredir <- topstoredir
	return(o)
}

#' @export
#' @docType methods
setGeneric(name='getInstruments',function(object){standardGeneric('getInstruments')})
#' @aliases getInstruments,ContractSeqList,ANY-method
setMethod('getInstruments','ContractSeqList'
		,function(object){
			inames <- unlist(lapply(object@csl, function(x)x@instName))
			return(inames)
		})

#' @docType method
#' @export
setGeneric( name='setLogger',function(object){standardGeneric("setLogger")})
#' @aliases setLogger,ContractSeqList,ANY-method
setMethod('setLogger', 'ContractSeqList',function(object){
			lg<-getLogger(object@loggername)
			lg$addHandler(writeToConsole,level="INFO")
      	    return(invisible())
		})

#' @export
#' @docType methods
setGeneric(name='getCS',function(object,iname){standardGeneric('getCS')})
#' @aliases getCS,ContractSeqList,ANY-method
setMethod('getCS','ContractSeqList'
		,function(object,iname){
			inames <- getInstruments(object)
			idx <-match( iname,inames)
			if(!is.na(idx)){
				return ( object@csl[[ idx ]]  )
			}else{
				return(NA)
			}
		})

#' @export
#' @docType methods
setGeneric(name='setCS<-'
		,function(object,value){standardGeneric('setCS<-')})
#' @aliases setCS<-,ContractSeqList,ANY-method
setReplaceMethod('setCS'
		,'ContractSeqList'
		,function(object,value){
			##validate input
			if (  !is.ContractSeq(value) ){
				stop(paste("[ContractSeqList:setCS validation]"
								,"parameter is not of type ContractSeq"))
			}
			inames <- getInstruments(object)
			iname <- getInstrumentName(value)
			idx <-match( iname,inames)
			if(!is.na(idx)){
				object@csl[[ idx ]] <- value
			}else{
				object@csl[[ length(object@csl) + 1 ]] <- value
			}
			return (object)
		})

#' update daily data EOD for instrument lsit
#' @export
#' @docType methods
setGeneric(name='updEOD',function(.Object, ctime=Sys.time() , updMeta=FALSE, persist=FALSE){standardGeneric('updEOD')})
#' @aliases lupdEOD,ContractSeqList,ANY-method
setMethod('updEOD','ContractSeqList'
		,function(.Object,  ctime=Sys.time(), updMeta=FALSE,persist=FALSE ){

			nameObject <- deparse(substitute(.Object))
            csl <- .Object@csl 
            proc_upd <- function(x,ct,um,ps){
            	updateEOD(x,ctime=ct,updMeta=um,persist=ps)
            	return(x)
            } 
            csl <- lapply(csl,FUN=proc_upd,ct=ctime,um=updMeta,ps=persist)

            .Object@csl <- csl
			assign(nameObject,.Object,envir=parent.frame())
			return( invisible())
			})

#' update intraday data for instrument lsit
#' @export
#' @docType methods
setGeneric(name='updIntra',function(.Object, ctime=Sys.time(),interval=60,persist=FALSE){standardGeneric('updIntra')})
#' @aliases updIntra,ContractSeqList,ANY-method
setMethod('updIntra','ContractSeqList'
		,function(.Object,ctime=Sys.time(),interval=60,persist=FALSE ){

			nameObject <- deparse(substitute(.Object))
            csl <- .Object@csl 
            proc_upd <- function(x,ival,ct,ps){
            	sc<- getUnexpiredContract(x,ctime=ct,n=1)
            	ec<- getUnexpiredContract(x,ctime=ct,n=2)
            	front_contr <- getContract(x,sc)
            	volrollDte<-getOnVolumeRoll(front_contr)
            	#if volroll is not yet determined
            	if(is.na(volrollDte)) { volrollDte <- ct}
            	#if ctime > volumeroll date, update front and first contracts
            	if(as.POSIXct(ct)>as.POSIXct(volrollDte)){
            		updateIntra(x,interval=ival,lstqtime=ct,startContract=sc,endContract=ec,persist=ps)
            	}else{
            		updateIntra(x,interval=ival,lstqtime=ct,persist=ps)
            	}
            	return(x)
            } 
            csl <- lapply(csl,FUN=proc_upd,ival=interval,ct=ctime,ps=persist)
            .Object@csl <- csl
			assign(nameObject,.Object,envir=parent.frame())
			return( invisible())
			})


#' print liquidity for instrument lsit
#' @export
#' @docType methods
setGeneric(name='lstliq',function(object, ctime=Sys.time(), n=10){standardGeneric('lstliq')})
#' @aliases lstliq,ContractSeqList,ANY-method
setMethod('lstliq','ContractSeqList'
		,function(object,  ctime=Sys.time(), n=10 ){
            
            proc_liq <-function(x,ct,ndays){
            	print(paste("instrument:",getInstrumentName(x))) 
            	printLiquidity(x,ctime=ct,n=ndays)
            }
			lapply(object@csl,FUN=proc_liq,ct=ctime,ndays=n)
			return( invisible())
		})



#' persist for instrument list eod data
#' @export
#' @docType methods
setGeneric(name='storecsl',function(.Object ){standardGeneric('storecsl')})
#' @aliases  storecsl,ContractSeqList,ANY-method
setMethod('storecsl','ContractSeqList'
		,function(.Object ){

	         lapply(.Object@csl, function(x){persistSeq(x)})
			return( invisible())
		})

#' persist for instrument list intradate data
#' @export
#' @docType methods
setGeneric(name='storeIntracsl',function(.Object,interval ){standardGeneric('storeIntracsl')})
#' @aliases storeIntracsl,ContractSeqList,ANY-method
setMethod('storeIntracsl','ContractSeqList'
		,function(.Object,interval ){ 
	         lapply(.Object@csl, function(x,ival){persistIntra(x,ival)},ival=interval)
			return( invisible())
		})


#' show current conttracts in instrument list
#' @export
#' @docType methods
setGeneric(name='lstcur',function(object, ctime=Sys.time())
									{standardGeneric('lstcur')})
#' @aliases lstcur,ContractSeqList,ANY-method
setMethod('lstcur','ContractSeqList'
		,function(object,  ctime=Sys.time()){
            proc_print <-function(x,ct){
            	print(paste("instrument:",getInstrumentName(x))) 
            	showcur(x,ctime=ct)
            }
			lapply(object@csl,FUN=proc_print,ct=ctime)                     
			return( invisible())
		})


###helper functions
setMethod('show'
	  ,'ContractSeqList'
	  ,function(object){
		cat("*** Type ContractSeqList *** \n")
        for ( j in 1:length(object@csl)){
				ccs <- object@csl[[j]]
				show(ccs)
	    }
       print("instruments in the list:")
       print(getInstruments(object))
	   cat("*******************************\n") 
       return ( invisible() )
       })

#' @export
#' @docType methods
setGeneric( name='is.ContractSeqList',function(object){standardGeneric("is.ContractSeqList")})
#' @aliases is.ContractSeqList,ContractSeqList,ANY-method
setMethod('is.ContractSeqList'
		,'ContractSeqList'
		,function(object){
			return ( is(object, 'ContractSeqList') )
		})


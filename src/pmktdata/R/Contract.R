##Contract Class with

##class declaration with validation
setClass(Class="Contract"
         ,representation(iName="character"
                         ,tickerPref="character"
                         ,month="POSIXct"
                         ,expiryDate="POSIXct"
                         ,rollOnDvolumeDate="POSIXct"
                         ,rollOnFixDayDate="POSIXct"
                         ,monthdict="character"
           )
         ,prototype(iName= 'undefined instrument'
                    ,tickerPref='undefined ticker'
                    ,month=as.POSIXct(NA)
                    ,expiryDate=as.POSIXct(NA)
                    ,rollOnDvolumeDate=as.POSIXct(NA)
                    ,rollOnFixDayDate=as.POSIXct(NA)
                    ,monthdict=c('F','G','H', 'J','K','M','N','Q','U', 'V', 'X', 'Z')
          )
         ,validity=function(object){
               #cat("~~~ Contract: validity inspector ~~~ \n")
               if ( length(object@iName) == 0 ){
                 stop("[Contract: validation] instrument name is not specified")
               }else if ( !is.numeric(unclass(object@month)) ) {
                 stop("[Contract: validation] month is not specified as POSIXct")
               }
             return(TRUE)
           }
         )


##user friendly classic function for constraactor
#' @export
dummyContract <- function(name,month){
	o <- new("Contract", iName=name, tickerPref=name, month=as.POSIXct(month) )
	return(o)
}

#' @export
bareContract <- function(name, ticker, month){
  o <- new("Contract", iName=name, tickerPref=ticker, month=as.POSIXct(month) )
  return(o)
}

#' @export
basicContract <-function(name, ticker, month,expiry){
  o <- new("Contract", iName=name, tickerPref=ticker, month=as.POSIXct(month), expiryDate=as.POSIXct(expiry) )
  return(o)
}

fullContract <-function(name, ticker, month,expiry,rollFixed,rollVol) {
  o <- new("Contract",iName=name, tickerPref=ticker
                     ,month=as.POSIXct(month), expiryDate=as.POSIXct(expiry)
                     ,rollOnFixDayDate=as.POSIXct(rollFixed)
                     ,rollOnDvolumeDate=as.POSIXct(rollVol)
           )
  return(o)
}


##########################
##getters and setters
setGeneric( name='getIname',function(object){standardGeneric("getIname")})
setMethod('getIname', 'Contract'
          ,function(object){
               return(object@iName)
           }
         )
setGeneric( name='getContractMonth',function(object,...){standardGeneric("getContractMonth")})
setMethod('getContractMonth', 'Contract'
          ,function(object, charfmt=F){
			  d1fmt<-"%Y-%m-%d"
              ny <- as.numeric(strftime(object@month, format='%Y'))
              if (!charfmt) {
                   return( strftime(object@month, format=d1fmt)  )
               }
               else {
                   return ( strftime(object@month, format="%B%y" ) )
               }
           })
   
setGeneric( name='getContractSuffix',function(object){standardGeneric("getContractSuffix")})
setMethod('getContractSuffix', 'Contract'
		   ,function(object){
			   cy <- strftime(object@month, format='%y')
			   nmonth <- as.numeric(strftime(object@month, format='%m'))
			   return ( paste(object@monthdict[nmonth],cy,sep="") )
		   })   

setGeneric( name='getExpiry',function(object){standardGeneric("getExpiry")})
setMethod('getExpiry', 'Contract'
          ,function(object){
			  d1fmt<-"%Y-%m-%d"
               return(strftime(object@expiryDate, format=d1fmt) )
              })
setGeneric(name="setExpiry<-",function(object,value){standardGeneric("setExpiry<-")})
	  setReplaceMethod( f="setExpiry"
			  ,signature="Contract"
			  ,definition=function(object,value){
				  object@expiryDate <-as.POSIXct(value)
				  ##validate input
				  if ( !is.numeric(unclass(object@expiryDate)) ) {
					  stop("[Contract: validation] expiryDate is not specified as POSIXct")
				  }
				  return (object)
			  })	  

setGeneric( name='getFixDayRoll',function(object){standardGeneric("getFixDayRoll")})
setMethod('getFixDayRoll', 'Contract'
          ,function(object){
			d1fmt<-"%Y-%m-%d"
            return(strftime(object@rollOnFixDayDate,format=d1fmt))
           })
setGeneric(name="setFixDayRoll<-",function(object,value){standardGeneric("setFixDayRoll<-")})
setReplaceMethod( f="setFixDayRoll"
                 ,signature="Contract"
                 ,definition=function(object,value){
                             object@rollOnFixDayDate <-as.POSIXct(value)
                             ##validate input
                             if ( !is.numeric(unclass(object@rollOnFixDayDate)) ) {
                                stop("[Contract: validation] rollOnFixDayDate is not specified as POSIXct")
                             }
                             return (object)
                             }
                )

setGeneric( name='getOnVolumeRoll',function(object){standardGeneric("getOnVolumeRoll")})
setMethod('getOnVolumeRoll', 'Contract'
          ,function(object){
		  d1fmt<-"%Y-%m-%d"
		  return(strftime(object@rollOnDvolumeDate, format=d1fmt) )		  
           }
         )

setGeneric(name="setOnVolumeRoll<-",function(object,value){standardGeneric("setOnVolumeRoll<-")})
setReplaceMethod( f="setOnVolumeRoll"
                  ,signature="Contract"
                  ,definition=function(object,value){
                    object@rollOnDvolumeDate <-as.POSIXct(value)
                    ##validate input
                    if ( !is.numeric(unclass(object@rollOnDvolumeDate)) ) {
                      stop("[Contract: validation] rollOnDvolumeDate is not specified as POSIXct")
                    }
                    return (object)
                  })

setGeneric( name='getContractTicker',function(object){standardGeneric("getContractTicker")})
setMethod('getContractTicker', 'Contract'
          ,function(object){
            cyr <- strftime(object@month, format='%y')
            nm <- as.numeric(strftime(object@month, format='%m'))
            cm <- object@monthdict[nm]
            tkr <- paste(object@tickerPref,cm,cyr,sep="")
            return(tkr)
          }
          )

###helper functions
setMethod ('show'
           ,'Contract'
           , function(object){
              cat("*** Class Contract, method Show *** \n")
              print(paste("* Instrument specs:"
                          ,object@iName, getContractTicker(object) ))
              print(paste("month:", object@month
                         ,"expiry:", object@expiryDate
                         ,"rollOnDayVolume:",object@rollOnDvolumeDate
                         ,"rollOnFixedDay:",object@rollOnFixDayDate ))
              cat("******* End Show (Contract) ******* \n")
           })

setGeneric( name='is.Contract',function(object){standardGeneric("is.Contract")})
setMethod('is.Contract'
          ,'Contract'
          ,function(object){
            return ( is(object, 'Contract') )
          })





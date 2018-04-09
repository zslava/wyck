# TODO: Add comment
#
# Author: zimine
###############################################################################


#' S4 object which holds an Object for a Trade
#' 
#' @rdname Trade-class
#' @name Trade
#' @exportClass Trade
setClass(Class="Trade"
         ,representation( tid="numeric"
         	               ,stratid="numeric"
         	               ,accid="numeric"
         	               ,itkr="character"
                         ,inceptDate="POSIXct"
         	               ,tstatus="character"  # c cancelled, y exposure, n no exposure
                         ,tqty="numeric"
                         ,entry="numeric"
                         ,stop="numeric"
                         ,ftarget="numeric"
                         ,mtarget="numeric"
                         ,comment="character"
                         ,torders ="list"  #vector with order ids
          )
         ,prototype( tid=0
         	        ,stratid=0
         	        ,accid=1
         	        ,itkr=as.character(NA)
         	        ,inceptDate=as.POSIXct(NA)
         	        ,tstatus='n'
         	        ,tqty=0
         	        ,entry=0
         	        ,stop=0
         	        ,ftarget=0
         	        ,mtarget=0
         	        ,comment='NA'
         	        ,torders=list()
          )
        ) 

##user friendly classic function for constructors
### dummy trade used to check RR ratios for potential trades
#' @export
mkDummyTrade <- function(ticker, incdate, entry, stop, ftarg, mtarg){
    
    o <- new("Trade", itkr=ticker, inceptDate=as.POSIXct(incdate)
                     ,entry=entry, stop=stop, ftarget=ftarg, mtarget=mtarg)
    return(o)
}

#' @export
mkTrade <- function(strat, acc, tkr
	               ,incDate,  qty, entry, stop, ftarg, mtarg){

    ##input validation
    if( !is.numeric(unclass(as.POSIXct(incDate)))){
 		 stop(paste("Trade:mkTrade validation]"
								,"incDate param expected to be a datetime string"))
	}


	o <- new("Trade" ,stratid=strat, accid=acc, itkr=tkr
		             ,inceptDate=as.POSIXct(incDate)
		             ,tqty=qty, entry=entry, stop=stop
		             ,ftarget=ftarg, mtarget=mtarg)
	return(o)
}


#' @export
crTradeFromrec<- function(tlist){
    ##input validation
    if( !is.list(tlist)){
 		 stop(paste("[Trade:crTradeFromrec validation]"
								,"tlist param expected to be a list with all trade properties"))
	}  
  #browser()
  sfmt<-"%Y-%m-%d %H:%M:%S"
	o <- new("Trade", tid=tlist$tid, stratid=tlist$strat, accid=tlist$acc, itkr=tlist$tkr
		            ,inceptDate=as.POSIXct(tlist$incdate,format=sfmt)  # new
                ,tstatus=tlist$status
		            ,tqty=tlist$tqty, entry=tlist$entry, stop=tlist$stop
		            ,ftarget=tlist$ftarget, mtarget=tlist$mtarget, comment=tlist$comment )
    return(o)  
}



### biz methods

#' @export
#' @docType method
setGeneric( name='getTid'
           ,function(object){standardGeneric("getTid")})
#' @aliases getTid,Trade,ANY-method
setMethod('getTid', 'Trade' ,function(object){ return(object@tid) })
#' @export
#' @docType methods
  setGeneric(name='setTid<-'
		  ,function(object,value){standardGeneric('setTid<-')})
#' @aliases setTid,Trade,ANY-method
  setReplaceMethod('setTid'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0  ){
				  stop(paste("[Trade:setTid validation]"
						,"supplied param for id expected to be a positive integer"))
			  }
			  object@tid <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getStratid'
           ,function(object){standardGeneric("getStratid")})
#' @aliases getStratid,Trade,ANY-method
setMethod('getStratid', 'Trade' ,function(object){ return(object@stratid) })
#' @export
#' @docType methods
  setGeneric(name='setStratid<-'
		  ,function(object,value){standardGeneric('setStratid<-')})
#' @aliases setStratid,Trade,ANY-method
  setReplaceMethod('setStratid'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0  ){
				  stop(paste("[Trade:setStratid validation]"
						,"supplied param for id expected to be a positive integer"))
			  }
			  object@stratid <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getTaccount'
           ,function(object){standardGeneric("getTaccount")})
#' @aliases getTaccount,Trade,ANY-method
setMethod('getTaccount', 'Trade' ,function(object){ return(object@accid) })
#' @export
#' @docType methods
  setGeneric(name='setTaccount<-'
      ,function(object,value){standardGeneric('setTaccount<-')})
#' @aliases setTaccount,Trade,ANY-method
  setReplaceMethod('setTaccount'
                  ,'Trade'
      ,function(object,value){
          #validation
        if ( !is.numeric(value) || value < 0  ){
          stop(paste("[Trade:setTaccount validation]"
            ,"supplied param for accid expected to be a positive integer"))
        }
        object@accid <- value
        return(object)
      })



#' @export
#' @docType method
setGeneric( name='getTticker'
           ,function(object){standardGeneric("getTticker")})
#' @aliases getTticker,Trade,ANY-method
setMethod('getTticker', 'Trade' ,function(object){ return(object@itkr) })
###no method for setTticker<-
#


#' @export
#' @docType method
setGeneric( name='getTstatus'
           ,function(object){standardGeneric("getTstatus")})
#' @aliases getTstatus,Trade,ANY-method
setMethod('getTstatus', 'Trade' ,function(object){ return(object@tstatus) })
#' @export
#' @docType methods
  setGeneric(name='setTstatus<-'
		  ,function(object,value){standardGeneric('setTstatus<-')})
#' @aliases setTstatus,Trade,ANY-method
  setReplaceMethod('setTstatus'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
		  	  okStatus <- c("c", "y", "n")
			  if ( !(value %in% okStatus) ){
				  stop(paste("[Trade:setStatus validation]"
						,"supplied param for status does not match valid Status c, y or n"))
			  }
			  object@tstatus <- value
			  return(object)
		  })


#' @export
#' @docType method
setGeneric( name='getTincDate'
           ,function(object,chr=FALSE){standardGeneric("getTincDate")})
#' @aliases getTincDate,Trade,ANY-method
setMethod('getTincDate', 'Trade' ,function(object,chr=FALSE){ 
     out <- object@inceptDate
     if(chr) out <- base::format(object@inceptDate, format="%Y-%m-%d %H:%M:%S" ) 
     return(out)
})


###no method for setTncDate<-
#' @export
#' @docType methods
 setGeneric(name='setTincDate<-'
      ,function(object,value){standardGeneric('setTincDate<-')})
#' @aliases setTincDate,Trade,ANY-method
 setReplaceMethod('setTincDate'
                  ,'Trade'
      ,function(object,value){
          #validation
        if ( !is.numeric(unclass(as.POSIXct(value))) ){
          stop(paste("[Trade:setTincDate validation]"
            ,"supplied param for incept date expected to be a POSIXct format string"))
        }
        object@inceptDate <- as.POSIXct(value)
        return(object)
      })

#' @export
#' @docType method
setGeneric( name='getTqty'
           ,function(object){standardGeneric("getTqty")})
#' @aliases getTqty,Trade,ANY-method
setMethod('getTqty', 'Trade' ,function(object){ return(object@tqty) })
#' @export
#' @docType methods
 setGeneric(name='setTqty<-'
		  ,function(object,value){standardGeneric('setTqty<-')})
#' @aliases setTqty,Trade,ANY-method
 setReplaceMethod('setTqty'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) ){
				  stop(paste("[Trade:setTqty validation]"
						,"supplied param for Qty expected to be an integer"))
			  }
			  object@tqty <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getEntry'
           ,function(object){standardGeneric("getEntry")})
#' @aliases getEntry,Trade,ANY-method
setMethod('getEntry', 'Trade' ,function(object){ return(object@entry) })
#' @export
#' @docType methods
 setGeneric(name='setEntry<-'
		  ,function(object,value){standardGeneric('setEntry<-')})
#' @aliases setEntry,Trade,ANY-method
 setReplaceMethod('setEntry'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0 ){
				  stop(paste("[Trade:setEntry validation]"
					,"supplied param for entry price expected to be a positive numeric"))
			  }
			  object@entry <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getStop'
           ,function(object){standardGeneric("getStop")})
#' @aliases getStop,Trade,ANY-method
setMethod('getStop', 'Trade' ,function(object){ return(object@stop) })
#' @export
#' @docType methods
 setGeneric(name='setStop<-'
		  ,function(object,value){standardGeneric('setStop<-')})
#' @aliases setStop,Trade,ANY-method
 setReplaceMethod('setStop'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0 ){
				  stop(paste("[Trade:setStop validation]"
					,"supplied param for stop price expected to be a positive numeric"))
			  }
			  object@stop <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getFtarget'
           ,function(object){standardGeneric("getFtarget")})
#' @aliases getFtarget,Trade,ANY-method
setMethod('getFtarget', 'Trade' ,function(object){ return(object@ftarget) })
#' @export
#' @docType methods
 setGeneric(name='setFtarget<-'
		  ,function(object,value){standardGeneric('setFtarget<-')})
#' @aliases setFtarget,Trade,ANY-method
 setReplaceMethod('setFtarget'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0 ){
				  stop(paste("[Trade:setFtarget validation]"
					,"supplied param for first target price expected to be a positive numeric"))
			  }
			  object@ftarget <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getMtarget'
           ,function(object){standardGeneric("getMtarget")})
#' @aliases getMtarget,Trade,ANY-method
setMethod('getMtarget', 'Trade' ,function(object){ return(object@mtarget) })
#' @export
#' @docType methods
 setGeneric(name='setMtarget<-'
		  ,function(object,value){standardGeneric('setMtarget<-')})
#' @aliases setMtarget,Trade,ANY-method
 setReplaceMethod('setMtarget'
  	              ,'Trade'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0 ){
				  stop(paste("[Trade:setMtarget validation]"
					,"supplied param for main target price expected to be a positive numeric"))
			  }
			  object@mtarget <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getComment',function(object){standardGeneric("getComment")})
#' @aliases getComment,Trade,ANY-method
setMethod('getComment', 'Trade' ,function(object){ return(object@comment) })
#' @export
#' @docType methods
 setGeneric(name='setComment<-' 
 	,function(object,value){standardGeneric('setComment<-')})
#' @aliases setComment,Trade,ANY-method
 setReplaceMethod('setComment','Trade',function(object,value){
		  	  #validation
			  if ( !is.character(value) ){
				  stop(paste("[Trade:setComment validation]"
					,"supplied param for trade comment expected to be a char string"))
			  }
			  object@comment <- value
			  return(object)
		  })

#' @export
#' @docType methods
 setGeneric(name='setTorders<-' 
 	,function(object,value){standardGeneric('setTorders<-')})
#' @aliases setTorders,Trade,ANY-method
 setReplaceMethod('setTorders','Trade',function(object,value){
		  	  #validation
		  	  for(j in 1:length(value)){
			     if ( !is( value[[j]] ,'Order') ){
				  stop(paste("[Trade:setTorders validation]"
					,"supplied list of objects contain instance(s) other than Order"))
			     }
			  }  
			  object@torders <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getTorders'
           ,function(object){standardGeneric("getTorders")})
#' @aliases getTorders,Trade,ANY-method
setMethod('getTorders', 'Trade' 
	,function(object){
		if (length(object@torders)==0) { return(NA)}
		else { return (object@torders) }
	})



#' @export
#' @docType method
setGeneric( name='getTradeOrder'
           ,function(object, oid){gtandardGeneric("getTradeOrder")})
#' @aliases getTradeOrder,Trade,ANY-method
setMethod('getTradeOrder', 'Trade' 
	,function(object,oid){
      if( !is.numeric(oid)) {
        stop(paste("[Trade:getTradeOrder validation]"
          ,"supplied param oid expected to be numeric"))
      }		
      aoids <- getOrdersattachedIds(object)
      coidx <- match(oid, aoids)
      if(is.na(coidx)){
    	return(NA)
      }else{
    	return( object@torders[[ coidx ]] )
      } 
   })

#' @export
#' @docType methods
setGeneric(name='setTradeOrder<-',function(object, value)
  {standardGeneric('setTradeOrder<-')})
#' @aliases setTradeOrder,Trade,ANY-method
setReplaceMethod('setTradeOrder','Trade'
   ,function(object, value){
      ##object validation
      if( !is(value, 'Order')){
        stop(paste("[Trade:setTradeOrder validation]"
          ,"supplied param for order is not of type Order"))
      }
      ##biz constraint validation
      #4 cases to consider
      tid <- getTid(object)
      trid <- getTrid(value)
      if( trid != tid){
        stop(paste("[Trade:setTradeOrder validation]"
          ,"trid from supplyed order does not match the id of this trade object"
          ,"Skipping.."))
      }         
      aoids <- getOrdersattachedIds(object)
      coid <- getId(value)
      coidx <- match(coid, aoids)
      if(!is.na(coidx) && coid != 0){ #order id found in attached orders => update order in list
                                      #if order is new with id = 0, append such order
        object@torders[[ coidx]] <- value
      }else{             #order id not found in already attached orders => append order 
        object@torders[[ length(object@torders)  + 1 ]] <- value
      }  
      return(object)
})

#' remove order from trade list of orders
#'
#' @export
#' @docType methods
setGeneric(name='removeTradeOrder',function(.Object, order) {standardGeneric('removeTradeOrder')})
#' @aliases removeTradeOrder,Trade,ANY-method
setMethod('removeTradeOrder','Trade'
   ,function(.Object,order){
      nameObject <- deparse(substitute(.Object))
      ##object validation
      if( !is(order, 'Order')){
        stop(paste("[Trade:removeTradeOrder validation]"
          ,"supplied param for order is not of type Order"))
      }
      aoids <- getOrdersattachedIds(.Object)
      coid <- getId(order)
      coidx <- match(coid, aoids)
      if(!is.na(coidx) ){ #order id found in attached orders => remove order in list
                                      #if order is new with id = 0, append such order
        .Object@torders[[ coidx]] <- NULL
      }     
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' update trade exposure status between c  y,  n
#'
#' @export
#' @docType methods
setGeneric(name='updTradeStatus',function(.Object,verbose=TRUE) {standardGeneric('updTradeStatus')})
#' @aliases updTradeStatus,Trade,ANY-method
setMethod('updTradeStatus','Trade'
   ,function(.Object,verbose=TRUE){
      nameObject <- deparse(substitute(.Object))
      tid <- getTid(.Object)
      ostatus <- getTstatus(.Object)
      tstatus <- "n"
       if(!hasOrdersattached(.Object)){ 
          #case 1. 
          tstatus <- "n"
       }else{         
         #case 2.1  all orders cancelled
         aostatuses <- unlist(lapply( getTorders(.Object), function(x){getStatus(x)}))
         if ( length(unique(aostatuses))==1 && aostatuses[1] == 'c'){
              setTstatus(.Object) <- 'c' 
         # case 2.2     
         }else{
         pos <- getTradePosition(.Object)
          if(pos == 0) {  tstatus <- 'n'  }
          else         {  tstatus <- 'y'  }
         }   
       }
       if( tstatus != ostatus){
          setTstatus(.Object) <- tstatus
          msg <- paste("trade:", tid, "status changed from",ostatus, "to",tstatus)
          if(verbose){ cat(msg,"\n")}
       }
      #browser()      
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' update order size to match open position
#'
#' @export
#' @docType methods
setGeneric(name='updOrdersizeOpenposition',function(.Object, chgdate=Sys.time(), verbose=TRUE) {standardGeneric('updOrdersizeOpenposition')})
#' @aliases updOrdersizeOpenposition,Trade,ANY-method
setMethod('updOrdersizeOpenposition','Trade'
   ,function(.Object, chgdate=Sys.time(), verbose=TRUE){
      nameObject <- deparse(substitute(.Object))

      tid <- getTid(.Object)
      tdir <- sign(getTqty(.Object))
      opos <- getTradePosition(.Object)
      if(opos == 0) { return(invisible())}

      ostatus <- unlist(lapply(.Object@torders,function(x){getStatus(x)}))
      woidx <- which(ostatus =='w')
      if(length(woidx) == 0) { return(invisible())}
      for(j in 1:length(woidx)) {
       cword <-  .Object@torders[[ woidx[j] ]]
       oqty <- getQty(cword)
       oid <- getId(cword)
       #browser()
        #conditional update workigng order qty
        if (  sign(oqty) != tdir 
           && oqty != opos ){
            setQty(cword) <- -1 * opos
            setChgDate(cword) <- chgdate 
            .Object@torders[ woidx[j] ] <- cword
            msg <- paste("Order:",oid, "qty changed from", oqty, "to", getQty(cword))
            if(verbose){ cat(msg,"\n") }
        }
      }
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


######## derived properties###########


#' @export
#' @docType method
setGeneric( name='getOrdersattachedIds'
           ,function(object){standardGeneric("getOrdersattachedIds")})
#' @aliases getOrdersattachedIds,Trade,ANY-method
setMethod('getOrdersattachedIds', 'Trade' 
	,function(object){
		attoids <- unlist(lapply(object@torders,function(x){getId(x)}))
		if(is.null(attoids)){ return(NA)}
		else  return( attoids )
	})


#' @export
#' @docType method
setGeneric( name='getFtargetRR'
           ,function(object){standardGeneric("getFtargetRR")})
#' @aliases getFtargetRR,Trade,ANY-method
setMethod('getFtargetRR', 'Trade' ,function(object){ 
	 return( round(( object@ftarget - object@entry) / ( object@entry - object@stop),digits=3 ))
	})

#' @export
#' @docType method
setGeneric( name='getMtargetRR'
           ,function(object){standardGeneric("getMtargetRR")})
#' @aliases getMtargetRR,Trade,ANY-method
setMethod('getMtargetRR', 'Trade' ,function(object){ 
	   return( round( (object@mtarget - object@entry) / ( object@entry - object@stop),digits=3 ))
	})


#' @export
#' @docType method
setGeneric( name='hasOrdersattached'
           ,function(object){standardGeneric("hasOrdersattached")})
#' @aliases hasOrdersattached,Trade,ANY-method
setMethod('hasOrdersattached', 'Trade' 
	,function(object){return( length(object@torders) >0 ) })

##################################
##################################
#' @export
#' @docType method
setGeneric( name='getTradePosition'
           ,function(object){standardGeneric("getTradePosition")})
#' @aliases getTradePosition,Trade,ANY-method
setMethod('getTradePosition', 'Trade' 
  ,function(object){
    tpos <- 0
    if(hasOrdersattached(object)){
      fpos <- unlist(lapply(object@torders
                          ,function(x){
                            if(getStatus(x)=='f') return ( getQty(x))
                            else return(0)
                           }))
      tpos <- sum(fpos)
    }
    return (tpos)
})




#######   deprecated
#' @export
#' @docType method
setGeneric( name='getTradeMtm'
           ,function(object,mmprice){standardGeneric("getTradeMtm")})
#' @aliases getTradeMtm,Trade,ANY-method
setMethod('getTradeMtm', 'Trade' 
  ,function(object,mmprice){
    mtm <- 0
    if(hasOrdersattached(object)){
      omtm <- unlist(lapply(object@torders
                          ,function(x,mm){
                            if(getStatus(x)=='f') return ( getMtm(x,mm))
                            else return(0)
                           },mm=mmprice))
      mtm <- sum(omtm)
    }
    return (mtm)
})


######## goood
#' @export
#' @docType method
setGeneric( name='getTradeMtmvalue'
           ,function(object, mmprice=0, contrmult=1){standardGeneric("getTradeMtmvalue")})
#' @aliases getTradeMtmvalue,Trade,ANY-method
setMethod('getTradeMtmvalue', 'Trade' 
  ,function(object,mmprice=0,contrmult=1){
    tval <- 0
    if(!hasOrdersattached(object)){ return(0) }
    onot <- unlist(lapply(object@torders
                          ,function(x){
                             res <- 0
                             if(getStatus(x)=='f') res <- getNotional(x)
                             return(res)
                           }))
    trlzdvalue <- sum(onot) * contrmult
    tval <- trlzdvalue
    tpos <- getTradePosition(object)
    if(tpos != 0) {
        unrlzdval <- -1 * contrmult * (-1) * sign(tpos) * abs(tpos) * mmprice
        tval <- tval + unrlzdval
    }
    return (tval)
})


#' @export
#' @docType method
setGeneric( name='getTradeUpnl'
           ,function(object, mmprice=0, contrmult=1){standardGeneric("getTradeUpnl")})
#' @aliases getTradeUpnl,Trade,ANY-method
setMethod('getTradeUpnl', 'Trade' 
  ,function(object,mmprice=0,contrmult=1){
    upnl <- 0
    opos <- getTradePosition(object)
    if(opos == 0) {return(0)}
    upnl <- opos * contrmult * ( mmprice - getTradeAvrgPrice(object, "enter") )
    return(upnl)
})


#' @export
#' @docType method
setGeneric( name='getTradeRpnl'
           ,function(object,contrmult=1){standardGeneric("getTradeRpnl")})
#' @aliases getTradeRpnl,Trade,ANY-method
setMethod('getTradeRpnl', 'Trade' 
  ,function(object,contrmult=1){
    rpnl <- 0
    opos <- getTradePosition(object)
    if(opos == 0) {return(getTradeMtmvalue(object, mmprice=0,contrmult=contrmult))}
    ##if open position
    tdir <-  sign(getTqty(object))
    vols <- unlist(lapply(object@torders
                          ,function(x){
                            res <- 0
                            if(getStatus(x)=='f' && sign(getQty(x)) == tdir ) res <-  getQty(x)   
                            return(res)
                           }))
    maxopenqty <- sum(vols)
    if(maxopenqty == 0) { return(0)}
    clsedqty <- maxopenqty - opos
    rpnl <- clsedqty * contrmult * ( getTradeAvrgPrice(object, "exit")  - getTradeAvrgPrice(object, "enter") )
    return(rpnl)
})

#' @export
#' @docType method
setGeneric( name='getTradeLockedCash'
           ,function(object, contrmult=1, marginpct=1){standardGeneric("getTradeLockedCash")})
#' @aliases getTradeLockedCash,Trade,ANY-method
setMethod('getTradeLockedCash', 'Trade' 
  ,function(object,contrmult=1,marginpct=1){
    cash <- 0
    opos <- getTradePosition(object)
    if(opos == 0) {return(0)}
    cash <- abs(opos) * contrmult * marginpct *  getTradeAvrgPrice(object, "enter") 
    return(cash)
})


#' @export
#' @docType method
setGeneric( name='getTradeAvrgPrice'
           ,function(object,crit="enter"){standardGeneric("getTradeAvrgPrice")})
#' @aliases getTradeAvrgPrice,Trade,ANY-method
setMethod('getTradeAvrgPrice', 'Trade' 
  ,function(object,crit="enter"){
    critokvals <-c("enter", "exit")
    if( !(crit %in% critokvals)){
           sokvals <- paste(critokvals,collapse=" ")
           stop(paste("[Trade:getTradeAvrgPrice validation]"
                     ,"supplied param for crit should have 1 of expected values:",sokvals))
     }
    if(crit=="enter"){  tdir <- sign(getTqty(object)) }
    else             {  tdir <-  -1 * sign(getTqty(object))  }
    if(!hasOrdersattached(object)){ return(0) }
     onot <- unlist(lapply(object@torders
                          ,function(x){
                            if(getStatus(x)=='f' && sign(getQty(x)) == tdir ) return ( getQty(x) * getFillprice(x) )
                            else                  return(0)
                           }))
     ovols <- unlist(lapply(object@torders
                          ,function(x){
                            if(getStatus(x)=='f' && sign(getQty(x)) == tdir ) return ( getQty(x) )  
                            else                  return(0)
                           }))

     if (sum(ovols) != 0 ) {
         avgprice <- sum(onot) / sum(ovols)
     }else{
         avgprice <- 0
     }    
     #browser()   
    return (avgprice )
})


#'verify if trade trade stop or target is set as working order(s)
#'  
#' @param crit=-1  crit=-1 for stop orders information,  crit=1  for profit target oders info
#'
#' return values  yes, no, part
#' @export
#' @docType method
setGeneric( name='isTradeProtected'
           ,function(object,crit=-1){standardGeneric("isTradeProtected")})
#' @aliases isTradeProtected,Trade,ANY-method
setMethod('isTradeProtected', 'Trade' 
  ,function(object,crit=-1){
    info <- "no"
    tpos <- getTradePosition(object)
     if(tpos != 0){
      #1. find aveage price of trade entry
      fprs <- unlist(lapply(object@torders
                          ,function(x){
                            if(getStatus(x)=='f') return ( getQty(x) * getRegprice(x))
                            else return(0)
                           }))
      mregprice <- sum(fprs) / tpos

      #2. find potential limit orders for a stop (crit=-1) or target (crit=1)
      critlims <- unlist(lapply(object@torders
                          ,function(x, cr, tdir,tprice){
                            stopPos <- 0
                            if(getStatus(x)=='w'){
                              #case for longs
                              if(tdir ==1 && sign(cr)*(getRegprice(x) - tprice ) > 0  ){
                                stopPos <- getQty(x)
                              }
                              #case for shorts
                              if(tdir ==-1 && sign(cr)*(getRegprice(x) -tprice) < 0 ){
                                stopPos <- getQty(x)
                              }
                            }
                            return(stopPos)
                           },cr=crit, tdir=sign(tpos),tprice=mregprice))
      stopPos <- sum(critlims)
      #browser()
      if ( stopPos == -1 * tpos) { info <- "yes"}
      else if ( stopPos != 0) { info <- "part"}
      else { info <- "no"}
  }
  return (info)
})

#'verify if trade with no position has orphaned working orders
#'  
#'
#' return boolean or char values  yes, no
#' @export
#' @docType method
setGeneric( name='findTradeOrphanedOrders'
           ,function(object){standardGeneric("findTradeOrphanedOrders")})
#' @aliases findTradeOrphanedOrders,Trade,ANY-method
setMethod('findTradeOrphanedOrders', 'Trade' 
  ,function(object){

    oords <- c()

    tpos <- getTradePosition(object)
    if(tpos == 0){
      # 1. check we have at least two filled orders
      trCompleted <- FALSE
      ostatus <- unlist(lapply(object@torders,function(x){getStatus(x)}))
      ordids  <- unlist(lapply(object@torders,function(x){getId(x)}))
      foidx <- which(ostatus =='f')
      # 2. check if we have working orders
      woidx <- which(ostatus =='w')
      #browser()
      #more then 1 filled order and more then 0 working orders with zero trade position
      if( length(foidx) > 1 &&  length(woidx)>0) { 
        oords <- ordids[woidx]
      }
    }
   return(oords)
})

#' @export
#' @docType method
setGeneric( name='strTradeOrphanedOrders'
           ,function(object){standardGeneric("strTradeOrphanedOrders")})
#' @aliases strTradeOrphanedOrders,Trade,ANY-method
setMethod('strTradeOrphanedOrders', 'Trade' 
  ,function(object){
    orphaned <- findTradeOrphanedOrders(object)
    msg <- ""
    if (is.null(orphaned)) {
      msg <- "no"
    }
    else {
      msg <- paste(orphaned, collapse=" ")
    }
    return(msg)
})

#' @export
#' @docType method
setGeneric( name='getTradeWorkingOrders'
           ,function(object){standardGeneric("getTradeWorkingOrders")})
#' @aliases getTradeWorkingOrders,Trade,ANY-method
setMethod('getTradeWorkingOrders', 'Trade' 
  ,function(object){
    wlist <- list()
    if(hasOrdersattached(object)){
      ostatus <- unlist(lapply(object@torders,function(x){getStatus(x)}))
      woidx <- which(ostatus =='w')
      if(length(woidx)>0) { wlist <- object@torders[woidx] }
    }
    return (wlist)
  })

#' @export
#' @docType method
setGeneric( name='getTradeOrdersStatus'
           ,function(object){standardGeneric("getTradeOrdersStatus")})
#' @aliases getTradeOrdersStatus,Trade,ANY-method
setMethod('getTradeOrdersStatus', 'Trade' 
  ,function(object){
    ostatus <- NA
    if(hasOrdersattached(object)){
      ostatus <- unlist(lapply(object@torders,function(x){getStatus(x)}))
    }
    return (ostatus)
  })




##################################
##################################






#' used to attach existing recorded orders to an existing recorded trade
#
#' @export
#' @docType methods
setGeneric(name='attachOrdersFromrecset',function(.Object, rset)
	{standardGeneric('attachOrdersFromrecset')})
#' @aliases attachOrdersFromrecset,Trade,ANY-method
setMethod('attachOrdersFromrecset','Trade'
   ,function(.Object, rset){
      ##object validation
      if( !is.data.frame(rset)){
        stop(paste("[Trade:attachOrdersFromrecset validation]"
          ,"supplied param of recordset is not of type data frame"))
      }
      ##biz constraint validation
      tid <- getTid(.Object)
      trids <- rset$trid
      if(  (length(unique(trids))>1) || unique(trids) != tid ){
        stop(paste("[Trade:attachOrdersFromrecset validation]"
          ,"a mismatch between this Trade tid and trade ids in the supplied order recordset"))
      }         
      nameObject <- deparse(substitute(.Object))
      for(j in 1:nrow(rset)){
      	o <- crOrderFromrec( as.list(rset[j,]))
      	#attachOrder(.Object, o)
        setTradeOrder(.Object) <- o
      }
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})




#' used for initial fixing trid in orders when a new trade is created
#
#' @export
#' @docType methods
setGeneric(name='setOrdersTrid',function(.Object) {standardGeneric('setOrdersTrid')})
#' @aliases setOrdersTrid,Trade,ANY-method
setMethod('setOrdersTrid','Trade'
   ,function(.Object){
   	  if(!hasOrdersattached(.Object)){
   	  	 stop(paste("[Trade:setOrdersTrid validation]"
          ,"no attached orders. Skipping.."))
   	  }
      tid <- getTid(.Object)
      if( tid == 0){
   	  	 stop(paste("[Trade:setOrdersTrid validation]"
          ,"this trade  id is 0. Can not set orders trid to 0"))      	
      }
   	  nameObject <- deparse(substitute(.Object))
      for( j in 1:length(.Object@torders)){
          corder <- .Object@torders[[j]]
          setTrid(corder) <- tid
          .Object@torders[[j]] <- corder
      }
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})



#### methods to init and append trade record to a data frame in Orderbook

#' @export
#' @docType methods
setGeneric(name='initTradedataframe'
          ,function(object)
                   {standardGeneric('initTradedataframe')})
#' @aliases initTradedataframe,Order,ANY-method
setMethod('initTradedataframe'
         ,'Trade'
   ,function(object){
            tdf <- data.frame( c(getTid(object))
                                ,c(getStratid(object))  
                                ,c(getTaccount(object))
                                ,c(getTticker(object)) 
                                ,c(getTincDate(object,chr=TRUE))
                                ,c(getTstatus(object)) 
                                ,c(getTqty(object))
                                ,c(getEntry(object))
                                ,c(getStop(object))
                                ,c(getFtarget(object))
                                ,c(getMtarget(object))
                                ,c(getComment(object))
                                  ,stringsAsFactors=FALSE )
   	        
            names(tdf) <- c("tid", "strat",  "acc", "tkr", "incdate", "status"
            	           ,"tqty", "entry", "stop", "ftarget", "mtarget", "comment")
      
      return(tdf)
})


#' @export
#' @docType methods
setGeneric(name='toTraderecAslist'
          ,function(object)
                  {standardGeneric('toTraderecAslist')})
#' @aliases toTraderecAslist,Trade,ANY-method
setMethod('toTraderecAslist'
         ,'Trade'
   ,function(object){
                   orec <- list( getTid(object)  ,getStratid(object) 
                                ,getTaccount(object) ,getTticker(object)
                                ,getTincDate(object,chr=TRUE)
                                ,getTstatus(object)
                                ,getTqty(object)  ,getEntry(object) ,getStop(object)
                                ,getFtarget(object)  ,getMtarget(object)
                                ,getComment(object)
                               )
      return(orec)
})

#' @export
#' @docType methods
setGeneric(name='printTrade'
          ,function(object, mmprice=0, contrmult=1, co=FALSE)
                  {standardGeneric('printTrade')})
#' print Trade details and orders which it contains
#'
#' @param mmprice=0 (numeric) = mark to market price  to trade pnl valuation
#  @param co = FALSE (boolean) if TRUE print cancelled orders 
#' @aliases printTrade,Trade,ANY-method
setMethod('printTrade'
         ,'Trade'
   ,function(object, mmprice=0, contrmult=1, co=FALSE){
    
     fnames1 <- c("strat", "acc", "tkr", "incdate", "status", "qty", "entry", "stop", "ftarg", "mtarg")
     rnames1 <- c(paste("tid:", getTid(object)) )
     tdf1 <- data.frame(         getStratid(object) 
                                ,getTaccount(object) ,getTticker(object)
                                ,getTincDate(object,chr=TRUE)
                                ,getTstatus(object)
                                ,getTqty(object)  ,getEntry(object) ,getStop(object)
                                ,getFtarget(object)  ,getMtarget(object)
  
                         , stringsAsFactors=FALSE       
                        )
     colnames(tdf1) <- fnames1
     rownames(tdf1) <- rnames1
     print(tdf1)

     fnames2 <-c("ftrr", "mtrr", "pos", "stopSet", "TargetSet", "Upnl", "Rpnl", "orphaned")
     rnames2 <- c("props:")
     tdf2 <- data.frame(  getFtargetRR(object)
                         ,getMtargetRR(object)
                         ,getTradePosition(object)
                         ,isTradeProtected(object,crit=-1)
                         ,isTradeProtected(object,crit=1)
                         ,getTradeUpnl(object,mmprice=mmprice, contrmult=contrmult)
                         ,getTradeRpnl(object, contrmult=contrmult)
                         ,strTradeOrphanedOrders(object)
                        ,stringsAsFactors=FALSE       
                       )
     colnames(tdf2) <-fnames2
     rownames(tdf2) <- rnames2
     cat(".....................................................................\n")
     print(tdf2)
     cat(paste("comment:",getComment(object),"\n")) 
     cat(".....................................................................\n")
     ###printing trade orders
     if( !hasOrdersattached(object)) { return() }
     torders <- getTorders(object)
     tstatus <- getTstatus(object)
     #browser() 
     if( tstatus != 'c' && !co){ ##filter out cancelled orders
       ostatus <- unlist(lapply(torders, function(x){getStatus(x)}))
       
       oidx <- which(ostatus != 'c')
       torders <- torders[oidx] 
     }
     otz <- base::format(torders[[1]]@inceptDate,format="%Z") # tzone of order

     rnames3 <- unlist(lapply(torders, function(x) { paste("order:", getId(x))}))
     fnames3 <- c("tid", "tkr", "incdate", "type", "ostatus","chdate","qty", "rprice", "fprice", "mtm")

     tdf3 <- data.frame( unlist(lapply(torders,function(x){getTrid(x)}))   
                         ,unlist(lapply(torders,function(x){getTicker(x)}))
                        #,as.POSIXct(unlist(lapply(torders,function(x,ch){getIncDate(x,ch)},ch=FALSE)),origin='1970-01-01',tz=otz)
                         ,unlist(lapply(torders,function(x,ch){getIncDate(x,ch)},ch=TRUE))
                         ,unlist(lapply(torders,function(x){getType(x)}))
                         ,unlist(lapply(torders,function(x){getStatus(x)}))
                        #,as.POSIXct(unlist(lapply(torders,function(x,ch){getChgDate(x,ch)},ch=FALSE)),origin='1970-01-01',tz=otz)
                         ,unlist(lapply(torders,function(x,ch){getChgDate(x,ch)},ch=TRUE))
                         ,unlist(lapply(torders,function(x){getQty(x)}))
                         ,unlist(lapply(torders,function(x){getRegprice(x)}))
                         ,unlist(lapply(torders,function(x){getFillprice(x)}))
                         ,unlist(lapply(torders,function(x,mm){getMtm(x,mm)},mm=mmprice))
                       ,stringsAsFactors=FALSE       
                       )
     colnames(tdf3) <-fnames3
     if (length(rnames3) == length(unique(rnames3))) { rownames(tdf3) <- rnames3 } #avoid settin duplicate rownames
    
     print(tdf3)
     cat("---------------------------------------------------------------------\n")
})

setMethod ('show'
           ,'Trade'
           , function(object){
              incDate <- getTincDate(object)
              tzval <- attributes(incDate)$tzone
              if(is.null(tzval)) { tzval <- ""}
              dfmt <- "%Y-%m-%d %H:%M:%S %Z"
              nf <-"%.4f"
           	  sb <- ""
              sb <-paste(sb,"** Trade ** ")
              sb <- paste(sb,"tid:", getTid(object)
                         ,"strat:", getStratid(object)
                         ,"acc:", getTaccount(object)
                         ,"instr:", getTticker(object)
                         ,"incdate:", strftime (incDate,format=dfmt, tz=tzval)
                         ,"status:", getTstatus(object)
                         ,"tqty:", sprintf(nf, getTqty(object))
                         ,"entry:", sprintf(nf, getEntry(object))
                         ,"stop:", sprintf(nf, getStop(object))
                         ,"risk:", sprintf(nf, -1*abs(getEntry(object) - getStop(object)) )
                         ,"ftrg:", sprintf(nf, getFtarget(object))
                         ,"mtrg:", sprintf(nf, getMtarget(object))
                         ,"frr:", sprintf(nf, getFtargetRR(object))
                         ,"mrr:", sprintf(nf, getMtargetRR(object))
                         )
              sb <- paste(sb, "\n")
              ##print attached orders
              if(hasOrdersattached(object)){
                sb<-paste(sb, "attached orders:\n")
                cat(sb)
              	for( j in 1:length(object@torders)){
              		cat("  ") 
              		show(object@torders[[j]])
              	}            
              }else{
              	cat(sb)
              }              
           })

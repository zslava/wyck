# TODO: Add comment
#
# Author: zimine
###############################################################################

#'  list of orders on the watch list
#'
#' S4 object which holds an Object for a working or filled order
#' 
#' @rdname Order-class
#' @name Order
#' @exportClass Order
setClass(Class="Order"
         ,representation( id="numeric"
         	             ,trid="numeric"
         	             ,accid="numeric"
         	             ,itkr="character"
                         ,inceptDate="POSIXct"
         	             ,type="character"  # m market, l - limit
         	             ,status="character"  # w working, c cancelled, f filled
                         ,changeDate="POSIXct"
                         ,qty="numeric"
                         ,rprice="numeric"
                         ,fprice="numeric"
          )
         ,prototype( id=0
         	        ,trid=0
         	        ,accid=1
         	        ,itkr='undef'
         	        ,inceptDate=as.POSIXct(NA)
         	        ,type='l'
         	        ,status='w'
         	        ,changeDate=as.POSIXct(NA)
         	        ,qty=0
         	        ,rprice=0
         	        ,fprice=0
          )
        ) 

##user friendly classic function for constructors


#' @export
mkLimitOrder <- function(tradeid, acc, tkr, incDate, qty, price){

    ##input validation
    if( !is(incDate, 'POSIXct') ){
 		 stop(paste("[Order:mkLimitOrder validation]"
								,"incDate param expected to be of POSIXct type"))
	}   	


	o <- new("Order",  trid=tradeid, accid=acc, itkr=tkr
		            ,inceptDate=incDate, qty=qty, rprice=price) 
	o@type <- "l" ## order type is limit
	o@status <- "w" # order status is working
	##initialize changeDate to inceptionDate
	o@changeDate <- o@inceptDate
	o@fprice <- 0
	return(o)
}

#' @export
mkMarketOrder <- function(tradeid, acc, tkr, incDate, qty, price, slippg=0){

    ##input validation
    if( !is(incDate, 'POSIXct') ){
 		 stop(paste("[Order:mkMarketOrder validation]"
								,"incDate param expected to be of POSIXct type"))
	}   	
    

	o <- new("Order", trid=tradeid, accid=acc, itkr=tkr
		            ,inceptDate=incDate, qty=qty, rprice=price)
    
	o@type <- "m" ## order type is market
	o@status <- "f" # order status is filled
	##set changeDate to inceptionDate
	o@changeDate <- o@inceptDate
	## set (different) filled price if it is supplied
	trdir <- sign( o@qty )
    fprice <- o@rprice + trdir * slippg
    o@fprice <- fprice 

	return(o)
}

#' @export
crOrderFromrec<- function(olist){

    ##input validation
    if( !is.list(olist)){
 		 stop(paste("[Order:mkOrderFromrowlist validation]"
								,"olist param expected to be a list with all order properties"))
	}
	sfmt<- "%Y-%m-%d %H:%M:%S"  	
	o <- new("Order", id=olist$id, trid=olist$trid, accid=olist$acc, itkr=olist$tkr
		            ,inceptDate=as.POSIXct( olist$incdate, format=sfmt)
		            ,type=olist$type, status=olist$status
		            ,changeDate=as.POSIXct( olist$chgdate, format=sfmt)
		            , qty=olist$qty, rprice=olist$rprice, fprice=olist$fprice )

    return(o)
}



### biz methods

#' @export
#' @docType method
setGeneric( name='getId'
           ,function(object){standardGeneric("getId")})
#' @aliases getId,Order,ANY-method
setMethod('getId', 'Order' ,function(object){ return(object@id) })
#' @export
#' @docType methods
  setGeneric(name='setId<-'
		  ,function(object,value){standardGeneric('setId<-')})
#' @aliases setId,Order,ANY-method
  setReplaceMethod('setId'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0  ){
				  stop(paste("[Order:setId validation]"
						,"supplied param for id expected to be a positive integer"))
			  }
			  object@id <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getTrid'
           ,function(object){standardGeneric("getTrid")})
#' @aliases getTrid,Order,ANY-method
setMethod('getTrid', 'Order' ,function(object){ return(object@trid) })
#' @export
#' @docType methods
  setGeneric(name='setTrid<-'
		  ,function(object,value){standardGeneric('setTrid<-')})
#' @aliases setTrid,Order,ANY-method
  setReplaceMethod('setTrid'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0  ){
				  stop(paste("[Order:setId validation]"
						,"supplied param for id expected to be a positive integer"))
			  }
			  #biz constraint: cannot change trid if it was already set
			  if(object@trid > 0){
				  stop(paste("[Order:setId validation]"
						,"cannot reset an already set Order trid", object@trid
						, "to", value ))			  	
			  }
			  object@trid <- value
			  return(object)
		  })


#' @export
#' @docType method
setGeneric( name='getType'
           ,function(object){standardGeneric("getType")})
#' @aliases getType,Order,ANY-method
setMethod('getType', 'Order' ,function(object){ return(object@type) })
#' @export
#' @docType methods
  setGeneric(name='setType<-'
		  ,function(object,value){standardGeneric('setType<-')})
#' @aliases setType,Order,ANY-method
  setReplaceMethod('setType'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
		  	  oktypes <- c("l", "m")
			  if ( !(value %in% oktypes) ){
				  stop(paste("[Order:setType validation]"
						,"supplied param for Type does not match valid types m or l"))
			  }
			  object@type <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getStatus'
           ,function(object){standardGeneric("getStatus")})
#' @aliases getStatus,Order,ANY-method
setMethod('getStatus', 'Order' ,function(object){ return(object@status) })
#' @export
#' @docType methods
  setGeneric(name='setStatus<-'
		  ,function(object,value){standardGeneric('setStatus<-')})
#' @aliases setStatus,Order,ANY-method
  setReplaceMethod('setStatus'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
		  	  okStatus <- c("w", "c", "f")
			  if ( !(value %in% okStatus) ){
				  stop(paste("[Order:setStatus validation]"
						,"supplied param for status does not match valid Status w, c or f"))
			  }
			  object@status <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getTicker'
           ,function(object){standardGeneric("getTicker")})
#' @aliases getTicker,Order,ANY-method
setMethod('getTicker', 'Order' ,function(object){ return(object@itkr) })
###no method for setTicker<-
#
#' @export
#' @docType method
setGeneric( name='getOaccount'
           ,function(object){standardGeneric("getOaccount")})
#' @aliases getOaccount,Order,ANY-method
setMethod('getOaccount', 'Order' ,function(object){ return(object@accid) })
###no method for setAccount<-

#' @export
#' @docType method
setGeneric( name='getIncDate'
           ,function(object,chr=FALSE){standardGeneric("getIncDate")})
#' @aliases getIncDate,Order,ANY-method
setMethod('getIncDate', 'Order' ,function(object,chr=FALSE){
   out <- object@inceptDate 
   if(chr) out <- base::format(object@inceptDate, format="%Y-%m-%d %H:%M:%S") 
   return(out)
 })
###no method for setIncDate<-

#' @export
#' @docType method
setGeneric( name='getChgDate'
           ,function(object,chr=FALSE){standardGeneric("getChgDate")})
#' @aliases getChgDate,Order,ANY-method
setMethod('getChgDate', 'Order' ,function(object,chr=FALSE){
  out <- object@changeDate
   if(chr){ out <- base::format(object@changeDate, format="%Y-%m-%d %H:%M:%S") }
   return(out)
})
#' @export
#' @docType methods
 setGeneric(name='setChgDate<-'
		  ,function(object,value){standardGeneric('setChgDate<-')})
#' @aliases setChgDate,Order,ANY-method
 setReplaceMethod('setChgDate'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
		  	  dtvalue <- as.POSIXct(value)
			  if ( !is.numeric(unclass(dtvalue)) ){
				  stop(paste("[Order:setChgDate validation]"
						,"supplied param for changeDate can not be converted to POSIXct type"))
			  }
			  object@changeDate <- dtvalue
			  return(object)
		  })


#' @export
#' @docType method
setGeneric( name='getQty'
           ,function(object){standardGeneric("getQty")})
#' @aliases getQty,Order,ANY-method
setMethod('getQty', 'Order' ,function(object){ return(object@qty) })
#' @export
#' @docType methods
 setGeneric(name='setQty<-'
		  ,function(object,value){standardGeneric('setQty<-')})
#' @aliases setQty,Order,ANY-method
 setReplaceMethod('setQty'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) ){
				  stop(paste("[Order:setQty validation]"
						,"supplied param for Qty expected to be an integer"))
			  }
			  object@qty <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getRegprice'
           ,function(object){standardGeneric("getRegprice")})
#' @aliases getRegprice,Order,ANY-method
setMethod('getRegprice', 'Order' ,function(object){ return(object@rprice) })
#' @export
#' @docType methods
 setGeneric(name='setRegprice<-'
		  ,function(object,value){standardGeneric('setRegprice<-')})
#' @aliases setRegprice,Order,ANY-method
 setReplaceMethod('setRegprice'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0 ){
				  stop(paste("[Order:setRegprice validation]"
					,"supplied param for registered price expected to be a positive numeric"))
			  }
			  ##biz logic constraint
			  if( object@status != "w" ) {
				  stop(paste("[Order:setRegprice validation]"
					,"cannot change registered price for a non-working order"))

			  }
			  object@rprice <- value
			  return(object)
		  })

#' @export
#' @docType method
setGeneric( name='getFillprice'
           ,function(object){standardGeneric("getFillprice")})
#' @aliases getFillprice,Order,ANY-method
setMethod('getFillprice', 'Order' ,function(object){ return(object@fprice) })
#' @export
#' @docType methods
 setGeneric(name='setFillprice<-'
		  ,function(object,value){standardGeneric('setFillprice<-')})
#' @aliases setFillprice,Order,ANY-method
 setReplaceMethod('setFillprice'
  	              ,'Order'
		  ,function(object,value){
		  	  #validation
			  if ( !is.numeric(value) || value < 0 ){
				  stop(paste("[Order:setFillprice validation]"
					,"supplied param for Fillprice expected to be a positive numeric"))
			  }
			  ##biz logic constraint
			  if( object@status == "c" ) {
				  stop(paste("[Order:setFillprice validation]"
					,"cannot change filled price for a non-working order"))

			  }
			  object@fprice <- value
			  return(object)
		  })

#' @docType methods
setGeneric(name='getMtm'
          ,function(object, mmprice=0)
                   {standardGeneric('getMtm')})
#' @aliases getMtm,Order,ANY-method
setMethod('getMtm'
         ,'Order'
   ,function(object, mmprice=0){

    mtmval <- 0
    qty <- getQty(object)
    fprice <- getFillprice(object)
    if(fprice != 0 ){
    	mtmval <- qty * (mmprice - fprice)
    }
    return (mtmval)
})


#' @docType methods
setGeneric(name='getNotional'
          ,function(object)
                   {standardGeneric('getNotional')})
#' @aliases getNotional,Order,ANY-method
setMethod('getNotional'
         ,'Order'
   ,function(object){

    notionval <- 0
    status <- getStatus(object)
    if ( status != "f") { return(0)}
    qty <- getQty(object)
    fprice <- getFillprice(object)
    if(fprice != 0 ){
    	notionval <- -1 * qty * fprice 
    }
    return (notionval)
})



#' @export
#' @docType methods
setGeneric(name='cancelOrder'
          ,function(.Object, chdate)
                   {standardGeneric('cancelOrder')})
#' @aliases cancelOrder,Order,ANY-method
setMethod('cancelOrder'
         ,'Order'
   ,function(.Object, chdate){
      ##object validation
		dtvalue <- as.POSIXct(chdate)
		if ( !is.numeric(unclass(dtvalue)) ){		
		  stop(paste("[Order:cancelOrder validation]"
					,"supplied param for changeDate can not be converted to POSIXct type"))
		}      

      nameObject <- deparse(substitute(.Object))
      setChgDate(.Object)<-dtvalue
      setStatus(.Object)<-'c'
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' @export
#' @docType methods
setGeneric(name='fillOrder'
          ,function(.Object, chdate, fillprice)
                   {standardGeneric('fillOrder')})
#' @aliases fillOrder,Order,ANY-method
setMethod('fillOrder'
         ,'Order'
   ,function(.Object, chdate, fillprice){
      ##object validation
		dtvalue <- as.POSIXct(chdate)
		if ( !is.numeric(unclass(dtvalue)) ){		
		  stop(paste("[Order:fillOrder validation]"
					,"supplied param for changeDate can not be converted to POSIXct type"))
		}      
		if ( !is.numeric(fillprice) ){		
		  stop(paste("[Order:fillOrder validation]"
					,"supplied param for fillprice expected to be a numeric type"))
		}      
      nameObject <- deparse(substitute(.Object))
      setChgDate(.Object)<-dtvalue
      setStatus(.Object)<-'f'
      setFillprice(.Object)<-fillprice
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' critical method to check on order fill between its inception date and cdate
#' covers two cases, 1. crossing the bar  2. clean gap
#'
#' @export
#' @docType methods
setGeneric(name='checkFillOrder'
          ,function(.Object, data, cdate, slippage=0, dfreq=0,verbose=TRUE)
                   {standardGeneric('checkFillOrder')})
#' @aliases checkFillOrder,Order,ANY-method
setMethod('checkFillOrder'
         ,'Order'
   ,function(.Object, data, cdate, slippage=0, dfreq=0,verbose=TRUE){
      ##object validation
        if(  getStatus(.Object) != 'w'){
		  stop(paste("[Order:checkFillOrder validation]"
					,"method applicable only to working worders"))
        }
		if ( !is.xts(data) ){		
		  stop(paste("[Order:checkFillOrder validation]"
					,"supplied data param expected to be an OHLC xts"))
		}
		if(!is.numeric(unclass(as.POSIXct(cdate)))){
		  stop(paste("[Order:checkFillOrder validation]"
					,"supplied cdate param expected to be POSIXct string"))

		}      
		if ( !is.numeric(slippage) ){		
		  stop(paste("[Order:checkFillOrder validation]"
					,"supplied param for slippage expected to be a numeric type"))
		}

		ocdt <- last(Cl(data[paste("::",getChgDate(.Object,chr=TRUE),sep="")]))
        ocdtcl <-  as.numeric(ocdt)
        fmt<-"%Y-%m-%d %H:%M:%S"
        sdata<-data[paste(format(index(ocdt), format=fmt),"::", sep="")]

		odir <- sign(getQty(.Object))
		flvl <- getRegprice(.Object)
        nameObject <- deparse(substitute(.Object))
		## two cases: cross, or gap (up or down)
		hitDatetime <- cdate
		fprice <- flvl
		vmsg <- ""

		#case 1 cross
		xcrit <- Hi(sdata) > flvl & Lo(sdata) < flvl
		indexTZ(xcrit) <- indexTZ(sdata)  # Hi(), Lo() loose tz info from xtsdata !! 
		#cross
		xHit <- match(TRUE, xcrit) # first occurence (what we need to get

        # #case 2  gap up or down. (for a moment do not consider it)
        gcrit <-  (odir ==1 & flvl < Lo(sdata) & flvl > ocdtcl)   |
                  (odir ==1 & flvl > Hi(sdata) & flvl < ocdtcl)   | 
        		  (odir == -1 & flvl > Hi(sdata) & flvl < ocdtcl) |
        		  (odir == -1 & flvl < Lo(sdata) & flvl > ocdtcl)
		indexTZ(gcrit) <- indexTZ(sdata)
		gHit <- match(TRUE, gcrit)


        ##internal function used immediately below
        filldetail<-function(data, indx, dfreq, direction, crossprice, slippage,type){
        	if (type=='cross'){
        		 fprice=crossprice+direction*slippage
        	}
        	if (type =='gap'){
     			if(direction==1){ fprice <- as.numeric(Lo(data[indx]))+direction*slippage }
 				else 			{ fprice <- as.numeric(Hi(data[indx]))+direction*slippage }     			   		
        	}
        	hitDate <- chkDtModEodelta(data,posixDate=index(data)[indx],dfreq=dfreq)  

        	return(list(price=fprice, date=hitDate))
        }

        hasAfill<-FALSE
        #case  clean cross
		if(!is.na(xHit) && is.na(gHit)){
			hasAfill <- TRUE
			fd <- filldetail(data=sdata,indx=xHit,dfreq=dfreq,direction=odir
							,crossprice=flvl,slippage=slippage,type="cross"  )  
		}
        #case  clean gap
		if(is.na(xHit) && !is.na(gHit)){
			hasAfill <- TRUE
			fd <- filldetail(data=sdata,indx=gHit,dfreq=dfreq,direction=odir
							,crossprice=flvl,slippage=slippage,type="gap"  )  
		}
		#case present cross  AND gap
		if(!is.na(xHit) && !is.na(gHit)){
			hasAfill<-TRUE
			idx <- min(xHit, gHit)
			if (xHit < gHit) {
			 	fd <- filldetail(data=sdata,indx=idx,dfreq=dfreq,direction=odir
												,crossprice=flvl,slippage=slippage,type="cross"  )  
			}else{
			 	fd <- filldetail(data=sdata,indx=idx,dfreq=dfreq,direction=odir
												,crossprice=flvl,slippage=slippage,type="gap"  )  
			}
		}
        #browser()
        if(hasAfill ){
        	fillOrder(.Object,  fd[['date']], fillprice=fd[['price']] )

            vmsg <- paste("order", getId(.Object), "is filled on", fd[['date']]
            	         ,getQty(.Object),"@", fd[['price']]) 
        	if(verbose) { print(vmsg) }
        }
        #### care to update Order record in Orderbook class
        assign(nameObject,.Object,envir=parent.frame())
        return(invisible())
})

#### methods to init and append order as records to the data frame

#' @export
#' @docType methods
setGeneric(name='initOrderdataframe'
          ,function(.Object)
                   {standardGeneric('initOrderdataframe')})
#' @aliases initOrderdataframe,Order,ANY-method
setMethod('initOrderdataframe'
         ,'Order'
   ,function(.Object){
            odf <- data.frame( c(getId(.Object))  ,c( getTrid(.Object) )
                            ,c(getOaccount(.Object)) , c( getTicker(.Object))
                            ,c( getIncDate(.Object,chr=TRUE))
                            ,c(getType(.Object)) ,c(getStatus(.Object))
                            ,c( getChgDate(.Object,chr=TRUE))
                            ,c( getQty(.Object)) ,c( getRegprice(.Object)) ,c(getFillprice(.Object))
                            , stringsAsFactors=FALSE )
            names(odf) <- c("id", "trid",  "acc", "tkr", "incdate", "type", "status",  "chgdate"
            	           ,"qty", "rprice", "fprice")
            #browser()
      return(odf)
})


#' @export
#' @docType methods
setGeneric(name='toOrderrecAslist'
          ,function(.Object)
           {standardGeneric('toOrderrecAslist')})
#' @aliases toOrderrecAslis,Order,ANY-method
setMethod('toOrderrecAslist'
         ,'Order'
   ,function(.Object){
                   orec <- list( getId(.Object)  ,getTrid(.Object) 
                                ,getOaccount(.Object) ,getTicker(.Object)
                                ,getIncDate(.Object,chr=TRUE)
                                ,getType(.Object)  ,getStatus(.Object)
                                ,getChgDate(.Object,chr=TRUE)
                                ,getQty(.Object)  ,getRegprice(.Object) ,getFillprice(.Object) )
      return(orec)
})

setMethod ('show'
           ,'Order'
           , function(object){
           	  sb <- ""
              sb <-paste(sb,"** Order ** ")
              sb <-paste(sb,"id:", getId(object)
                         ,"trid:", getTrid(object)
                         ,"instr:", getTicker(object)
                         ,"type:",  getType(object)
                         ,"status:", getStatus(object)
                         ,"chdate:",getChgDate(object)
                         ,"qty:", getQty(object)
                         ,"rprice:", formatC(getRegprice(object), digits=4, format="f")
                         ,"fprice:", formatC(getFillprice(object), digits=4,format="f")
                         )
              sb <- paste(sb,"\n")
              cat(sb)
           })

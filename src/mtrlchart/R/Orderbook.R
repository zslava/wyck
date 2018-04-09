# TODO: Add comment
#
# Author: zimine
###############################################################################

#'  list of orders on the watch list
#'
#' S4 object which holds a table with trades and a table with orders
#' associated management methods and hooks  to blotter
#'
#' @rdname Orderbook-class
#' @name Orderbook
#' @exportClass Orderbook
setClass(Class='Orderbook'
        ,representation( pfpars='list'
		   	                 ,ometadata='data.frame'
                         ,tmptrade='Trade'
                         ,oidcur="numeric"
                         ,oidseed="numeric"
                         ,tidcur="numeric"
                         ,tidseed="numeric"                         
                         ,odf='data.frame'
                         ,tdf='data.frame'
                        )
       ,prototype(pfpars=as.list(NA)
		             ,ometadata=as.data.frame(NA)
                 ,tmptrade=new('Trade')
                 ,oidcur=0
                 ,oidseed=1
                 ,tidcur=0
                 ,tidseed=1
                 ,odf=as.data.frame(matrix(NA))
                 ,tdf=as.data.frame(matrix(NA))
                )
)

##init portfolio details
#
#'@export
initPorfolioParams  <-function(accid, stratid, pname, iniDate, iniEquity
                             ,risktradepct, actinstrs ){

      if( !is.character(pname)){
        stop(paste("[Orderbook:initPortfolioparams]"
                  ,"pname param expected to be character"))
      }
      if( !is.numeric(iniEquity) || !is.numeric(risktradepct) ){
        stop(paste("[Orderbook:initPortfolioparams]"
                  ,"iniEquity, risktradepct params expected to be numeric"))
      }

      pp <- list( account=accid
                 ,strategy=stratid
                 ,name=pname
                 ,iniDate=as.POSIXct(iniDate)
                 ,iniEquity=iniEquity
                 ,risktrpct=risktradepct
                 ,activeinst=actinstrs
                 )
      return(pp)
} 

#constructor
#' Initializes Orderbook vectors with config data
#'
#' @export
initOrderbook <-function(pparams
                        ,instrvec, typevec,currencyvec,ticksizevec
                        ,tickvalvec,unitfeevec, marginvec, slippvec, relweigthvec){

 ##validation
  if(!is.list(pparams)){
    stop(paste("[Orderbook:initOrderbook validation]"
    ,"pparams expected to be a list filled with portfolio params"))

  }
  if(   length(instrvec) != length(typevec)
     || length(instrvec) != length(currencyvec)
     || length(instrvec) != length(ticksizevec)
     || length(instrvec) != length(tickvalvec) ){
    stop(paste("[Orderbook:initOrderbook validation]"
		,"instrument list vectors with config info"
		,"are of different lengths"))
  }

  o <- new("Orderbook")
  odfr <- data.frame( instrvec,typevec
                     ,currencyvec,ticksizevec
                     ,tickvalvec,unitfeevec, marginvec, slippvec, relweigthvec
                     ,stringsAsFactors=FALSE)
  names(odfr) <- c('IDNAM', 'TYP', 'CURR' ,'TKSIZ', 'TKVAL','UFEE', 'UINIMARG','TKSLIPP', 'RWGHT')
  o@ometadata <- odfr
  o@pfpars <- pparams

  return(o)
}


#constructor
#' Initializes Orderbook  with config data in a csv file
#'
#' @export
initOrderbookcsv <- function(pparams, csvfile){
  if ( !file.exists(csvfile)){
	stop(paste("[Orderbook:initOrderbookcsv validation]"
    		  ,csvfile,"not found"))
  }
  ldef <- read.csv(csvfile,skip=1,header=FALSE)
  instrvec <- as.character(ldef[,1])
  typevec <- as.character(ldef[,2])
  currencyvec <- as.character(ldef[,3])
  ticksizevec <- as.numeric(ldef[,4])
  tickvaluevec <- as.numeric(ldef[,5])
  feesvec <- as.numeric(ldef[,6])
  marginvec <- as.numeric(ldef[,7])
  slippvec <- as.numeric(ldef[,8])
  relweightvec <- as.numeric(ldef[,9])

  ob <-  initOrderbook(pparams
                      ,instrvec, typevec,currencyvec
                      ,ticksizevec,tickvaluevec,feesvec
                      ,marginvec,slippvec, relweightvec)
  return(ob)
}


#' add blotter initialization
#'
#' @export
#' @docType methods
setGeneric(name='initOrderbookBlotter'
          ,function(object, tickers)
                   {standardGeneric('initOrderbookBlotter')})
#' @aliases initOrderbookBlotter,Orderbook,ANY-method
setMethod('initOrderbookBlotter'
         ,'Orderbook'
   ,function(object,tickers){

  ###############blotter part#################
  #.blotter <- new.env()

  if(missing(tickers)){
   stop(paste("[Orderbook:initOrderbookBlotter validation]"
             ,"expected an array of ticker as input parameter"))
  }


  #blotter define currencies
  odfr <- object@ometadata
  
  currencyvec <- odfr$CURR
  portfname <- object@pfpars$name
  #accname <- object@pfpars$account
  accname <- portfname

  fmt <- "%Y-%m-%d"
  iDatestr<-strftime(as.POSIXct(object@pfpars$iniDate),format=fmt)
  iniEquity <- object@pfpars$iniEquity

  ucurr <- unique(currencyvec)
  for(j in 1:length(ucurr)){
      currency(ucurr[j])  ## blotter function
  }
  #blotter define instruments
  for(j in 1:length(tickers) ){
    ctkr <- tickers[j]
    ctype <- getInstrmetinfo(object, ticker=ctkr, mtype="itype")
    ccurr <- getInstrmetinfo(object, ticker=ctkr, mtype="currency")
    cmult <- getInstrmetinfo(object, ticker=ctkr, mtype="contrsize")
    ctick <- getInstrmetinfo(object, ticker=ctkr, mtype="ticksize")
    
  switch(ctype
   ,"eq"=stock(primary_id=ctkr
                   ,currency=ccurr
                   ,multiplier=cmult
                   ,tick_size=ctick
             )
   ,"ix"=stock(primary_id=ctkr
                    ,currency=ccurr
                    ,multiplier=cmult
                    ,tick_size=ctick
             )
   ,"fut"=future(primary_id=ctkr
                     ,currency=ccurr
                     ,multiplier=cmult
                    ,tick_size=ctick
                )
   )
  }
  ##blotter init portfolio and account
  symbols <- tickers
  #( to reinitialize, the  .blotter environment should be reset by .blotter <-new.env() )
  initPortf(name=portfname,symbols=symbols, initDate=iDatestr)
  initAcct(name=accname,portfolios=portfname,initDate=iDatestr,initEq=iniEquity)


  })

#' get next free identity integer for Order or Trade 
#'
#' @export
#' @docType methods
setGeneric(name='getNextIdentity'
          ,function(object, ivar)
                   {standardGeneric('getNextIdentity')})
#' @aliases getNextIdentity,Orderbook,ANY-method
setMethod('getNextIdentity'
         ,'Orderbook'
   ,function(object, ivar){
    okvars <- c("order", "trade")
    if( !(ivar %in% okvars)){
        stop(paste("[Orderbook:getNextIdentity validation]"
          ,"supplied param for identity variable can be order or trade"))      
    }
    if(ivar == "order"){
      return( object@oidcur + object@oidseed )
    }
    if(ivar == "trade"){
      return( object@tidcur + object@tidseed )
    }
})



#' store the next free identity integer for Order or Trade after its immediate first usage
#'
#' @export
#' @docType methods
setGeneric(name='storeNextIdentity'
          ,function(.Object, ivar)
                   {standardGeneric('storeNextIdentity')})
#' @aliases storeNextIdentity,Orderbook,ANY-method
setMethod('storeNextIdentity'
         ,'Orderbook'
   ,function(.Object, ivar){
    okvars <- c("order", "trade")
    if( !(ivar %in% okvars)){
        stop(paste("[Orderbook:storeNextIdentity validation]"
          ,"supplied param for identity variable can be order or trade"))      
    }
    nameObject <- deparse(substitute(.Object))
    if(ivar == "order"){
      .Object@oidcur <- .Object@oidcur + .Object@oidseed
    }
    if(ivar == "trade"){
      .Object@tidcur <- .Object@tidcur + .Object@tidseed
    }
    assign(nameObject,.Object,envir=parent.frame())
    return(invisible())
})

#' get next free identity integer for Order or Trade 
#'
#' @export
#' @docType methods
setGeneric(name='getTemptrade'
          ,function(object)
                   {standardGeneric('getTemptrade')})
#' @aliases getTemptrade,Orderbook,ANY-method
setMethod('getTemptrade'
         ,'Orderbook'
   ,function(object){
      return( object@tmptrade )
    
})


#' get active portfolio instruments
#'
#' @export
#' @docType methods
setGeneric(name='getActiveInstruments'
          ,function(object)
                   {standardGeneric('getActiveInstruments')})
#' @aliases getActiveInostruments,Orderbook,ANY-method
setMethod('getActiveInstruments'
         ,'Orderbook'
   ,function(object){
      return( object@pfpars$activeinst )
})

#' @export
#' @docType methods
  setGeneric(name='setActiveInstruments<-'
      ,function(object,value){standardGeneric('setActiveInstruments<-')})
#' @aliases setTstatus,Orderbook,ANY-method
  setReplaceMethod('setActiveInstruments'
                  ,'Orderbook'
      ,function(object,value){
          #validation
          if (!is.character(value)){
          stop(paste("[Orderbook:setActiveInstruments validation]"
            ,"supplied param for instruments expected to be an array of chars"))
          }    
        object@pfpars$activeinst <- value
        return(object)
      })

#' get active portfolio instruments
#'
#' @export
#' @docType methods
setGeneric(name='getWatchInstruments'
          ,function(object)
                   {standardGeneric('getWatchInstruments')})
#' @aliases getActiveInostruments,Orderbook,ANY-method
setMethod('getWatchInstruments'
         ,'Orderbook'
   ,function(object){
      return( object@pfpars$watchinst )
})


#' @export
#' @docType methods
  setGeneric(name='setWatchInstruments<-'
      ,function(object,value){standardGeneric('setWatchInstruments<-')})
#' @aliases setTstatus,Orderbook,ANY-method
  setReplaceMethod('setWatchInstruments'
                  ,'Orderbook'
      ,function(object,value){
          #validation
          if (!is.character(value)){
          stop(paste("[Orderbook:setWatchInstruments validation]"
            ,"supplied param for instruments expected to be an array of chars"))
          }    
        object@pfpars$watchinst <- value
        return(object)
      })


####### CRUD 4 methods for Order records in data frame

#' @export
#' @docType methods
setGeneric(name='insOrderRec'
          ,function(.Object, order)
                   {standardGeneric('insOrderRec')})
#' @aliases insOrderRec,Orderbook,ANY-method
setMethod('insOrderRec'
         ,'Orderbook'
   ,function(.Object, order){
      ##object validation
      if( !is(order, 'Order')){
        stop(paste("[Orderbook:insOrderRec validation]"
          ,"supplied param for order is not of type Order"))
      }
      nameObject <- deparse(substitute(.Object))
      odf <- .Object@odf
      noid  <- getNextIdentity(.Object, "order")
      setId(order) <-noid

       #two cases         
      if(is.na(odf)[1] || nrow(odf) == 0 ) {  # case 1. insert first order in empty data frame
           odf <- initOrderdataframe(order)
           storeNextIdentity(.Object, "order")
      }else{ #case 2  append order to non-empty data frame
            ##additional constraint: check for id uniqueness
            if( !is.na(match(noid, odf$id))){
              stop(paste("[Orderbook:insOrderRec validation]"
               ,"order id already exists in the orders data frame. Skipping insertion.."))
           }
           orec <- toOrderrecAslist(order)
           odf<-rbind(odf,orec)   
           storeNextIdentity(.Object, "order")
      }
      .Object@odf <- odf   
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' @export
#' @docType methods
setGeneric(name='updOrderRec'
          ,function(.Object, order)
                   {standardGeneric('updOrderRec')})
#' @aliases updOrderRec,Orderbook,ANY-method
setMethod('updOrderRec'
         ,'Orderbook'
   ,function(.Object, order){
      ##object validation
      if( !is(order, 'Order')){
        stop(paste("[Orderbook:updOrderRec validation]"
          ,"supplied param for order is not of type Order"))
      }
      nameObject <- deparse(substitute(.Object))
   
       odf <- .Object@odf
       oid <- getId(order)
       ridx <- match(oid, odf$id)  
       if( is.na(ridx)) {
              stop(paste("[Orderbook:updOrderRec validation]"
               ,"order id not present in the orders data frame. Skipping update.."))
       }       
       orec <- toOrderrecAslist(order)
       odf[ridx,] <- orec
      .Object@odf <- odf   
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' @export
#' @docType methods
setGeneric(name='rmOrderRec'
          ,function(.Object, id)
                   {standardGeneric('rmOrderRec')})
#' @aliases rmOrderRec,Orderbook,ANY-method
setMethod('rmOrderRec'
         ,'Orderbook'
   ,function(.Object, id){
      ##object validation
      if( !is.numeric(id)){
        stop(paste("[Orderbook:rmOrderRec validation]"
          ,"supplied param for id is not numeric"))
      }
      nameObject <- deparse(substitute(.Object))
      odf<-.Object@odf
      cidx <- match(id, odf$id)
      if(is.na(cidx)){
        print(paste("rmOrderRec warning: no order records found with id", id))
      }else{
        odf <- odf[-cidx,]
      }
      .Object@odf <- odf   
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' @export
#' @docType methods
setGeneric(name='getOrderRec',function(.Object, id)
                   {standardGeneric('getOrderRec')})
#' @aliases getOrderRec,Orderbook,ANY-method
setMethod('getOrderRec','Orderbook',function(.Object, id){
      ##object validation
      if( !is.numeric(id)){
        stop(paste("[Orderbook:getOrderRec validation]"
          ,"supplied param for id is not numeric"))
      }
      odf<-.Object@odf
      cidx <- match(id, odf$id )
      if(is.na(cidx)){
        cat("Orderbook:getOrderRec warning: no order records found with id", id, "\n")
        return(NA)
      }else{
        rec <- odf[cidx,]
        return(as.list(rec)) ## return df row as a list
      }
})

#' wrapper for getOrderRec
#'
#' @export
#' @docType methods
setGeneric(name='getOrderFromRec',function(object, id)
                   {standardGeneric('getOrderFromRec')})
#' @aliases getOrderFromRec,Orderbook,ANY-method
setMethod('getOrderFromRec','Orderbook',function(object, id){
     o <- crOrderFromrec( getOrderRec(object, id=id))
     return(o)
})

####### CRUD 4 methods for Trade records in data frame

#' @export
#' @docType methods
setGeneric(name='insTradeRec'
          ,function(.Object, trade)
                   {standardGeneric('insTradeRec')})
#' @aliases insTradeRec,Orderbook,ANY-method
setMethod('insTradeRec'
         ,'Orderbook'
   ,function(.Object, trade){
      ##object validation
      if( !is(trade, 'Trade')){
        stop(paste("[Orderbook:insTradeRec validation]"
          ,"supplied param for trade is not of type Trade"))
      }
      #biz constraint: cannot insert trade which has no attached orders
      if( !hasOrdersattached(trade)){
        stop(paste("[Orderbook:insTradeRec validation]"
          ,"this trade does not have orders attached. Skipping insertion.."))
      }
      nameObject <- deparse(substitute(.Object))
      tdf <- .Object@tdf
      ntid  <- getNextIdentity(.Object, "trade")
      setTid(trade) <-ntid  ## set to this trade the newly created id
      setOrdersTrid(trade) ## !!set to attached orders the newly created trid 
      
       #two cases         
      if(is.na(tdf)[1] || nrow(tdf) == 0) {  # case 1. insert first trade in empty data frame
           tdf <- initTradedataframe(trade)  
           storeNextIdentity(.Object, "trade")
      }else{ #case 2  append trade to non-empty data frame
            ##additional constraint: check for id uniqueness
            if( !is.na(match(ntid, tdf$tid))){
              stop(paste("[Orderbook:insTradeRec validation]"
               ,"trade id already exists in the tradess data frame. Skipping insertion.."))
           }
           
           trec <- toTraderecAslist(trade)
           tdf<-rbind(tdf,trec)  ## faulty line ! all rows in dataframe should be in same tzone
           storeNextIdentity(.Object, "trade")
      }
      ##cascade insert of attached orders
      atorders <- getTorders(trade)
      for( j in 1:length(atorders)){
          insOrderRec(.Object, atorders[[j]] )
      }
      .Object@tdf <- tdf   
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


#' @export
#' @docType methods
setGeneric(name='updTradeRec'
          ,function(.Object, trade, cascade=TRUE)
                   {standardGeneric('updTradeRec')})
#' @aliases updTradeRec,Orderbook,ANY-method
setMethod('updTradeRec'
         ,'Orderbook'
   ,function(.Object, trade, cascade=TRUE){
      ##object validation
      if( !is(trade, 'Trade')){
        stop(paste("[Orderbook:updTradeRec validation]"
          ,"supplied param for order is not of type Trade"))
      }
      nameObject <- deparse(substitute(.Object))
   
       tdf <- .Object@tdf
       tid <- getTid(trade)
       tidx <- match(tid, tdf$tid)  
       if( is.na(tidx)) {
              stop(paste("[Orderbook:updOrderRec validation]"
               ,"trade id not present in the orders data frame. Skipping update.."))
       }       
       trec <- toTraderecAslist(trade)
       tdf[tidx,] <- trec
      .Object@tdf <- tdf
      ##cascading update of attached orders
      if( cascade && hasOrdersattached(trade) ) {
         atorders <- getTorders(trade)
        for( j in 1:length(atorders)){
          updOrderRec(.Object, atorders[[j]] )
        }
      }
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' @export
#' @docType methods
setGeneric(name='rmTradeRec'
          ,function(.Object, tid)
                   {standardGeneric('rmTradeRec')})
#' @aliases rmTradeRec,Orderbook,ANY-method
setMethod('rmTradeRec'
         ,'Orderbook'
   ,function(.Object, tid){
      ##object validation
      if( !is.numeric(tid)){
        stop(paste("[Orderbook:rmTradeRec validation]"
          ,"supplied param for id is not numeric"))
      }
      nameObject <- deparse(substitute(.Object))
      trade <- getTradeFromRec(.Object, tid)
      tdf<-.Object@tdf
      cidx <- match(tid, tdf$tid)
      if(is.na(cidx)){
        print(paste("rmTradeRec warning: no Trade record found with tid", tid))
      }else{
        tdf <- tdf[-cidx,]
      }
      .Object@tdf <- tdf
      ##cascade rm of attached orders
      if( hasOrdersattached(trade) ) {
         atorders <- getTorders(trade)
        for( j in 1:length(atorders)){
          rmOrderRec(.Object, getId(atorders[[j]]) )
        }
      }         
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


#' @export
#' @docType methods
setGeneric(name='getTradeRec'
          ,function(.Object, tid)
                   {standardGeneric('getTradeRec')})
#' @aliases getTradeRec,Orderbook,ANY-method
setMethod('getTradeRec'
         ,'Orderbook'
   ,function(.Object, tid){
      ##object validation
      if( !is.numeric(tid)){
        stop(paste("[Orderbook:getTradeRec validation]"
          ,"supplied param for tid is not numeric"))
      }
      tdf<-.Object@tdf
      ctidx <- match(tid, tdf$tid )
      if(is.na(ctidx)){
        cat("Orderbook:getTradeRec warning: no order records found with tid", tid, "\n")
        return(NA)
      }else{
        rec <- tdf[ctidx,]
        return(as.list(rec)) ## return df row as a list
     }
})

#' wrapper for getOrderRec
#'
#' @export
#' @docType methods
setGeneric(name='getTradeFromRec',function(object, tid)
                   {standardGeneric('getTradeFromRec')})
#' @aliases getTradeFromRec,Orderbook,ANY-method
setMethod('getTradeFromRec','Orderbook',function(object, tid){
     t <- crTradeFromrec( getTradeRec(object, tid=tid))
     #tid <- getTid(t)
     odf <- object@odf
     tordrecs <- odf[odf$trid == tid,]
     if(nrow(tordrecs)>0){
       attachOrdersFromrecset(t,tordrecs)
     }
     return(t)
})

##############################################
############# user bisness methods ###########
##############################################

#' check for trade attractiviness from RR perspective
#' store the trade object into the orderbook as temp trade
#
#' @export
#' @docType methods
setGeneric(name='check4trade'
          ,function(.Object, instr, incdate, pent, pstop, ftarget, mtarget, verbose=TRUE)
                   {standardGeneric('check4trade')})
#' @aliases check4trade,Orderbook,ANY-method
setMethod('check4trade'
         ,'Orderbook'
   ,function(.Object, instr, incdate, pent, pstop, ftarget, mtarget, verbose=TRUE){
      ##method validation
      if( !is.numeric(pent) || !is.numeric(pstop)  ||
          !is.numeric(ftarget) || !is.numeric(mtarget)) {
        stop(paste("[Orderbook:check4trade validation]"
          ,"all supplied params after Orderbook expected to be numeric"))
      }
      okinstrs <- .Object@ometadata$IDNAM
      if( !instr %in% okinstrs ){
        stop(paste("[Orderbook:check4trade validation]"
          ,"no trading configuration for the supplied instrument", instr))
      }
      nameObject <- deparse(substitute(.Object))
      tr <- mkDummyTrade(instr, incdate, pent, pstop, ftarget, mtarget)
      metdf <-.Object@ometadata

      ##tr size based on risk percentage from the current portfolio equity
      #browser()

      marginreq <- getInstrmetinfo(.Object, ticker=instr, mtype="imarginpct")
      contr_size <- getInstrmetinfo(.Object, ticker=instr, mtype="contrsize")

      #riskcap <- .Object@pfpars$iniEquity * .Object@pfpars$risktrpct
      riskcap <-  calcPortfolioEquity(.Object, edate=incdate) * .Object@pfpars$risktrpct

      #browser()

      ## trade size based capped by risk per trade
      #trmaxqty <-   riskcap / ( marginreq * contr_size * abs(pent - pstop))
      trmaxqty <-   riskcap / (  contr_size * abs(pent - pstop))
      

      ## cash available for instrument
      eqavlbl <- eqtyAlloc4instrument(.Object, ticker=instr)
      travlblqty <- eqavlbl  /  ( marginreq * contr_size * pent )

      if( pent - pstop > 0)  trsign <- 1
      else  trsign <- -1
          
      #setTqty(tr) <- trsign * floor( trmaxqty)
      setTqty(tr) <- trsign * trmaxqty

      # if(travlblqty < trmaxqty  ){
      #     setTqty(tr) <- trsign * round( travlblqty, digits=2)        
      #     }else{ 
      #     setTqty(tr) <- trsign * round( trmaxqty, digits=2)
      # }
      setStratid(tr) <- .Object@pfpars$strategy
      setTaccount(tr) <- .Object@pfpars$account

      #print potential trade
      show(tr)
     
      # msg if trade is attractive
      if( getMtargetRR(tr) >=3.0 && verbose){
          msg <- paste(" Trade signal! Attractive main target RR"
                    ,getMtargetRR(tr) )
          cat(msg,"\n")
      }
      ## msg if capital funding problem
      if (travlblqty < trmaxqty ){
          msg <- paste(" Instrument equity available only for position size"
                    ,travlblqty, "versus risk budjet size:",trmaxqty )
          cat(msg,"\n")
      }
      .Object@tmptrade <- tr
      #browser()
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


#' openTrade  either 'm' for  market trade with immediate fill + limit stop order
#' or 'l' for limit trade with placing a 1 limit order
#
#' @export
#' @docType methods
setGeneric(name='openTrade'
          ,function(.Object, mtype, incdate, qty, entry, comment, plstop=TRUE, verbose=TRUE)
                   {standardGeneric('openTrade')})
#' @aliases openTrade,Orderbook,ANY-method
setMethod('openTrade'
         ,'Orderbook'
   ,function(.Object, mtype, incdate, qty, entry,comment, plstop=TRUE, verbose=TRUE){
      ##method validation
      oktypes <- c('m', 'l')
      if( !mtype %in% oktypes ){
        stop(paste("[Orderbook:openTrade validation]"
          ,"mtype param",mtype,"not in the list of authorized values", cat(oktypes) ))
      }
      tr <- .Object@tmptrade
      ttkr <- getTticker(tr)
      if(is.na(ttkr)){
        stop(paste("[Orderbook:openTrade validation]"
          ,"orderbook temp trade appears uninitialized,"
          ," run a check for potential trade method"))        
      }
      nameObject <- deparse(substitute(.Object))
      omd <- .Object@ometadata
      if(!missing(comment)) { setComment(tr) <- comment }
      setTqty(tr) <-  qty
      setEntry(tr) <- entry
      #browser()
      setTincDate(tr) <- incdate
      islippage <- omd[omd$IDNAM==ttkr,]$TKSLIPP * omd[omd$IDNAM==ttkr,]$TKSIZ

      if( mtype =='m'){  ## case for market order trade
         
         poso <- mkMarketOrder(getTid(tr), getTaccount(tr), getTticker(tr) 
                               ,getTincDate(tr), getTqty(tr), getEntry(tr), islippage )
         setTradeOrder(tr) <- poso
         if(verbose) { print("trade opened with market order") 
         show(poso)
         } 
         if(plstop){
          stopo <- mkLimitOrder( getTid(tr), getTaccount(tr), getTticker(tr)  ## stop limit order
                               ,getTincDate(tr), -1*getTqty(tr), getStop(tr) )          
          setTradeOrder(tr) <- stopo
          if(verbose) { 
            print("placed additional limit stop order") 
            show(stopo)
          }
         }
      }else{  ## mtype=='l' case for limit order
         poso <- mkLimitOrder( getTid(tr), getTaccount(tr), getTticker(tr)  ## stop limit order
                               ,getTincDate(tr),  getTqty(tr), getEntry(tr) ) 
         #browser()
         setTradeOrder(tr) <- poso                                                 
         if(verbose) { print("trade opened with limit working order") 
         show(poso)
         } 
      }

      ### update trade status
      updTradeStatus(tr,verbose=verbose)
      #browser()
      ##record new trade into df
      insTradeRec(.Object, tr)   ## szi check here
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


#' cancelTrade  
#' only can cancel a trade with zero exposure and working orders
#
#' @export
#' @docType methods
setGeneric(name='cancelTrade'
          ,function(.Object, tid, chgdate)
                   {standardGeneric('cancelTrade')})
#' @aliases cancelTrade,Orderbook,ANY-method
setMethod('cancelTrade'
         ,'Orderbook'
   ,function(.Object, tid, chgdate){
      tr <- getTradeFromRec(.Object, tid=tid)
      tpos <- getTradePosition(tr)
      twords <- getTradeWorkingOrders(tr)
      tostatus <- getTradeOrdersStatus(tr)
      diststatus <- unique(tostatus)
      if( tpos != 0 ||   length(diststatus) != 1  || diststatus[1] != 'w' ){
        stop(paste("[Orderbook:cancelTrade validation]"
          ,"trade with id:",tid, "can not be cancelled"
          ,"as it has non-zero exposure or other than only working orders"))        

      }
      nameObject <- deparse(substitute(.Object))
      for(j in 1:length(twords)){  #cancel each trade order
        setStatus(twords[[j]]) <- 'c'
        setChgDate(twords[[j]]) <- chgdate
        setTradeOrder(tr) <- twords[[j]]
      }

      #setTstatus(tr) <- 'c'
      updTradeStatus(tr,verbose=verbose)
      ##record updated traded into df
      updTradeRec(.Object,tr, cascade=TRUE)
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())     
    })


#' eraseTrade  
#' erase trade and associated orders.
#' should be used only for administrative tasks
#' e.g. trade entered with a mistake
#
#' @export
#' @docType methods
setGeneric(name='eraseTrade'
          ,function(.Object, tid)
                   {standardGeneric('eraseTrade')})
#' @aliases eraseTrade,Orderbook,ANY-method
setMethod('eraseTrade'
         ,'Orderbook'
   ,function(.Object, tid){
      tr <- getTradeFromRec(.Object, tid=tid)
      nameObject <- deparse(substitute(.Object))
      ##cascading delete of trade record from df
      rmTradeRec(.Object, tid=tid)
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())     
    })

#' eraseOrder
#' should be used only for administrative tasks
#' e.g. order entered with a mistake
#
#' @export
#' @docType methods
setGeneric(name='eraseOrder'
          ,function(.Object, oid)
                   {standardGeneric('eraseOrder')})
#' @aliases eraseOrder,Orderbook,ANY-method
setMethod('eraseOrder'
         ,'Orderbook'
   ,function(.Object, oid){
      nameObject <- deparse(substitute(.Object))
      ord <- getOrderFromRec(.Object, id=oid)
      tid <- getTrid(ord)
      tr <- getTradeFromRec(.Object, tid=tid)
      ots <- getTstatus(tr)

      # biz functionality
      rmOrderRec(.Object, id=oid)
      removeTradeOrder(tr, ord)
      #update trade status if sttus is changed
      updTradeStatus(tr)
      if( getTstatus(tr) != ots ){
        updTradeRec(.Object, tr, cascade=FALSE)   
      }
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())     
    })

#' update trade working orders qty to match open position
#' should be used only for administrative tasks
#' e.g. order entered with a mistake
#
#' @export
#' @docType methods
setGeneric(name='updOrdersTradePosition'
          ,function(.Object, tid=0, chgdate=Sys.time(), verbose=T)
                   {standardGeneric('updOrdersTradePosition')})
#' @aliases updOrdersTradePosition,Orderbook,ANY-method
setMethod('updOrdersTradePosition'
         ,'Orderbook'
   ,function(.Object, tid=0,chgdate=Sys.time(), verbose=T){
      nameObject <- deparse(substitute(.Object))
      tr <- getTradeFromRec(.Object, tid=tid)

      # biz functionality
      updOrdersizeOpenposition(tr, chgdate=chgdate, verbose=verbose)
      updTradeStatus(tr)
      updTradeRec(.Object, tr, cascade=TRUE)   
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())     
    })


#' addTradeOrder  either 'm' for  market trade with immediate fill
#' or 'l' for limit trade with placing a limit order
#
#' @export
#' @docType methods
setGeneric(name='addTradeOrder'
          ,function(.Object, tid, incdate, mtype , qty, price, verbose=TRUE)
                   {standardGeneric('addTradeOrder')})
#' @aliases addTradeOrder,Orderbook,ANY-method
setMethod('addTradeOrder'
         ,'Orderbook'
   ,function(.Object, tid, incdate, mtype, qty, price,verbose=TRUE){
      ##method validation
      oktypes <- c('m', 'l')
      if( !mtype %in% oktypes ){
        stop(paste("[Orderbook:addTradeOrder validation]"
          ,"mtype param",mtype,"not in the list of authorized values", cat(oktypes) ))
      }
      nameObject <- deparse(substitute(.Object))      
      tr <- getTradeFromRec(.Object, tid=tid)
      ttkr <- getTticker(tr)      
      omd <- .Object@ometadata

      islippage <- omd[omd$IDNAM==ttkr,]$TKSLIPP * omd[omd$IDNAM==ttkr,]$TKSIZ
      islippage <- min( islippage, price/1000)   #bring down slippage  to price * 1/1000 value  for small adjusted prices

      if( mtype =='m'){  ## case for market order trade
         opos <- getTradePosition(tr) 
         no <- mkMarketOrder(getTid(tr), getTaccount(tr), getTticker(tr) 
                            ,incdate, qty, price, islippage )
         setTradeOrder(tr) <- no
         if (verbose) {
           cat('added market order to trade: ', tid, '\n')
           show(no)
         }
      }else{  ## mtype=='l' case for limit order         
         no <- mkLimitOrder( getTid(tr), getTaccount(tr), getTticker(tr)  ## stop limit order
                             ,incdate,  qty, price ) 
         setTradeOrder(tr) <- no                                                  
         if (verbose) {
           cat('added limit order to trade: ', tid, '\n')
           show(no)
         }
      }
      ## record new order into df
      insOrderRec(.Object,no)
      ##update trade status
      updTradeStatus(tr,verbose=verbose)
      ##update trade object ito df for a possible trade status change
      updTradeRec(.Object, tr, cascade=FALSE)   
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#' updTradeOrder  methods covers several biz cases which change an order
#' belonging to a trade
#
#' @export
#' @docType methods
setGeneric(name='updTradeOrder'
          ,function(.Object, oid, chgdate, param, value=0, verbose=TRUE)
                   {standardGeneric('updTradeOrder')})
#' @aliases updTradeOrder,Orderbook,ANY-method
setMethod('updTradeOrder'
         ,'Orderbook'
   ,function(.Object, oid, chgdate, param, value=0, verbose=TRUE){
      ##method validation
      okpars <- c('cancel', 'tomarket', 'unfill', 'price', 'qty')
      if( !param %in% okpars ){
        stop(paste("[Orderbook:updTradeOrder validation]"
          ,"supplied param not in the list of authorized values", cat(okpars) ))
      }
      if( !is.numeric(value) ){
        stop(paste("[Orderbook:updTradeOrder validation]"
          ,"supplied value expected to be of type numeric" ))
      }
      nameObject <- deparse(substitute(.Object))
      ##common  prerequisitory part
      omodified <- FALSE
      o <-  getOrderFromRec(.Object, id=oid)
      trid <- getTrid(o)
      tr <- getTradeFromRec(.Object, tid=trid)     
      otype<- getType(o)
      ost <- getStatus(o)
      ticker <- getTicker(o)

      omd <- .Object@ometadata
      #islippage <- omd[omd$IDNAM==ticker,]$TKSLIPP * omd[omd$IDNAM==ticker,]$TKSIZ
      islippage <- getInstrmetinfo(.Object, ticker=ticker, mtype="contrslippage")

      #biz case 1. cancel order    
      if(param == 'cancel'){
        if( otype == 'l' && ost == 'w'){
          cancelOrder(o, chgdate)
          setTradeOrder(tr) <- o
          omodified <- TRUE
          if(verbose) { cat("canceled order: ", oid, "\n")} 
          # #check if trades should change status to 'cancelled' (contains only cancelled orders)
          # aostatuses <- unlist(lapply( getTorders(tr), function(x){getStatus(x)}))
          # if ( length(unique(aostatuses))==1 && aostatuses[1] == 'c'){
          #     setTstatus(tr) <- 'c'    
          #    if(verbose) { cat("triggered cancelling trade: ", trid, "\n")} 
          # }
        }else{
        cat("failed attempt to cancel order", oid, "which is not limit working order","\n")
        }   
      }
      #biz case 2. turn limit order to market order, (exit exposure)
      if(param == 'tomarket'){
        if( otype == 'l' && ost == 'w'){
          if(value == 0){
            stop(paste("[Orderbook:updTradeOrder validation]"
            ,"supplied value for price expected to be a positive numeric"))
          }            
          ##create new market order and set it to trade
          qty <- getQty(o)
          fprice <- value + sign(getQty(o)) * islippage
          
          #####replace by new market order
          # addTradeOrder(.Object, tid=trid, incdate=chgdate,  mtype="m"
          #              ,qty=qty, price=value,verbose=verbose)
          no <- mkMarketOrder( tradeid=trid, acc=getTaccount(tr), tkr=getTticker(tr) 
                             , incDate=chgdate, qty=qty, price=value, slippg=islippage )

          noid <- getNextIdentity(.Object, ivar="order")
          setId(no) <- noid
          setTradeOrder(tr) <- no
          insOrderRec(.Object,no) 
          #peek  new order id

         #cancel existing order
          cancelOrder(o, chgdate)
          setTradeOrder(tr) <- o


          # utr <- getTradeFromRec(.Object, tid=trid) 
          # ids <- getOrdersattachedIds(utr)
          # noid <- max(ids)
          omodified <- TRUE

          if(verbose) { cat("turned limit order: ", oid, "to new market order", noid, getQty(o),"@", fprice, "\n") } 
          #browser()
          #2.1 change trade status if position is now zero, pay attention utr and tr
        #   if ( getTradePosition(utr) == 0) { setTstatus(tr) <- 'n'}
        #   else                             {  setTstatus(tr) <- 'y'}           
        }else{
        cat("failed to update order", oid, "to market order as it is not a limit working order","\n")    
        }
      }        
      #biz case 2.1 unfill order (roll back incorrectly filled  limit order turned to market)
      if(param == 'unfill'){
        if( otype == 'm' && ost == 'f'){
          setChgDate(o) <- chgdate
          setType(o) <- 'l'
          setStatus(o) <- 'w'
          setFillprice(o) <- 0
          setTradeOrder(tr) <- o
          omodified <- TRUE
          if(verbose) { cat("turned filled market order: ", oid, "back to a limit working order","\n")} 
          # #2.1 change trade status if position is now zero
          # if ( getTradePosition(tr) == 0) { setTstatus(tr) <- 'n'}
          # else                             {  setTstatus(tr) <- 'y'}           
        }else{
        cat("failed to unfill order", oid, "to market order as it is not a market filled order","\n")
        }   
      }      
      #biz case 3. adjust price level( trail stop, adjust target)
      if(param == 'price'){
        if( otype == 'l' && ost == 'w'){
          if(value == 0){
            stop(paste("[Orderbook:updTradeOrder validation]"
            ,"supplied value for price expected to be a positive numeric"))
          }            
          setChgDate(o) <- chgdate
          setRegprice(o) <- value
          setTradeOrder(tr) <- o
          omodified <- TRUE
          if(verbose) { cat("changed price for working order: ", oid, "to",value,"\n") }                    
        }else{
           cat("failed attempt to change order ", oid, "price which is not a limit working order","\n")
        }
      } 
      #biz case 4. adjust qty  (change exposure)
      if(param == 'qty'){
        if( otype == 'l' && ost == 'w'){
          if(value == 0){
            stop(paste("[Orderbook:updTradeOrder validation]"
            ,"supplied value for quantity expected to be a positive numeric"))
          }            
          setChgDate(o) <- chgdate
          setQty(o) <- value
          setTradeOrder(tr) <- o
          omodified <- TRUE
          if(verbose) { cat("changed qty for working order: ", oid, "to",value,"\n") }                    
        }else{
           cat("failed attempt to change order ", oid, "qty which is not a limit working order","\n")
        }
      } 
      ## common final part
      ##update trade status
      #browser()
      updTradeStatus(tr,verbose=verbose)
      updTradeRec(.Object, tr, cascade=TRUE) 
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


#' get instrument working ordes
#' 
#'
#' @export
#' @docType methods
setGeneric(name='getWorkingOrders'
          ,function(.Object, ticker, cdate, verbose=TRUE )
                   {standardGeneric('getWorkingOrders')})
#' @aliases getWorkingOrders,Orderbook,ANY-method
setMethod('getWorkingOrders'
         ,'Orderbook'
          ,function(.Object, ticker, cdate, verbose=TRUE ){

      nameObject <- deparse(substitute(.Object))
      iacc <- .Object@pfpars$account
      odf <- .Object@odf
      words <- odf[ odf$acc == iacc
                  & odf$tkr== ticker 
                  & odf$status=='w'
                  & as.POSIXct(odf$incdate) <= as.POSIXct(cdate) , ]  ## to posixct to compare dates

      if(nrow(words) == 0) { return(list())}            
      
      l<-list()
      for (j in 1:nrow(words)){
        cord <- crOrderFromrec(as.list(words[j,]))
        l[[length(l)+1]] <- cord
      }
      return(l)
})          

#' procOrderFills  checks all instrument working limit orders for a fill
#'
#' @export
#' @docType methods
setGeneric(name='procOrderFills'
          ,function(.Object, ticker, cdate, data, verbose=TRUE )
                   {standardGeneric('procOrderFills')})
#' @aliases procOrderFills,Orderbook,ANY-method
setMethod('procOrderFills'
         ,'Orderbook'
   ,function(.Object, ticker, cdate, data, verbose=TRUE ){
      ##method validation
      if(!is.numeric(unclass(as.POSIXct(cdate)))){
       stop(paste("[Orderbook:procOrderFills validation]"
          ,"supplied cdate param expected to be POSIXct string"))
      }
      if ( !is.xts(data) || length(data) == 0 ){   
      stop(paste("[Orderbook:procOrderFills validation]"
          ,"supplied data param expected to be an OHLC xts"))
      }          
      nameObject <- deparse(substitute(.Object))
      #0  set instrument slippage & accnt
      omd <- .Object@ometadata

      islippage <- getInstrmetinfo(.Object, ticker=ticker, mtype="contrslippage") 

      #logic to bring down slippage  to price * 1/1000 value  
      # for small adjusted prices (stocks in yahoo finance)
      islippage <- min(islippage , Cl(last(data))* 1/1000  )

      iacc <- .Object@pfpars$account
      ##1.1 get all instrument working orders  up to cdate for curr account
      worders<-getWorkingOrders(.Object,ticker=ticker,cdate=cdate,verbose=FALSE)

      #1.2 compute  global position in these trades
      ogpos <- calcInstrumentPosition(.Object, ticker, edate=cdate)$gpos
      data_freq <- getXtsSecondsFrequency(data) #xts data frequency
       
      if(length(worders)==0){
        if(verbose) {print(paste("instrument:",ticker,"no working orders to fill..")) } 
        return(invisible())
      }
      #2. iterate over working orders 
      for (j in 1:length(worders)){
        cord <- worders[[j]]
        #2.1 strip data (we do not need to strip data here, szi implement cfill intraday)

        #2.2 actual check if order is filled
        if(nrow(data) > 0 ) { checkFillOrder(cord, data=data, cdate=cdate
                              ,dfreq=data_freq,slippage=islippage, verbose=verbose) } #key func
        #browser()
        if ( getStatus(cord) == 'f'  ){
            ctr <- getTradeFromRec(.Object, tid=getTrid(cord))
            setTradeOrder(ctr)<- cord # reattach changed order to traade
            #2.3 change trade status if position is now zero
            updTradeStatus(ctr,verbose=verbose)
            #2.4 update order record and trade record
            updTradeRec(.Object, ctr, cascade=TRUE) 
        }
      }#iterate over working orders
      #################################
      # iterate over concerned trades (update their status order)
      #################################
      #3.0 recompute global position of concerned trades
      ngpos <- calcInstrumentPosition(.Object, ticker, edate=cdate)$gpos
      #3.1 print message if gpos changed
      if(ngpos != ogpos && verbose ){
        print(paste("account:", iacc,"instrument:",ticker
                   ,"date:",cdate, "global position changed from"
                   ,ogpos, "to", ngpos))
      }                            
      
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


#' @export
#' @docType methods
setGeneric(name='printTrades'
          ,function(object, ticker, sdate, edate, mmprice=0, ttd=FALSE,otd=FALSE, ctd=FALSE)
                   {standardGeneric('printTrades')})
#' printTrades  checks all instrument working limit orders for a fill
#'
#' @param Orderbook instance
#' @param ticker (char)  instrument ticker
#' @param n=-1 (numeric) Print n tail trades. if n=-1 print all trades
#' @param mmprice=0 (numeric) mark to market price
#' @param otn=FALSE ( boolean) print only trades with open position
#' @param ct=FALSE ( boolean)  if TRUE also print cancelled trades
#  @param co = FALSE (boolean) if TRUE print cancelled orders 
#' @aliases printTrades,Orderbook,ANY-method
setMethod('printTrades'
         ,'Orderbook'
   ,function(object, ticker, sdate, edate , mmprice=0, ttd=FALSE,otd=FALSE,ctd=FALSE){
      ##method validation
      # if(!is.numeric(unclass(as.POSIXct(edate)))){
      #  stop(paste("[Orderbook:printTrades validation]"
      #     ,"supplied edate param expected to be POSIXct string"))
      # }
      oktickers <- object@ometadata$IDNAM
      if( !(ticker %in%  oktickers)){
        stop(paste("[Orderbook:printTrades validation]"
          ,"supplied ticker param invalid value"))      
      }
      if(missing(sdate)){
        sdate <- object@pfpars$iniDate
      }


      if(missing(edate)){
        edate <- as.POSIXct(Sys.time(), tz="UTC")
      }

      iacc <- object@pfpars$account
      tdf <- object@tdf
      trecs <- NA
      cmult <- getInstrmetinfo(object, ticker=ticker, mtype="contrsize")

      #1. select instrument trade records subset
      if(ctd){
        trecs <- getTradesSubset(object, ticker=ticker, sdate=sdate, edate=edate, crit="canceled")
      }else if(otd){
        trecs <- getTradesSubset(object, ticker=ticker, sdate=sdate, edate=edate, crit="open")        
      }else if(ttd){
        trecs <- getTradesSubset(object, ticker=ticker, sdate=sdate, edate=edate, crit="closed")        
      }else{  ##include cancelled trades
        trecs <- getTradesSubset(object, ticker=ticker, sdate=sdate, edate=edate, crit="all")
      }
         
      if(nrow(trecs)==0) { return() }  ## no trades to print

    
      
      #sort trecs on status i.e. trades with position at the end  
      trecs <- trecs[ with(trecs, order(status)),]
 

      #2. iterate to print each trade
      tids <- trecs$tid
      for(j in 1:length(tids)){
        cat('################## ticker ',ticker, ' #######################\n')
        printTrade( getTradeFromRec(object, tid=tids[j]), mmprice=mmprice, contrmult=cmult, co=FALSE )  #do not print cancelled orders
      }

      #3. print global position
      printInstrGlpos(object, ticker=ticker, sdate=sdate, mmprice=mmprice, edate=edate)

})

#' @export
#' @docType methods
setGeneric(name='printInstrGlpos'
          ,function(object, ticker, sdate, mmprice, edate=Sys.time() )
                   {standardGeneric('printInstrGlpos')})
#' @aliases printInstrGlpos,Orderbook,ANY-method
setMethod('printInstrGlpos'
         ,'Orderbook'
   ,function(object, ticker, sdate, mmprice, edate=Sys.time() ){

      if(missing(sdate)){
        sdate <- object@pfpars$iniDate
      }
      if(missing(mmprice)){
       stop(paste("[Orderbook:printInstrGlpos validation]"
          ,"please specify markt 2 market price"))
      }
      pos <- calcInstrumentPosition(object, ticker=ticker, sdate=sdate, edate=edate)

      st <- paste(edate)
      st <- paste(st, ticker, "position:", pos$gpos)
      if(pos$gpos > 0) {
        st <- paste(st, "; open trades ids: ", paste(pos$otrids,collapse=",") )
      }
      #global pnl
      rpnl <- calcInstrumentPnl(object, ticker=ticker, mmprice=mmprice, sdate=sdate, edate=edate, ptype="realized")
      upnl <- calcInstrumentPnl(object, ticker=ticker, mmprice=mmprice, sdate=sdate, edate=edate, ptype="unrealized")

      #gpnl <- calcInstrumentPnl(object, ticker=ticker, mmprice=mmprice, sdate=sdate, edate=edate)
      srpnl <-formatC(rpnl, digits=1, format='f')
      supnl <-formatC(upnl, digits=1, format='f')
      
      st <- paste(st, "PNL realized:",srpnl, "unrealized:",supnl)

      print(st)
    
    })


#' calcInstrumentPosition  
#
#' @export
#' @docType methods
setGeneric(name='calcInstrumentPosition'
          ,function(object, ticker, sdate, edate=Sys.time() )
                   {standardGeneric('calcInstrumentPosition')})
#' @aliases calcInstrumentPosition,Orderbook,ANY-method
setMethod('calcInstrumentPosition'
         ,'Orderbook'
   ,function(object, ticker, sdate, edate=Sys.time() ){

      iacc <- object@pfpars$account
      tdf <- object@tdf

      if(missing(sdate)){
        sdate <- object@pfpars$iniDate
      }
      gpos <- 0 #global position
      noptrs <- 0 #number of trades with open positions
      optrsids <- c()  #ids of trades with open position
      result <- list(gpos=gpos, notr=noptrs, otrids=optrsids)
      trecs <- getTradesSubset(object, ticker=ticker, sdate=sdate, edate=edate, crit="open")
      if (nrow(trecs) == 0) { return ( result ) }
      tids <- trecs$tid
      for( j in 1:length(tids)){
         ctr <- getTradeFromRec(object, tids[j])
         ctpos <- getTradePosition(ctr)
         gpos <- gpos + ctpos
         if(ctpos != 0){
          noptrs <- noptrs + 1
          optrsids[length(optrsids)+1] <- tids[j]
         }

      }
      result <- list(gpos=gpos, notr=noptrs, otrids=optrsids)
      #result <- gpos      
      return(  result )
})



# calcInstrumentPnl  OK reviwed
#
#' @export
#' @docType methods
setGeneric(name='calcInstrumentPnl'
          ,function(object, ticker, mmprice, sdate , edate=Sys.time(), ptype="both" )
                   {standardGeneric('calcInstrumentPnl')})
#' @aliases calcInstrumentPnl,Orderbook,ANY-method
setMethod('calcInstrumentPnl'
         ,'Orderbook'
   ,function(object, ticker, mmprice, sdate,  edate=Sys.time(), ptype="both" ){

      iacc <- object@pfpars$account
      tdf <- object@tdf

      if(missing(ticker)){
       stop(paste("[Orderbook:calcInstrumentPnl validation]"
          ,"please specify ticker"))
      }
      if(missing(mmprice)){
       stop(paste("[Orderbook:calcInstrumentPnl validation]"
          ,"please specify mark2market price"))
      }
      if(missing(sdate)){
        sdate <- object@pfpars$iniDate
      }
      ptypeokvals <- c("both", "realized", "unrealized")
    if( !(ptype %in% ptypeokvals)){
           sokvals <- paste(ptypeokvals,collapse=" ")
           stop(paste("[Orderbook:calcInstrumentPnl validation]"
                     ,"supplied param for ptype should have 1 of expected values:",sokvals))
     }
        
      cmult <- getInstrmetinfo(object, ticker=ticker, mtype="contrsize")
      ##filter ticker trades between two dates  
      trecs <-  getTradesSubset(object, ticker=ticker, sdate=sdate, edate=edate, crit="uncanceled")
      pnl <- 0
      #browser()
      if ( is.null(trecs)  || nrow(trecs) == 0) { return(0) }
      tids <- trecs$tid
      for( j in 1:length(tids)){
          ctr <- getTradeFromRec(object, tids[j])
          trpnl <- switch(ptype
               ,"both"      = getTradeMtmvalue(ctr,mmprice=mmprice,contrmult=cmult)
               ,"realized"  = getTradeRpnl(ctr, contrmult=cmult)
               ,"unrealized"= getTradeUpnl(ctr, mmprice=mmprice,contrmult=cmult)
          )
          pnl <- pnl + trpnl
          #browser()
      }
      return( pnl )
})




# calculate portfolio current value = iniEquity + realized pnl
#'
#' @export
#' @docType methods
setGeneric(name='calcPortfolioEquity'
          ,function(object, sdate , edate=Sys.time() )
                   {standardGeneric('calcPortfolioEquity')})
#' calculate portfolio current value  from the list of active instruments
#'
#' @param Orderbook instance
#' @param ticker (char)  instrument ticker
#' @param sdate (char)  (optional) start date valuation period  default is porfolio initial date
#' @param edate (char)  (optional) end date valuation period. default is current bar date
#' @aliases calcPortfolioEquity,Orderbook,ANY-method
setMethod('calcPortfolioEquity'
         ,'Orderbook'
  ,function(object, sdate,  edate=Sys.time() ){
    if(missing(sdate)){
        sdate <- object@pfpars$iniDate
    }
   mmprice <- 0 # do not need mark2market price for cur equity value  
   
   portfeqty <- object@pfpars$iniEquity

   instrs <- getActiveInstruments(object)
   for(j in 1:length(instrs)){
     ctkr <- instrs[j]
     cirpnl <- calcInstrumentPnl(object, ticker=ctkr, mmprice=mmprice, sdate=sdate, edate=edate, ptype="realized")
     portfeqty <- portfeqty + cirpnl
   }
  return(portfeqty)
})

#' @export
#' @docType methods
setGeneric(name='calcPortfolioCash'
          ,function(object, sdate , edate=Sys.time() )
                   {standardGeneric('calcPortfolioCash')})
#' calculate portfolio current value  from the list of active instruments
#'
#' @param Orderbook instance
#' @param ticker (char)  instrument ticker
#' @param sdate (char)  (optional) start date valuation period  default is porfolio initial date
#' @param edate (char)  (optional) end date valuation period. default is current bar date
#' @aliases calcPortfolioCash,Orderbook,ANY-method
setMethod('calcPortfolioCash'
         ,'Orderbook'
  ,function(object, sdate,  edate=Sys.time() ){
    if(missing(sdate)){
        sdate <- object@pfpars$iniDate
    }   
    fcash <- calcPortfolioEquity(object, sdate=sdate, edate=edate )  
    lcash <- 0
    instrs <- getActiveInstruments(object)
    for(j in 1:length(instrs)){
     ctkr <- instrs[j]
     cmargin <- getInstrmetinfo(object, ticker=ctkr, mtype="imarginpct")
     ccontrsize <- getInstrmetinfo(object, ticker=ctkr, mtype="contrsize")

     trecs <-  getTradesSubset(object, ticker=ctkr, sdate=sdate, edate=edate, crit="uncanceled")
     if ( is.null(trecs)  || nrow(trecs) == 0) { next }
     tids <- trecs$tid
     for( k in 1:length(tids)){
          ctr <- getTradeFromRec(object, tids[k])
          clcash <- getTradeLockedCash(ctr, contrmult=ccontrsize, marginpct=cmargin)
          lcash <- lcash + clcash
     }
   }
  fcash <- fcash - lcash # portfCash - all locked cash
  return(fcash)
})




#' @export
#' @docType methods
setGeneric(name='eqtyAlloc4instrument'
          ,function(object, ticker, sdate , edate=Sys.time() )
                   {standardGeneric('eqtyAlloc4instrument')})
#' calculate portfolio current value  from the list of active instruments
#'
#' @param Orderbook instance
#' @param ticker (char)  instrument ticker
#' @param sdate (char)  (optional) start date valuation period  default is porfolio initial date
#' @param edate (char)  (optional) end date valuation period. default is current bar date
#' @aliases eqtyAlloc4instrument,Orderbook,ANY-method
setMethod('eqtyAlloc4instrument'
         ,'Orderbook'
  ,function(object, ticker, sdate,  edate=Sys.time() ){

    totaleqty <- calcPortfolioEquity(object, sdate=sdate, edate=edate)
    actinstrs <- getActiveInstruments(object)
    instrShare <- getInstrmetinfo(object, ticker=ticker, mtype="prelweight") / length(actinstrs)
    eqty4instr <- instrShare * totaleqty
    return(eqty4instr)
})


###---------------
#' @export
#' @docType methods
setGeneric(name='blotterPnl'
          ,function(object, ticker, xdata, sdate , edate=Sys.time(),  npf=1,init=FALSE, chart=TRUE, tz='EST', verbose=TRUE )
                   {standardGeneric('blotterPnl')})
#' calculate portfolio current value  from the list of active instruments
#'
#' @param Orderbook instance
#' @param ticker (char)  instrument ticker
#' @param sdate (char)  (optional) start date valuation period  default is porfolio initial date
#' @param edate (char)  (optional) end date valuation period. default is current bar date
#' @aliases blotterPnl,Orderbook,ANY-method
setMethod('blotterPnl'
         ,'Orderbook'
  ,function(object, ticker, xdata, sdate,  edate=Sys.time(), npf=1,init=FALSE, chart=TRUE, tz='EST', verbose=TRUE ){

      ctz <- Sys.getenv("TZ")
      Sys.setenv(TZ=tz) # what is impact of this at posixct converstion

      portfname <- object@pfpars$name
      inidate <- object@pfpars$iniDate
      iniEqty <- object@pfpars$iniEquity
       portf.st <- portfname
       acct.st <- portfname
      
      if(init){
         ##initOrderbookBlotter(object,ticker) 

         #temp solution
         currency("USD")
         stock(ticker, currency=getInstrmetinfo(object, ticker, "currency")
              ,tick_size=getInstrmetinfo(object, ticker, "ticksize")  )
         symbols <- c(ticker)
         initPortf(portf.st,symbols, initDate=inidate)
         initAcct(acct.st,portfolios=portf.st, initDate=inidate, initEq=iniEqty)
      }

      #browser()
      assign(ticker, xdata, envir=parent.frame(n=npf))

      d1fmt <- "%Y-%m-%d"
      s1fmt <- "%Y-%m-%d %H:%M:%S"
      contrMult <- getInstrmetinfo(object, ticker=ticker, mtype="contrsize")
      rndPrec <- 4
      # get filled orders
      forecs <- getOrdersSubset(object, ticker=ticker, sdate=sdate, edate=edate, ocrit='fall')
       if ( is.null(forecs)  || nrow(forecs) == 0) { return(invisible()) }

       ##sort on chdate.. 

      #browser()
      forecs <- forecs[ with(forecs, order(chgdate)),] # sorting on char date continues to work if dt in format %Y-%m-%d %H:%M:%S



      # iterate through orders recs
      for( j in 1:nrow(forecs)){
         crec <- forecs[j,]

         #browser()
          
         addTxn(portfname
            ,Symbol=ticker
            ,TxnDate=base::format(adjEODmins(crec$chgdate,tz), format=s1fmt ) #put internal func to treat 00:00:00
            ,TxnQty=crec$qty
            ,TxnPrice=round( crec$fprice, digits=rndPrec )
            ,TxnFees=0
            ,ConMult <- contrMult
            ,verbose=verbose  )
      }

      ### update nav on every day
      edt <- as.POSIXct( base::format(edate, format=d1fmt), tz=tz ) 
      dtes <- paste(sdate,'::',edt,sep='')
  
      #browser()
  
      updatePortf(portf.st, Dates = index(xdata[dtes]) )
      updateAcct(acct.st, Dates = index(xdata[dtes]) )
      updateEndEq(acct.st, Dates = index(xdata[dtes]) )

      # optionally chart  
      if(chart){
        chart.Posn(portfname, Symbol=ticker, Dates=dtes)

        # ret1 <- PortfReturns(acct.st)
        # ret1$total <- rowSums(ret1)
        # dev.new()
        # charts.PerformanceSummary(ret1$total,geometric=FALSE,wealth.index=TRUE)
      } 
                 
      ##print transactiosn & stats
      txn<-  getTxns(Portfolio=portfname, Symbol=ticker)
      print(txn)

      trstats <- tradeStats(portfname, ticker)
      print(trstats)

      rm(ticker,envir=parent.frame(n=1))  #remove data 

      Sys.setenv(TZ=ctz)

   return(invisible())
})

###-----------------


#' @export
#' @docType methods
setGeneric(name='blotterPnl_bk'
          ,function(object, ticker, xdata, sdate , edate=Sys.time(),  npf=1,init=FALSE, chart=TRUE, verbose=TRUE )
                   {standardGeneric('blotterPnl_bk')})
#' calculate portfolio current value  from the list of active instruments
#'
#' @param Orderbook instance
#' @param ticker (char)  instrument ticker
#' @param sdate (char)  (optional) start date valuation period  default is porfolio initial date
#' @param edate (char)  (optional) end date valuation period. default is current bar date
#' @aliases blotterPnl,Orderbook,ANY-method
setMethod('blotterPnl_bk'
         ,'Orderbook'
  ,function(object, ticker, xdata, sdate,  edate=Sys.time(), npf=1,init=FALSE, chart=TRUE, verbose=TRUE ){


      portfname <- object@pfpars$name
      inidate <- object@pfpars$iniDate
      iniEqty <- object@pfpars$iniEquity
       portf.st <- portfname
       acct.st <- portfname
      
      if(init){
         ##initOrderbookBlotter(object,ticker) 

         #temp solution
         currency("USD")
         stock(ticker, currency=getInstrmetinfo(object, ticker, "currency")
              ,tick_size=getInstrmetinfo(object, ticker, "ticksize")  )
         symbols <- c(ticker)
         initPortf(portf.st,symbols, initDate=inidate)
         initAcct(acct.st,portfolios=portf.st, initDate=inidate, initEq=iniEqty)
      }

      #browser()
      assign(ticker, xdata, envir=parent.frame(n=npf))

      d1fmt <- "%Y-%m-%d"
      s1fmt <- "%Y-%m-%d %H:%M:%S"
      contrMult <- getInstrmetinfo(object, ticker=ticker, mtype="contrsize")
      rndPrec <- 4
      # get filled orders
      forecs <- getOrdersSubset(object, ticker=ticker, sdate=sdate, edate=edate, ocrit='fall')
       if ( is.null(forecs)  || nrow(forecs) == 0) { return(invisible()) }

       ##sort on chdate.. 
      #trecs <- trecs[ with(trecs, order(incdate)),]
      #browser()
      forecs <- forecs[ with(forecs, order(chgdate)),]

      adjEODmins <-function(date){
        pdate <- as.POSIXct(date)
        if ( as.numeric(strftime(pdate,format='%H')) 
            +  as.numeric(strftime(pdate,format='%M'))
            + as.numeric(strftime(pdate,format='%S')) ==0 ){
                  pdate <- pdate + 86399
         }
         return(pdate)
      }

      # iterate through orders recs
      for( j in 1:nrow(forecs)){
         crec <- forecs[j,]

         #browser()
          
         addTxn(portfname
            ,Symbol=ticker
#            ,TxnDate=strftime( crec$chgdate, format=s1fmt ) #put internal func to treat 00:00:00
            ,TxnDate=strftime( adjEODmins(crec$chgdate) ) #put internal func to treat 00:00:00
            ,TxnQty=crec$qty
            ,TxnPrice=round( crec$fprice, digits=rndPrec )
            ,TxnFees=0
            ,ConMult <- contrMult
            ,verbose=verbose  )
      }

      ### update nav on every day
      edt <- as.POSIXct( strftime(edate), format=d1fmt ) + 86399 
      dtes <- paste(sdate,'::',edt,sep='')
  
      #browser()
  
      updatePortf(portf.st, Dates = index(xdata[dtes]) )
      updateAcct(acct.st, Dates = index(xdata[dtes]) )
      updateEndEq(acct.st, Dates = index(xdata[dtes]) )

      # optionally chart  
      if(chart){
        chart.Posn(portfname, Symbol=ticker, Dates=dtes)

        # ret1 <- PortfReturns(acct.st)
        # ret1$total <- rowSums(ret1)
        # dev.new()
        # charts.PerformanceSummary(ret1$total,geometric=FALSE,wealth.index=TRUE)
      } 
                 
      ##print transactiosn & stats
      txn<-  getTxns(Portfolio=portfname, Symbol=ticker)
      print(txn)

      trstats <- tradeStats(portfname, ticker)
      print(trstats)

      rm(ticker,envir=parent.frame(n=1))  #remove data 
   return(invisible())
})


####################### internal non-exported functions #########


###islippage <- omd[omd$IDNAM==ttkr,]$TKSLIPP * omd[omd$IDNAM==ttkr,]$TKSIZ

#'
#' @docType methods
setGeneric(name='getInstrmetinfo'
          ,function(object, ticker, mtype='contrslippage')
                   {standardGeneric('getInstrmetinfo')})
#' @aliases getInstrmetinfo,Orderbook,ANY-method
setMethod('getInstrmetinfo'
         ,'Orderbook'
   ,function(object, ticker, mtype='contrslippage'){

    if(missing(ticker)){
       stop(paste("[Orderbook:getInstrmetinfo validation]"
                     ,"ticker param is mandatory"))
    }
    mtypeokvals <- c("itype", "currency", "ticksize","tickvalue"
                     ,"contrsize", "contrfee", "imarginpct"
                     ,"contrslippage","prelweight")
    if( !(mtype %in% mtypeokvals)){
           sokvals <- paste(mtypeokvals,collapse=" ")
           stop(paste("[Orderbook:getInstrmetinfo validation]"
                     ,"supplied param for mtype should have 1 of expected values:",sokvals))
     }
     omd <- object@ometadata
     contrSize <- omd[omd$IDNAM==ticker,]$TKVAL / omd[omd$IDNAM==ticker,]$TKSIZ
     metainfo <- switch(mtype
                  ,"itype"    =   omd[omd$IDNAM==ticker,]$TYP
                  ,"currency" =   omd[omd$IDNAM==ticker,]$CURR
                  ,"ticksize" =   omd[omd$IDNAM==ticker,]$TKSIZ
                  ,"tickvalue" =  omd[omd$IDNAM==ticker,]$TKVAL
                  ,"contrsize" = contrSize
                  ,"contrfee" =  contrSize * omd[omd$IDNAM==ticker,]$UFEE
                  ,"imarginpct" = omd[omd$IDNAM==ticker,]$UINIMARG
                  ,"contrslippage" = omd[omd$IDNAM==ticker,]$TKSLIPP * omd[omd$IDNAM==ticker,]$TKSIZ
                  ,"prelweight"     = omd[omd$IDNAM==ticker,]$RWGHT
                 )
     return(metainfo)
})

#'
#' @docType methods
setGeneric(name='getTradesSubset'
          ,function(object, ticker, sdate, edate=Sys.time(), crit='all')
                   {standardGeneric('getTradesSubset')})
#' @aliases getTradesSubset,Orderbook,ANY-method
setMethod('getTradesSubset'
         ,'Orderbook'
   ,function(object, ticker, sdate, edate=Sys.time() , crit='all'){

    if(missing(sdate)){
      sdate <- object@pfpars$iniDate
    }
    critokvals <- c("all", "uncanceled", "canceled","closed", "open")
    if( !(crit %in% critokvals)){
           sokvals <- paste(critokvals,collapse=" ")
           stop(paste("[Orderbook:getTradesSubset validation]"
                     ,"supplied param for crit should have 1 of expected values:",sokvals))
     }
    tdf <- object@tdf 
    iacc <- object@pfpars$account     
  
    #crit biz cases
    trecs <-c() #empty null vector
    #browser()
    if(!is.na(tdf)[1]){
    trecs <- tdf [ tdf$acc == iacc
                   & tdf$tkr== ticker
                   & as.POSIXct(tdf$incdate)  > as.POSIXct(sdate)
                   & as.POSIXct(tdf$incdate)  <= as.POSIXct(edate) , ]
    #browser()
      if(length(trecs)>0) {
        trecs <- switch(crit
         ,"all"          = trecs     
         ,"uncanceled"   = trecs [ trecs$status != 'c' , ] 
         ,"canceled"     = trecs [ trecs$status == 'c' , ] 
         ,"closed"       = trecs [ trecs$status == 'n' , ] 
         ,"open"         = trecs [ trecs$status == 'y' , ] 
        )
      }
     }
    return(trecs)
})

#' @docType methods
setGeneric(name='getOrdersSubset'
          ,function(object, ticker, sdate, edate=Sys.time(), tcrit='all', ocrit="all")
                   {standardGeneric('getOrdersSubset')})
#' @aliases getOrdersSubset,Orderbook,ANY-method
setMethod('getOrdersSubset'
         ,'Orderbook'
   ,function(object, ticker, sdate, edate=Sys.time() , tcrit='all', ocrit="all"){
    if(missing(sdate)){
      sdate <- object@pfpars$iniDate
    }
    ocritokvals <- c("all", "uncanceled", "canceled", "fall",  "wall")
    
    if( !(ocrit %in% ocritokvals) ){
           sokvals <- paste(ocritokvals,collapse=" ")
           stop(paste("[Orderbook:getOrdersSubset validation]"
                     ,"supplied param for ocrit:", ocrit, "should have 1 of expected values:",sokvals))
     }
     iacc <- object@pfpars$account    
     odf <- object@odf 
     
     orecs <- c() #empty null vector
     if(!is.na(odf)[1]){ 
       orecs <- odf [ odf$acc == iacc
                     & odf$tkr == ticker
                     & as.POSIXct(odf$incdate)  > as.POSIXct(sdate)
                     & as.POSIXct(odf$incdate)  <= as.POSIXct(edate) , ]
                     
        if(length(orecs)>0) {              
         orecs <- switch(ocrit,
            "all"      = orecs
           ,"canceled" = orecs [ orecs$status == 'c' , ]
           ,"uncanceled" = orecs [ orecs$status != 'c' , ]
           ,"fall"     = orecs [ orecs$status == 'f' , ]
           ,"wall"     = orecs [ orecs$status == 'w' , ]
         )
        }
      } 
      return( orecs )
})
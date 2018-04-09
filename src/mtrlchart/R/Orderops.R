

##### check for potential trade
 #' @export
 #' @docType methods
setGeneric(name='tdck'
	     ,function(.Object,cbl=F, cbs=F, arby=F
	     	              ,pent=0, pstop=0, ft=0, mt=0,sav=FALSE){standardGeneric('tdck')})
 #' check for a potential trade
 #'
 #' @param Wwatchlist instance
 #' @param cbl (boolean) use last bar to check a potential trade for long
 #' @param cbs (boolean) use last bar to check a potential trade for short
 #' @param arby (boolean) use arbitrary price when bar picking instead of bar's hi, low
 #' @param pent (numeric) entry price. Default value  = 0. If price  = 0 it will be set interactively
 #' @param pstop (numeric) stop price. Default value  = 0. If price  = 0 it will be set interactively
 #' @param ft (numeric) first target price. Default value  = 0. If price  = 0 it will be set interactively
 #' @param mt (numeric) main target price. Default value  = 0. If price  = 0 it will be set interactively
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdck,Wwatchlist,ANY-method
setMethod('tdck','Wwatchlist'
    ,function(.Object ,cbl=F ,cbs=F ,arby=F
    	      ,pent=0, pstop=0, ft=0, mt=0,sav=FALSE){
      ##validation
        if ( dev.cur() == 1 ){
         stop(paste("[Wwatchlist:pc4trade validation]"
                   ,"missing chart to place a fact"))
      }
      nameObject <- deparse(substitute(.Object))
      
      ob <- getOrderbook(.Object)
      omd <- ob@ometadata

      cinstr <- getCurinstr(.Object)
      ctf  <- getCurtimeframe(.Object) 

      chkdt <- getMarketdatalastdate(.Object, tkr=cinstr,tf=ctf)

      cdata <- getMarketdataserie(.Object, tkr=cinstr, tf=ctf)
      tzxts <-indexTZ(cdata)
      tksize <- omd[omd$IDNAM==cinstr,]$TKSIZ
      d1fmt <- "%Y-%m-%d"

      ## either use last bar for entry stop or choose them interactively
      if(cbl){
      	pent <- as.numeric(Hi(last(cdata))) + tksize * 1 
      	pstop <- as.numeric(Lo(last(cdata))) - tksize * 1

      }else if(cbs){
      	pstop <- as.numeric(Hi(last(cdata))) + tksize * 1
      	pent <- as.numeric(Lo(last(cdata)))  - tksize * 1
      }else{
      	if(pent==0) {
          cat('Select level of entry for a potential trade:\n ')
          pbar <- zooompick(n=15, arby=arby)
          pent <- pbar$y
          cat('selected price for entry: ', pent, "\n")
        }
        if(pstop==0){    
          cat('Select level of stop for a potential trade :\n ')
          pbar <- zooompick(n=15, arby=arby)
          pstop <- pbar$y
          cat('selected price for stop: ', pstop, "\n")
        }
      }
      #############
      #common part, choose interactively first target and main target
      if(ft==0){
        cat('Select level of first target for a potential trade:\n ')
        pbar <- zooompick(n=15, arby=arby)
        ft <- pbar$y
        cat('selected price for first target: ', ft, "\n")
      }
      if(mt==0){
        cat('Select level of main target for a potential trade :\n ')
        pbar <- zooompick(n=15, arby=arby)
        mt <- pbar$y
        cat('selected price for main target: ', mt, "\n")
      }  
      #call method in Ordrebook
      
      check4trade(ob, instr=cinstr, incdate=chkdt,pent=pent, pstop=pstop
      	          ,ftarget=ft, mtarget=mt, verbose=TRUE)
      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
 })


##### open market or limit trade
 #' @export
 #' @docType methods
setGeneric(name='tdpl'
       ,function(.Object,cmnt="", qty=0, mkto=FALSE, plstop=TRUE, verbose=TRUE,sav=FALSE){standardGeneric('tdpl')})
 #' open market or limit order trade
 #'
 #' @param Wwatchlist instance
 #' @param mkto (boolean) if true open trade with market order 
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdpl,Wwatchlist,ANY-method
setMethod('tdpl','Wwatchlist'
    ,function(.Object ,cmnt="", qty=0, mkto=FALSE, plstop=TRUE, verbose=TRUE,sav=FALSE){

      nameObject <- deparse(substitute(.Object))
      ##validation
      ob <- getOrderbook(.Object)
      tmptrade<- getTemptrade( ob )
      if ( getTqty(tmptrade) == 0 ){
         stop(paste("[Wwatchlist:tdpl validation]"
                   ,"empty temporary trade. Run pc4tr to fill details of potential trade"))
      }

      chkdt <- getMarketdatalastdate(.Object)
      cdata <- getMarketdataserie(.Object)
     
      cpent <- getEntry(tmptrade)
      copstop <- getStop(tmptrade)
      if(qty != 0) {
        cqty <- qty
      }else{
       cqty <- getTqty(tmptrade)
      }

      ##2 scenarios
      # scen 1. market order trade
      
      if(mkto){
        cpent <- as.numeric(Cl(last(cdata)))
        openTrade(ob, mtype='m', incdate=chkdt, qty=cqty, entry=cpent,comment=cmnt, plstop=plstop, verbose=verbose )

      }else{
      # scen 2 limit order trade
        openTrade(ob, mtype='l', incdate=chkdt, qty=cqty, entry=cpent,comment=cmnt, plstop=plstop, verbose=verbose )
      }
      # 
      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

##### administrative erase trade
 #' @export
 #' @docType methods
setGeneric(name='tdrm'
       ,function(.Object, tid=0,sav=FALSE){standardGeneric('tdrm')})
 #' erase trade (only admin usage)
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdrm,Wwatchlist,ANY-method
setMethod('tdrm','Wwatchlist'
    ,function(.Object , tid=0,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)
      #biz method from Orderbook
      eraseTrade(ob, tid=tid)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

##### cancel trade
 #' @export
 #' @docType methods
setGeneric(name='tdca'
       ,function(.Object, tid=0,sav=FALSE){standardGeneric('tdca')})
 #' cancel trade
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdca,Wwatchlist,ANY-method
setMethod('tdca','Wwatchlist'
    ,function(.Object , tid=0,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      chkdt <- getMarketdatalastdate(.Object)
     
      cancelTrade(ob, tid=tid, chgdate=chkdt)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

 #'update wokring trade orders qty to match trade open osition
 #'
 #' @export
 #' @docType methods
setGeneric(name='tdeqty'
       ,function(.Object, tid=0,verbose=TRUE,sav=FALSE){standardGeneric('tdeqty')})
 
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdeqty,Wwatchlist,ANY-method
setMethod('tdeqty','Wwatchlist'
    ,function(.Object , tid=0,verbose=TRUE,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      chkdt <- getMarketdatalastdate(.Object)
      #biz func
      updOrdersTradePosition(ob, tid=tid, chgdate=chkdt, verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})




##### add stop or target order to trade non public function
 #' @docType methods
setGeneric(name='addOrder'
       ,function(.Object, kind='s', tid=0, qty=0, pr=0,verbose=TRUE,sav=FALSE)
       {standardGeneric('addOrder')})
 #' place stop or target order for a trade as a limit order
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @param kind="s" (char)  kind="s" add stop, kind="t" add target
 #' @param qty=0 (numeric)  stop order quantity & direction. if zero, quantity is get from trade  
 #' @param price=0 (numeric)  price of stop, if zoero, price is get from trade param
 #' @return void. Wwatchlist instance is modified
 #' @aliases addOrder,Wwatchlist,ANY-method
setMethod('addOrder','Wwatchlist'
    ,function(.Object , kind='s', tid=0, qty=0, pr=0, verbose=TRUE,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      #validation
      if( pr < 0 ){
        stop(paste("[Orderbook:addTradeOrder validation]"
          ,"mtype param",mtype,"not in the list of authorized values", cat(oktypes) ))
      }      
      ob <- getOrderbook(.Object)
      
      chkdt <- getMarketdatalastdate(.Object)

      tr <- getTradeFromRec(ob, tid=tid)

      if(qty==0) { qty <- -1 * getTqty(tr) }
      if(pr==0) {
        if       (kind=='s') { pr <- getStop(tr) }
        else if  (kind=='t') { pr <- getMtarget(tr) }
      }
      #biz method
      addTradeOrder(ob, tid=tid, incdate=chkdt, mtype='l', qty=qty, price=pr, verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


##### administrative erase order
 #' @export
 #' @docType methods
setGeneric(name='odrm'
       ,function(.Object, oid=0,sav=FALSE){standardGeneric('odrm')})
 #' erase order (only admin usage)
 #'
 #' @param Wwatchlist instance
 #' @param oid (numeric)  order id
 #' @return void. Wwatchlist instance is modified
 #' @aliases odrm,Wwatchlist,ANY-method
setMethod('odrm','Wwatchlist'
    ,function(.Object , oid=0,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)
      #biz method from Orderbook
      eraseOrder(ob, oid=oid)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})



##### cancel limit working order
 #' @export
 #' @docType methods
setGeneric(name='odwca'
       ,function(.Object, oid=0,verbose=TRUE,sav=FALSE){standardGeneric('odwca')})
 #' cancel order
 #'
 #' @param Wwatchlist instance
 #' @param oid (numeric)  order id
 #' @return void. Wwatchlist instance is modified
 #' @aliases odwca,Wwatchlist,ANY-method
setMethod('odwca','Wwatchlist'
    ,function(.Object , oid=0,verbose=TRUE,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      chkdt <- getMarketdatalastdate(.Object)

      updTradeOrder(ob, oid=oid, chgdate=chkdt, param='cancel', verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file      
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


##### add stop limit order to trade
 #' @export
 #' @docType methods
setGeneric(name='tdastop'
       ,function(.Object, tid=0, qty=0, pr=0,verbose=TRUE,sav=FALSE)
       {standardGeneric('tdastop')})
 #' add stop limit order to trade
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdastop,Wwatchlist,ANY-method
setMethod('tdastop','Wwatchlist'
    ,function(.Object , tid=0, qty=0, pr=0,verbose=TRUE,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      addOrder(.Object, kind='s', tid=tid, qty=qty, pr=pr,verbose=verbose,sav=sav)
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

##### add target limit order to trade
 #' @export
 #' @docType methods
setGeneric(name='tdatarg'
       ,function(.Object, tid=0, qty=0, pr=0,verbose=TRUE,sav=FALSE){standardGeneric('tdatarg')})
 #' add target limit order to trade
 #
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdatarg,Wwatchlist,ANY-method
setMethod('tdatarg','Wwatchlist'
    ,function(.Object , tid=0, qty=0, pr=0,verbose=TRUE,sav=FALSE){
      nameObject <- deparse(substitute(.Object))
      addOrder(.Object, kind='t', tid=tid, qty=qty, pr=pr,verbose=verbose,sav=sav)
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

 #' admin add arbitrary order to trade
 #'
 #' @export
 #' @docType methods
setGeneric(name='odadd'
       ,function(.Object, tid=0, type='l', qty=0, pr=0, verbose=TRUE,sav=FALSE){standardGeneric('odadd')})
 #' add market abitrary order to a trade 
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases odadd,Wwatchlist,ANY-method
setMethod('odadd','Wwatchlist'
    ,function(.Object , tid=0, type='m', qty=0, pr=0, verbose=TRUE,sav=FALSE){
      if( qty==0 ){
        stop(paste("[Wwatchlist:odadd validation]"
          ,"please specify non zero qty when adding a new market order for a trade", tid))
      }      
      okvals <- c('m', 'l')
      if( !(type %in% okvals) ){
        sokvals <- paste(okvals,collapse=" ")
        stop(paste("[Watchlist:odadd validation]"
          ,"supplied type param not in the list of authorized values", sokvals ))
      }        
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      if(pr<=0) {
          cat('Select price level for  order:\n ')
          pbar <- zooompick(n=15, arby=FALSE)
          pr <- pbar$y
          cat('selected price for order: ', pr, "\n")
       }

       cat('Select bar to register order:\n ')
       pbar <- zooompick(n=15, arby=FALSE)
       chkdt <- as.POSIXct(pbar$t, tz=pbar$tz)
       cat('selected time for order: ', strftime(pbar$t), "\n")

      #biz method from Orderbook
      addTradeOrder(ob, tid=tid, incdate=chkdt, mtype=type, qty=qty, price=pr, verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

#
#
#
### add market order to trade
 #' @export
 #' @docType methods
setGeneric(name='tdamkt'
       ,function(.Object, tid=0, qty=0, dfreq,verbose=TRUE,sav=FALSE){standardGeneric('tdamkt')})
 #' add market abitrary order to a trade 
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdamkt,Wwatchlist,ANY-method
setMethod('tdamkt','Wwatchlist'
    ,function(.Object , tid=0, qty=0, dfreq, verbose=TRUE,sav=FALSE){
      if( qty==0 ){
        stop(paste("[Wwatchlist:tdamkt validation]"
          ,"please specify non zero qty when adding a new market order for a trade", tid))
      } 
    if (missing(dfreq)){
      #read dfreq from the plotted current chart
      dfreq <- getChartDataPeriodicity(gdevId=dev.cur())$frequency 
    }else{
      if(!is.numeric(dfreq)){
        stop(paste("[Orderops:tdamkt validation]"
            ,"specified param dfreq is expected to be numeric"))    
      }      
    }           
    nameObject <- deparse(substitute(.Object))
    ob <- getOrderbook(.Object)
    
    tr<-getTradeFromRec(ob, tid=tid)
    ttkr <- getTticker(tr) 

    #daily,weekly scenario
    if(dfreq %in% c(86400,604800) ) {
      ctf <- getCurtimeframe(.Object)  # c("d1","w1")

      chkdt <- getMarketdatalastdate(.Object, tkr=ttkr,tf=ctf) 
      cdata <- getMarketdataserie(.Object, tkr=ttkr, tf=ctf)
    }else{ #intraday scnario
      chkdt <- getCintradate(getMarketdata(.Object) )      # returns POSIXct
      cdata <- getIntraCdatedata(getMarketdata(.Object),iname=ttkr,ndays=1,dfreq=dfreq)
    } 
      #pr    <- getMarketdatalastbar(.Object)$CL
      pr <- as.numeric(Cl(last(cdata)))
      #biz method from Orderbook
      addTradeOrder(ob, tid=tid, incdate=chkdt, mtype='m', qty=qty, price=pr, verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})


 #' @export
 #' @docType methods
setGeneric(name='tdalim'
       ,function(.Object, tid=0, qty=0, pr=0, verbose=TRUE,sav=FALSE){standardGeneric('tdalim')})

 #' add limit abitrary order to a trade 
 #'
 #' @param Wwatchlist instance
 #' @param tid (numeric)  trade id
 #' @return void. Wwatchlist instance is modified
 #' @aliases tdalim,Wwatchlist,ANY-method
setMethod('tdalim','Wwatchlist'
    ,function(.Object , tid=0, qty=0, pr=0,verbose=TRUE,sav=FALSE){
      if( qty==0 ){
        stop(paste("[Wwatchlist:tdalim validation]"
          ,"please specify non zero qty when adding a new market order for a trade", tid))
      }      
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      chkdt <- getMarketdatalastdate(.Object) 

      if(pr<=0) {
          cat('Select price level for new limit order:\n ')
          pbar <- zooompick(n=15, arby=FALSE)
          pr <- pbar$y
          cat('selected price for order: ', pr, "\n")
       }

      #biz method from Orderbook
      addTradeOrder(ob, tid=tid, incdate=chkdt, mtype='l', qty=qty, price=pr, verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

### modify limit order price
 #' @export
 #' @docType methods
setGeneric(name='odwmp'
       ,function(.Object, oid=0, pr=0, verbose=TRUE,sav=FALSE){standardGeneric('odwmp')})

 #' modify working limit order price
 #'
 #' @param Wwatchlist instance
 #' @param oid (numeric)  order id
 #' @param pr (numeric)  new order price
 #' @return void. Wwatchlist instance is modified
 #' @aliases odwmp,Wwatchlist,ANY-method
setMethod('odwmp','Wwatchlist'
    ,function(.Object , oid=0, pr=0,verbose=TRUE,sav=FALSE){

    
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      chkdt <- getMarketdatalastdate(.Object)

      if(pr<=0) {
          cat('Select price level for modified order:\n ')
          pbar <- zooompick(n=15, arby=FALSE)
          pr <- pbar$y
          cat('selected price for order: ', pr, "\n")
       }

      #biz method
      updTradeOrder(ob, oid=oid, chgdate=chkdt, param='price'
                    ,value=pr, verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

### modify limit order qty
 #' @export
 #' @docType methods
setGeneric(name='odwmq'
       ,function(.Object, oid=0, qty=0, verbose=TRUE,sav=FALSE){standardGeneric('odwmq')})

 #' modify working limit order qty
 #' 
 #' @param Wwatchlist instance
 #' @param oid (numeric)  order id
 #' @param pr (numeric)  new order price
 #' @return void. Wwatchlist instance is modified
 #' @aliases odwmq,Wwatchlist,ANY-method
setMethod('odwmq','Wwatchlist'
    ,function(.Object , oid=0, qty=0,verbose=TRUE,sav=FALSE){

      if( qty == 0 ){
        stop(paste("[Wwatchlist:odwmq validation]"
          ,"please specify a non-zero qty to modify a quantity of existing order", oid))
      }      
      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      chkdt <- getMarketdatalastdate(.Object)

      #biz method
      updTradeOrder(ob, oid=oid, chgdate=chkdt, param='qty'
                    ,value=qty, verbose=verbose)

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})

###  trasform limit order to market order
 #' @export
 #' @docType methods
setGeneric(name='odw2m'
       ,function(.Object, oid=0, oncl=TRUE, verbose=TRUE,sav=FALSE){standardGeneric('odw2m')})

 #' replace limit order by market order
 #'  
 #' @param Wwatchlist instance
 #' @param oid (numeric)  order id
 #' @return void. Wwatchlist instance is modified
 #' @aliases odw2m,Wwatchlist,ANY-method
setMethod('odw2m','Wwatchlist'
    ,function(.Object , oid=0, oncl=TRUE,verbose=TRUE,sav=FALSE){

      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)

      chkdt <- getMarketdatalastdate(.Object)
      if(oncl){ pr  <- getMarketdatalastbar(.Object)$CL }
      else    { pr  <- getMarketdatalastbar(.Object)$OP }

      #biz method
      updTradeOrder(ob, oid=oid, chgdate=chkdt, param='tomarket'
                    ,value=pr, verbose=verbose)

     ##check for orphaned orders
     o <-  getOrderFromRec(ob, id=oid) 
     trid <- getTrid(o)
     tr <- getTradeFromRec(ob, tid=trid )
     orphanedMsg <- strTradeOrphanedOrders(tr)
     if(orphanedMsg != "no"){
      cat("Trade id: ", trid, " found orphaned workng orders: ", orphanedMsg, "\n" )
     }

      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})



##### print trades

 #' @export
 #' @docType methods
setGeneric(name='tdp'
       ,function(object, sdate, al=FALSE, ttd=FALSE, otd=FALSE, ctd=FALSE){standardGeneric('tdp')})
#' print trades
#'
#' @param Wwatchlist instance
#' @param al=FALSE ( boolean) print only open trades from all instruments in a list
#' @param ttd=FALSE ( boolean)  if TRUE print only trades with closed position
#' @param otd=FALSE ( boolean)  if TRUE print only trades with open position
#  @param ctd = FALSE (boolean) if TRUE print  only cancelled trades
#' @return void. Wwatchlist instance is unmodified
#' @aliases tdp,Wwatchlist,ANY-method
setMethod('tdp','Wwatchlist'
    ,function(object , sdate, al=FALSE, ttd=FALSE, otd=FALSE, ctd=FALSE){
      ob <- getOrderbook(object)
      d1fmt <- "%Y-%m-%d"
      ctf  <- getCurtimeframe(object)

      if(missing(sdate)){
        sdate <- ob@pfpars$iniDate
      }       
      mmprices <- c()
      if(!al){ # 1 instrument

        cinstr <- getCurinstr(object)
        chkdt <- getMarketdatalastdate(object)   
        mmprice <- getMarketdatalastbar(object)$CL  #close on last bar


        printTrades(ob, ticker=cinstr, sdate=sdate, edate=chkdt, mmprice=mmprice
                   , ttd=ttd, otd=otd, ctd=ctd)
     }else{ #trades for all active instruments
        
        instrs <- getActiveInstruments(ob)  ##on on active instruments
        #interate over instruments
        for (j in 1:length(instrs)){
          cinstr <- instrs[j]
          chkdt <- getMarketdatalastdate(object, tkr=cinstr)
          mmprice <- getMarketdatalastbar(object, tkr=cinstr)$CL
          printTrades(ob, ticker=cinstr, sdate=sdate, edate=chkdt, mmprice=mmprice
                     ,ttd=ttd, otd=otd, ctd=ctd)
        }
        ##reprint global position
        cat('\n')
        for (j in 1:length(instrs)){
          cinstr <- instrs[j]    
          chkdt <- getMarketdatalastdate(object, tkr=cinstr)
          mmprice <- getMarketdatalastbar(object, tkr=cinstr)$CL 
          cat("##################################################\n")
          printInstrGlpos(ob, ticker=cinstr, sdate=sdate, mmprice=mmprice, edate=chkdt)
        }
        ### print portfolio numbers, nav, equity, cash
        prfp(object, sdate=sdate, tf=ctf)                  
     }
      return(invisible())
})

##### print porftolio

 #' @export
 #' @docType methods
setGeneric(name='prfp'
       ,function(object, sdate, tf='d1'){standardGeneric('prfp')})
#' print prfpolio numbers
#'
#' @param Wwatchlist instance
#' @param sdate= date a date from which to print prfpolio numbers
#' @return void. Wwatchlist instance is unmodified
#' @aliases prfp,Wwatchlist,ANY-method
setMethod('prfp','Wwatchlist'
 ,function(object , sdate, tf='d1'){
   ob <- getOrderbook(object)
   if(missing(sdate)){
        sdate <- ob@pfpars$iniDate
   } 
   instrs <- getActiveInstruments(ob)
   ftkr <- instrs[1]
   efdate <- getMarketdatalastdate(object, tkr=ftkr, tf=tf)

   pnav <- calcPortfolioNav(object, sdate=sdate, tf=tf)
   peqty <- calcPortfolioEquity(ob, sdate=sdate, edate=efdate)
   pcash <- calcPortfolioCash(ob, sdate=sdate, edate=efdate)

   spnav <-formatC(pnav, digits=1, format='f')
   speqty <-formatC(peqty, digits=1, format='f')
   spcash <-formatC(pcash, digits=1, format='f')
   
   st <- paste("Portfolio NAV:",spnav, "Equity:",speqty, "Cash avlbl:",spcash)
   cat(st,"\n")
   return(invisible())
 })




##### print temp trade

 #' @export
 #' @docType methods
setGeneric(name='tdpt'
       ,function(object){standardGeneric('tdpt')})
#' print currently stored temporary trade result of tdckade
#'
#' @return void. Wwatchlist instance is unmodified
#' @aliases tdpt,Wwatchlist,ANY-method
setMethod('tdpt','Wwatchlist'
    ,function(object){
      ob <- getOrderbook(object)
      tmptrade<- getTemptrade(ob)
      cat("temporary trade details:\n")
      printTrade(tmptrade)
      return(invisible())
})


##### check order fills

 #' @export
 #' @docType methods
setGeneric(name='cfill'
       ,function(.Object, dfreqmin=60, forcedfreq=FALSE,verbose=TRUE,sav=FALSE){standardGeneric('cfill')})
#' check order fills for a current instrument
#'
#' @return void. Wwatchlist instance can be modified
#' @aliases cfill,Wwatchlist,ANY-method
setMethod('cfill','Wwatchlist'
    ,function(.Object, dfreqmin=60, forcedfreq=FALSE,  verbose=TRUE,sav=FALSE){

      nameObject <- deparse(substitute(.Object))

      ob <- getOrderbook(.Object)
 
      cstory <- getCurstory(.Object)
      cinstr <- getSinstrument(cstory)
      mldl <- getMarketdata(.Object)

    if(!is.numeric(dfreqmin)){
        stop(paste("[Orderops:cfill validation]"
            ,"specified param dfreqminn is expected to be numeric"))    
    }
    if( !dfreqmin  %in% getDataFreqValidValues()){
          stop(paste("[Orderops:cfill validation]"
            ,"dfreqmin expected to have a value in", paste(getDataFreqValidValues(),collapse=", ") ))    
    
    }

      #if a chart is present read dfreq from the plotted current chart 
    if (!is.null(dev.list())) { 
      dfreq_chart <- getChartDataPeriodicity(gdevId=dev.cur())$frequency 
     }
     ctf<-'intraday'
      #daily,weekly scenario
     if(dfreq_chart %in% c(86400,604800) || dfreqmin >=86400  ) {
      ctf <- getCurtimeframe(.Object)  # c("d1","w1")
    
      chkdt <- getMarketdatalastdate(.Object, tkr=cinstr,tf=ctf) # returns POSIXct
      cdata <- getMarketdataserie(.Object, tkr=cinstr, tf=ctf)
     }else{ #intraday
      chkdt <- getCintradate(mldl)      # returns POSIXct
      #find how many days of intraday xts data to pull
      worders<- getWorkingOrders(ob, ticker=cinstr, cdate=chkdt )
      oldest_ord_posx <- min(as.POSIXct(origin='1970-01-01'
                                                ,unlist(lapply(worders
                                                  ,function(x){x@changeDate}))))
      ndays <- 1 + (days( as.Date(chkdt) - days(as.Date(oldest_ord_posx)))) @day  #lubridate
      cdata <- getIntraCdatedata(mldl,iname=cinstr,ndays=ndays,dfreq=dfreqmin)
     }

      
      ## biz method from Orderbook
      procOrderFills(ob, ticker=cinstr, cdate=chkdt, data=cdata, verbose=verbose) 
    
      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      #browser()
      return(invisible())
})


##### go to next bar and check for order fills. 

 #' @export
 #' @docType methods
setGeneric(name='gnf'
       ,function(.Object,tf='d1', single=FALSE, verbose=TRUE,sav=FALSE){standardGeneric('gnf')})
#' advance data to next bar and check order fills for all instruments
#'
#' @return void. Wwatchlist instance is unmodified
#' @aliases gnf,Wwatchlist,ANY-method
setMethod('gnf','Wwatchlist'
    ,function(.Object,tf='d1', single=FALSE, verbose=TRUE,sav=FALSE){

      nameObject <- deparse(substitute(.Object))
      ob <- getOrderbook(.Object)
      ctf <- tf
      cstory <- getCurstory(.Object)
      cinstr <- getSinstrument(cstory)

      #wind to next day bar on daily data
      if(ctf=='d1'){
         gnd(.Object,f=FALSE,n=1)
      }
      if(single){ ## check fills for current instrument
        chkdt <- getMarketdatalastdate(.Object, tkr=cinstr,tf=ctf)
        cdata <- getMarketdataserie(.Object, tkr=cinstr, tf=ctf)

        procOrderFills(ob, ticker=cinstr, cdate=chkdt, data=cdata, verbose=verbose) 

      }else{      ## check fills for all acrive portfolio
        instrs <- getActiveInstruments(ob)
       #browser()
       #iterate over instrument list
       for (j in 1:length(instrs)){
         cinstr <- instrs[j]
         chkdt <- getMarketdatalastdate(.Object, tkr=cinstr,tf=ctf)
         cdata <- getMarketdataserie(.Object, tkr=cinstr, tf=ctf)

        ## biz method from Orderbook
        procOrderFills(ob, ticker=cinstr, cdate=chkdt, data=cdata, verbose=verbose)         
       } ## iterate over instrument list
      }
      setOrderbook(.Object) <- ob
      if(sav){persist(.Object)}  #saves to rdat file
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())

})


##### chart pnl for 1 ticket in portfolio. expects watchlist current date to be at the end 
## of portfolio evaluation period
 #' @export
 #' @docType methods
setGeneric(name='ctpnl'
       ,function(.Object, ticker, tf='d1'
                ,sdate=.Object@orderbook@pfpars$iniDate
                ,edate=getCdate(getMarketdata(.Object))
                ,tz="UTC"
                ,verbose=TRUE) {standardGeneric('ctpnl')})
#' advance data to next bar and check order fills for all instruments
#'
#' @return void. Wwatchlist instance is unmodified
#' @aliases ctpnl,Wwatchlist,ANY-method
setMethod('ctpnl','Wwatchlist'
    ,function(.Object, ticker, tf='d1'
                ,sdate=.Object@orderbook@pfpars$iniDate
                ,edate= getCdate(getMarketdata(.Object))
                ,tz="UTC"
                ,verbose=TRUE){

      if (missing(ticker)){
        stop(paste("[Orderops:ctpnl validation]"
            ,"missing param ticker"))
      }
      ob <- getOrderbook(.Object)
      pname <- ""

      #trade_tickers <- getActiveInstruments(ob)
      pname<-initOrderbookBlotter(ob,tickers=ticker)
      xtsdata <- getCdatedata( getMarketdata(.Object)  ,iname=ticker, tfvalue=tf)

      #browser()
      ##call of function from Orderbook

      blotterPnl(ob, ticker=ticker, xdata=xtsdata, sdate=sdate, edate=edate, npf=2,chart=TRUE, init=FALSE, tz=tz, verbose=verbose)

      return(invisible())
    })



##### chart pnl for 1 ticket in portfolio. expects watchlist current date to be at the end 
## of portfolio evaluation period
 #' @export
 #' @docType methods
setGeneric(name='ctotperf'
       ,function(.Object, tickers, tf='d1'
                ,sdate=.Object@orderbook@pfpars$iniDate
                ,edate=getCdate(getMarketdata(.Object))
                ,tz="UTC"
                ,verbose=TRUE) {standardGeneric('ctotperf')})
#' advance data to next bar and check order fills for all instruments
#'
#' @return void. Wwatchlist instance is unmodified
#' @aliases ctotperf,Wwatchlist,ANY-method
setMethod('ctotperf','Wwatchlist'
    ,function(.Object, tickers, tf='d1'
                ,sdate=.Object@orderbook@pfpars$iniDate
                ,edate= getCdate(getMarketdata(.Object))
                ,tz="UTC"
                ,verbose=TRUE){

      ob <- getOrderbook(.Object)
      if (missing(tickers)){
        tickers <- getActiveInstruments(ob)
      }


      # assign current date in Mldata to edate
      mldata <- getMarketdata(.Object)
      setCdate(mldata) <- edate
      setMarketdata(.Object) <- mldata
      
      ctz <- Sys.getenv("TZ")
      Sys.setenv(TZ=tz)
    
      #blotter portfolio name
      portfname <- ob@pfpars$name
      portf.st <- portfname
      acct.st <- portfname

      #reinitialize blotter following the call .blotter<-new.env()
      trade_tickers <- getActiveInstruments(ob)
      pname<-initOrderbookBlotter(ob,tickers=trade_tickers)

      npf<-1 # depth of call stack to assign xts to ticker names
      d1fmt <- "%Y-%m-%d"
      s1fmt <- "%Y-%m-%d %H:%M:%S"

      #main loop over actively traded tickers
      for(i in 1:length(tickers)){
        ticker <- tickers[i]

        xtsdata <- getCdatedata( mldata  ,iname=ticker, tfvalue=tf)
        assign(ticker, xtsdata, envir=parent.frame(n=npf))

        contrMult <- getInstrmetinfo(ob, ticker=ticker, mtype="contrsize")
        rndPrec <- 4
        #get filled orders
         forecs <- getOrdersSubset(ob, ticker=ticker, sdate=sdate, edate=edate, ocrit='fall')
        if ( is.null(forecs)  || nrow(forecs) == 0) { next } #goto next ticker if no open trades

        # iterate through orders recs
        for( j in 1:nrow(forecs)){
          crec <- forecs[j,]
          #browser()
          #xtranDate <- if(tf=='d1') strftime(crec$chgdate,format=d1fmt) else strftime(crec$chgdate,format=s1fmt) #!szi
          #xtranDate=base::format(adjEODmins(crec$chgdate,tz), format=s1fmt ) 
          addTxn(portfname
            ,Symbol=ticker
            ,TxnDate=base::format(adjEODmins(crec$chgdate,tz), format=s1fmt ) 
            ,TxnQty=crec$qty
            ,TxnPrice=round( crec$fprice, digits=rndPrec )
            ,TxnFees=0
            ,ConMult <- contrMult
            ,verbose=verbose  )
        }
      } #loop on tickers end

      ### update nav on every day
      dtes <- paste(sdate,'::',edate,sep='')
      updatePortf(portf.st, Dates = index(xtsdata[dtes]) )
      updateAcct(acct.st, Dates = index(xtsdata[dtes]) )
      updateEndEq(acct.st, Dates = index(xtsdata[dtes]) )

      #browser()
      ret1 <- PortfReturns(portf.st)
      ret1$total <- rowSums(ret1)
       
      charts.PerformanceSummary(ret1$total,geometric=FALSE,wealth.index=TRUE)

      eq <-getEndEq(portfname, last(index(xtsdata[dtes] )) )   
      Sys.setenv(TZ=ctz)
      #browser()
      return(invisible())
    })


##### loop data over bars, with order fills checks


 #' @export
 #' @docType methods
setGeneric(name='lpd'
       ,function(.Object,tf='d1', dtype="ofill", gfirst=FALSE, wt=5, mwt=1.0, osunix=TRUE, verbose=TRUE){standardGeneric('lpd')})
setMethod('lpd','Wwatchlist'
    ,function(.Object,tf='d1',dtype="ofill", gfirst=FALSE, wt=5, mwt=1.0, osunix=TRUE, verbose=TRUE){
     okvals <- c('nfill', 'ofill')
      if( !(dtype %in% okvals) ){
        sokvals <- paste(okvals,collapse=" ")
        stop(paste("[Watchlist:lpd validation]"
          ,"supplied dtype param not in the list of authorized values", sokvals ))
      }    
  niter <- floor(wt / mwt)    
  nameObject <- deparse(substitute(.Object))
  while(1){ #infinite loop
   cntdn <- wt
   ### biz content
   s<-Sys.time()
   if(dtype == 'nfill'){
        gnd(.Object, f=gfirst)
        cc(.Object,tf=tf,npf=2) # charting
   }else if(dtype == 'ofill'){
       gnf(.Object, tf=tf, single=TRUE, verbose=verbose)
       cc(.Object, tf=tf,npf=2) # charting
   }else{    
       gnf(.Object, tf=tf, single=FALSE, verbose=verbose)
       cc(.Object, tf=tf,npf=2) ## charting here may change
   }
   e<-Sys.time()
  #### biz content end
      nsleep <- floor(wt-as.numeric(e-s))
    cat("next iteration in ", nsleep, " seconds\n")
    if(osunix){
       cat("q to exit\n")
       osstr <- paste("read -t ",nsleep, " -n 1", sep="")
       r <- system(osstr)       
       if(r == 0) { break} 
    }else{
       cat("CTRL-C to exit\n")
       Sys.sleep(nsleep)
    }
  }#inf loop
  #save object
  persist(.Object)
  persistMdata(.Object)
  assign(nameObject,.Object,envir=parent.frame())
  return(invisible())
})

 #' @export
 #' @docType methods
setGeneric(name='lpm'
       ,function(.Object, tfs=T,ncol=2,nof=F, dtype='ofill', wt=8, mwt=1.0, osunix=TRUE, verbose=TRUE){standardGeneric('lpm')})
setMethod('lpm','Wwatchlist'
    ,function(.Object, tfs=T,ncol=2,nof=F, dtype='ofill', wt=8, mwt=1.0, osunix=TRUE, verbose=TRUE){
     okvals <- c('nfill', 'ofill')
      if( !(dtype %in% okvals) ){
        sokvals <- paste(okvals,collapse=" ")
        stop(paste("[Watchlist:lpm validation]"
          ,"supplied dtype param not in the list of authorized values", sokvals ))
      }
  tf <- "d1" #operate on daily framework    
  nameObject <- deparse(substitute(.Object))
  while(1){ #infinite loop
   cntdn <- wt
   ### biz content
   s<-Sys.time()
    if(dtype == 'nfill'){
        gnd(.Object, f=FALSE) #go next day
    }else{
        gnf(.Object, tf=tf, single=FALSE, verbose=verbose) #go next day and check fills
    }
    #charting
    if(tfs){
         ccs(.Object,tf=tf,ncol=ncol,nof=nof,npf=3)
    }else{
        ccm(.Object,nof=nof,npf=3) # charting
    }      
    
   e<-Sys.time()
  #### biz content end
    nsleep <- floor(wt-as.numeric(e-s))
    cat("next iteration in ", nsleep, " seconds\n")
    if(osunix){
       cat("q to exit\n")
       osstr <- paste("read -t ",nsleep, " -n 1", sep="")
       r <- system(osstr)       
       if(r == 0) { break} 
    }else{
       cat("CTRL-C to exit\n")
       Sys.sleep(nsleep)
    }
  }#inf loop
  #save object
  persist(.Object)
  persistMdata(.Object)
  assign(nameObject,.Object,envir=parent.frame())
  return(invisible())
})


##### calculate porftolio Nav

 
 #' @docType methods
setGeneric(name='calcPortfolioNav'
       ,function(object, sdate, tf='d1'  ){standardGeneric('calcPortfolioNav')})
#' print portfolio numbers
#'
#' @param Wwatchlist instance
#' @param sdate= date a date from which to print portfolio numbers
#' @return void. Wwatchlist instance is unmodified
#' @aliases calcPortfolioNav,Wwatchlist,ANY-method
setMethod('calcPortfolioNav','Wwatchlist'
 ,function(object , sdate, tf='d1' ){

   ob <- getOrderbook(object)
   if(missing(sdate)){
        sdate <- ob@pfpars$iniDate
   }
   pnav <- ob@pfpars$iniEquity    
   instrs <- getActiveInstruments(ob)  ##on on active instruments
   for(j in 1:length(instrs)){
     ctkr <- instrs[j]
     cedate <- getMarketdatalastdate(object, tkr=ctkr, tf=tf)
     cmmprice <- getMarketdatalastbar(object, tkr=ctkr, tf=tf)$CL
     cnav <-  calcInstrumentPnl(ob, ticker=ctkr, mmprice=cmmprice, sdate=sdate, edate=cedate, ptype="both")
     pnav <- pnav + cnav
   }
   return(pnav)
 })

############################################
############# helper functions, non public
#############################################
 #' @docType methods
setGeneric(name='getMarketdataserie',function(object, tkr, tf){standardGeneric('getMarketdataserie')})
#' @aliases getMarketdataserie,Wwatchlist,ANY-method
setMethod('getMarketdataserie','Wwatchlist'
          ,function(object, tkr, tf){
      cinstr <- ""
      if(missing(tkr)){      
       cstory <- getCurstory(object)
       cinstr <- getSinstrument(cstory)
      }else{
        cinstr <- tkr
      }
      ctf <- ""
      if(missing(tf)) { 
        ctf  <- getCurtimeframe(object) 
      }
      else  {
        tfokvals <- c('w1', 'd1', 'h1', 'h2','h4','m30', 'm15', 'm10', 'm5', 'm3', 'm1')
        if( !(tf %in% tfokvals)){
           stop(paste("[Wwatchlist:getMarketdataserie validation]"
                     ,"supplied param for tf is not of expected value"))
        }
        ctf <- tf
      }
      cdata <- getCdatedata(object@mdata, cinstr, ctf)
      return(cdata)
})

 #' @docType methods
setGeneric(name='getCurinstr',function(object){standardGeneric('getCurinstr')})
#' @aliases getCurinstr,Wwatchlist,ANY-method
setMethod('getCurinstr','Wwatchlist'
          ,function(object){
       cstory <- getCurstory(object)
       cinstr <- getSinstrument(cstory)
       return(cinstr)
})


#' @docType methods
setGeneric(name='getMarketdatalastdate',function(object, tkr, tf,n=0){standardGeneric('getMarketdatalastdate')})
#' @aliases getMarketdatalastdate,Wwatchlist,ANY-method
setMethod('getMarketdatalastdate','Wwatchlist'
          ,function(object, tkr, tf,n=0){
           cda <-  getMarketdataserie(object, tkr=tkr, tf=tf)
           cdae <- last(cda,n+1)
           
           outPosixDt <- index(first(cdae))
           retPosixDt <- chkDtModEodelta(cda,outPosixDt,dfreq=0) # POSIXct with 23:59:00 from internals.R
           return(retPosixDt)
})

 #' @docType methods
setGeneric(name='getMarketdatalastbar',function(object, tkr, tf, n=0, hasVol=FALSE){standardGeneric('getMarketdatalastbar')})
#' @aliases getMarketdatalastbar,Wwatchlist,ANY-method
setMethod('getMarketdatalastbar','Wwatchlist'
          ,function(object, tkr, tf, n=0, hasVol=FALSE){
           cda <-  getMarketdataserie(object, tkr=tkr, tf=tf)
           cdae <- last(cda,n+1)
           bcl <- as.numeric(Cl(first(cdae)))
           bhi <- as.numeric(Hi(first(cdae)))
           blo <- as.numeric(Lo(first(cdae)))
           bop <- as.numeric(Op(first(cdae)))
           outl <- list(OP=bop, HI=bhi,LO=blo,CL=bcl)
           if(hasVol) { 
              bvo <- as.numeric(Vo(first(cdae)))
              outl$VO <-bvo
           }
           return(outl)
})




#' help on trades management functions
#'
 #' @export
 #' @docType methods
setGeneric(name='trhelp',function(.Object){standardGeneric('trhelp')})
#' @aliases trhelp,Wwatchlist,ANY-method
setMethod('trhelp','Wwatchlist'
          ,function(.Object){
            cat('######## Trades / order management #######\n')
            cat('tdck(o, cbl=F, cbs=F, arby=F,pent=0, pstop=0, ft=0, mt=0) ::\t  check for a trade reward risk potential\n')
             cat('\t\tcbl=T, cbs=T use last bar high low for entry and stop levels\n')
             cat('\t\tarby=T  allow arbitrary price selection in the zoom\n')
             cat('\t\tpent=n, pstop=m, ft=x, mt=y  specify numeric values for price entry, stop, first target and main target\n')
            cat('\n========== Trades operations ===========\n')
            cat('tdpl(o,cmnt="", qty=0, mkto=F, cmnt="", plstop=TRUE, verbose=T) ::\t open trade, mkto=T - open on market order else on limit order comment - trade comment \n') 
             cat('\t\tcmnt="" - add comment to trade, if qty=0  - quantity is determinted from temp trade\n')
             cat('\t\tif plstop=TRUE  then in case of market order a stop limit order is auto placed\n')
            cat('\n===@@@@ Trade admin ops @@@===\n') 
            cat('tdrm(o, tid=0) ::\t  administrative erase of trade record specified by tid\n') 
            cat('tdca(o, tid=0) ::\t  cancel trade and its order(s). Works only if a trade has working limit orders and no open position\n')

            cat('\n========== Order management ===========\n')
            cat('tdeqty(o, tid=0, verbose=T) ::\t adjust qty of working worders to match a trade open position\n')
            cat('odwca(o,oid=0,verbose=T) :\t cancel limit working order specified by oid\n')
            cat('tdastop(o, tid=0, qty=0, pr=0,verbose=T) ::\t add stop limit order to trade tid\n')
            cat('tdatarg(o, tid=0, qty=0, pr=0,verbose=T) ::\t add target limit order to trade tid\n')
            cat('tdamkt(o, tid=0, qty=0 ,verbose=T) ::\t add arbitrary market order to trade tid\n')
            cat('tdalim(o, tid=0, qty=0, pr=0,verbose=T) ::\t add arbitrary limit order to trade tid\n')
            cat('odwmp(o, oid=0, pr=0, verbose=T) ::\t modify working limit order price\n') 
            cat('\t\t if pr=0  price will be selected interactively \n')
            cat('odwmq(o, oid=0, qty=0, verbose=T) ::\t modify working limit order  qty\n') 
            cat('odw2m(o, oid=0, verbose=T) ::\t transform working limit order  to market order\n') 

            cat('\n===@@@@ Order admin ops @@@===\n')
            cat('odrm(o, oid=0) ::\t  administrative erase of order record specified by oid\n') 
            cat('odadd(o, tid=0, type="l", qty=0, pr=0, n=0, verbose=T) ::\t administrative add limit or market order on a bar specified interactively \n')

            cat('\n========== Print trades ===========\n')
            cat('tdp(o,sdate, n=-1, al=F, otn=F, ct=F, co=F) ::\t print trades  for current instument\n')
             cat('\t\t if al=T - for all instruments \n')
             cat('\t\t if sdate  is specified - print trades between sdate and last data date \n')
            cat('prfp(o,sdate, tf="d1") ::\t print portfolio nav, equity and available cash \n')  
            cat('tdpt(o) ::\t print temp trade params \n')

            cat('\n========== Order fills and market data management  ===========\n')
            cat('cfill(o,verbose=TRUE) ::\t check working order(s) fills for a current instrument at current date \n')
            cat('gnf(o, tf="d1", single=F, verbose=TRUE) ::\t go to next bar and check working order fills for all instrument list \n')
            cat('lpd(o, tf="d1", dtype="ofill", gfirst=F, wt=3.0, mwt=1.0, unix=T, verbose=T) ::\t loop over data, use Ctrl-C to exit\n')
            cat('lpm(o, tfs=T, ncol=2,nof=F,dtype="ofill", wt=8, mwt=1.0, unix=T, verbose=T) ::\t loop over data on several instruments\n')
            cat('\t\t  instruments are from wl@orderdata@pfpars@watchinst, dtype="pfill" - check fills, "nfill" - no fills \n')
            cat('\t ?? afer exit use  wl <- rwl(wl) to reload watchlist from disk \n')
            
            cat('\n========== Order charting ===========\n')
            cat('co(o,tf,nbp=1,scf=1)  ::\t chart filled and working orders for current instrument on daily chart\n')
            cat('itco(o,dfreq,lbl,npf=1,scf=1)  ::\t chart filled and working orders for current instrument on intraday or daily chart\n')
            cat('ctpnl(o,tkr,tf="d1", sdate, edate)  ::\t chart Pnl  and drawdown for ONE ticker between sdate and edate\n')
            cat('ctotperf(o,tickers,tf="d1", sdate, edate)  ::\t chart total performnce and drawdown for list of tickers between sdate and edate\n')

})

#working on intraday
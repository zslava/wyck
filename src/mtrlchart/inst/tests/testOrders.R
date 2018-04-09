# TODO: Add comment
#
# Author: zimine
###############################################################################
doAll <- FALSE


context("init trade conf portfolio")

doNext <- TRUE
if(doNext || doAll){
test_that("init Orderbook " , {


     instr     <- c('GSPC', 'XOM', 'CL','EC')
     itype     <- c('ix', 'eq', 'fut', 'fut')
     icurr     <- c("USD","USD", "USD", "USD")
     itksize   <- c(0.25, 0.01, 0.01, 0.0001)
     itkval    <- c(12.5, 0.01, 10, 12.5)
     unitfee   <- c(2.15, 0.002, 2.50, 2.6)
     imargin   <- c(1, 1, 0.08, 0.08) 
     slipp     <-c(1, 1, 1, 1)
     rweight   <-c(1, 1, 1, 1)
     portf <- "mportf"
     startcapital <- 100000
     iniDate <- "1995-01-01"
     risktr <- 0.01
     acc <- 100
     strat <- 1
     activeinstrs <- c("XOM", "CL", "EC")

     ppars <- initPorfolioParams(accid=acc, stratid=strat, pname=portf
                                ,iniDate=iniDate, iniEquity=startcapital
                                ,risktradepct=risktr, actinstrs=activeinstrs )

     ob <- initOrderbook(ppars
                        ,instr,itype,icurr,itksize,itkval,unitfee,imargin,slipp, rweight)

     try(rm("account.mportf" , "portfolio.mportf",pos=.blotter),silent=TRUE)
     initOrderbookBlotter(ob, tickers=c("XOM") ) ##blotter part
     #browser()
     expect_equal( nrow(ob@ometadata) , 4 )
     ##test FinancialInstrument
     expect_output ( getInstrument("USD")$type, "currency" )
     expect_output ( getInstrument("CL")$type, "future" )

    
    #browser()
    
})}

doNext <- FALSE
if(doNext || doAll ){
test_that("init Orderbook from csv, manage orders" , {

     cfgfname <- "~/googledrive/cdat/order/test_tradebook_cfg.csv"
     portf <- "mportf"
     startcapital <- 100000
     iniDate <- "1995-01-01"
     risktr <- 0.01
     acc <- 100
     strat <- 1   
     activeinstrs <- c("CL", "EC")  
     ppars <- initPorfolioParams(accid=acc, stratid=strat, pname=portf, iniDate=iniDate
                                ,iniEquity=startcapital
                                ,risktradepct=risktr, actinstrs=activeinstrs)
     ob <- initOrderbookcsv(ppars,cfgfname)
     #try(rm("account.mportf" , "portfolio.mportf",pos=.blotter),silent=TRUE)
     #initOrderbookBlotter(ob) ##blotter part

     expect_equal( nrow(ob@ometadata) , 4 )
     ##test FinancialInstrument
     expect_output ( getInstrument("USD")$type, "currency" )
     expect_output ( getInstrument("CL")$type, "future" )
     #browser()

})}





context("Order object")
doNext <- TRUE
if(doNext || doAll){
test_that("Order props " , {

 lo <- mkLimitOrder(1,10, "ES", "2012-12-25 10:14:00", -10, 1134.25)

 expect_equal(lo@rprice, 1134.25)
 expect_equal(lo@inceptDate, as.POSIXct("2012-12-25 10:14:00"))
 expect_equal(lo@status, "w")


lm <- mkMarketOrder(1,10, "ES", "2012-12-25 10:15:00", -10, 1134.50)

 expect_equal(lm@fprice, 1134.50)
 expect_equal(lm@type, "m")
 expect_equal(lm@status, "f")
 expect_equal(lm@fprice, 1134.50)


 ## test order for a fill
  load('~/googledrive/cdat/wldata.rdat')
  d <- iwld$icoll[[2]]$cs$daily$data
  ##check for data in 2012
  lo <- mkLimitOrder(1,10, "CL", "2012-08-08 23:59:59", 1, 95)

   cdate <- "2012-08-09 23:59:59"
   sd <- d[paste(getIncDate(lo),"::",cdate,sep="")]
   checkFillOrder(lo, sd, cdate,  0.01) 

   cdate <- "2012-08-10 23:59:59"
   sd <- d[paste(getIncDate(lo),"::",cdate,sep="")]
   checkFillOrder(lo, sd, cdate,  0.01) 


   cdate <- "2012-08-17 23:59:59"
   sd <- d[paste(getIncDate(lo),"::",cdate,sep="")]   
  checkFillOrder(lo, sd, cdate, 0.01) 

  ## check gap fill order
  lo <- mkLimitOrder(1,10, "CL","2012-02-15 23:59:59", -1, 104.55 )
   cdate <- "2012-02-17 23:59:59"
   sd <- d[paste(getIncDate(lo),"::",cdate,sep="")]   
   checkFillOrder(lo, sd, cdate, 0.01) 

   cdate <- "2012-02-21 23:59:59"
   sd <- d[paste(getIncDate(lo),"::",cdate,sep="")]   
   checkFillOrder(lo, sd, cdate, 0.01) 

   #browser()
 
})}

context("Trade object")
doNext <- TRUE
if(doNext || doAll){
test_that("Trade props " , {


trd <- mkTrade(strat=2, acc=10, 'ES', "2012-12-20 23:59:59", -2, 1410.25, 1409, 1420, 1430)


expect_equal(getTid(trd), 0)
expect_equal(getTstatus(trd), 'n')
expect_equal(getTincDate(trd), as.POSIXct("2012-12-20 23:59:59") )

setTid(trd) <- 1
expect_equal(getTid(trd), 1)

o1 <- mkMarketOrder(1, 10,"ES", "2012-12-25 10:15:00", 10, 1134.50)
o2 <- mkMarketOrder(1, 10,"ES", "2012-12-25 10:16:00", 5, 1134.75)

setId(o1) <- 1
setId(o2) <- 2


setTradeOrder(trd)<- o1
setTradeOrder(trd) <- o2
expect_equal(length(trd@torders), 2)

expect_true( hasOrdersattached(trd))
atoids <- getOrdersattachedIds(trd)


#browser()

})}


context("Orderbook object")
doNext <- TRUE
if(doNext || doAll){
test_that(" orders in Orderbook " , {

ob <- new("Orderbook")

## 1. insert some orders

o1 <- mkLimitOrder(1, 10, "ES", "2012-12-25 10:14:00", -10, 1134.25)
o2 <- mkMarketOrder(1, 10,"ES", "2012-12-25 10:15:00", 10, 1134.50, 0.25)
o3 <- mkMarketOrder(1, 10,"ES", "2012-12-25 10:16:00", 5, 1134.75, 0.25)
o4 <- mkLimitOrder(2, 10,"CL", "2012-12-25 10:20:00", 1, 90.03)

insOrderRec(ob,o1)
insOrderRec(ob,o2)
insOrderRec(ob,o3)
insOrderRec(ob,o4)


expect_equal(ncol(ob@odf), 11)
expect_equal(nrow(ob@odf), 4)

rmOrderRec(ob, 2)

expect_false(is.na(match(1,ob@odf$id)) )
expect_true(is.na(match(2,ob@odf$id)) )

##testing change order status
fmt <- "%Y-%m-%d %H:%M:%S"
##cancel Order
#co <-  crOrderFromrec( getOrderRec(ob, id=1))
co <- getOrderFromRec(ob, id=1)

cancelOrder(co, "2012-12-25 10:19:20")
updOrderRec(ob,co)
cco <- getOrderFromRec(ob, id=1)

#browser()
expect_equal( strftime( getChgDate(cco), format=fmt), "2012-12-25 10:19:20")
expect_equal(getFillprice(cco), 0)


##filling order
co <- getOrderFromRec(ob, id=4)

fillOrder(co, chdate="2012-12-25 10:22:15", fillprice=90.05)
updOrderRec(ob,co)

cco <- getOrderFromRec(ob, id=4)

expect_equal( strftime( getChgDate(cco), format=fmt), "2012-12-25 10:22:15")
expect_equal(getStatus(cco), "f")
expect_equal(getFillprice(cco), 90.05)

rmOrderRec(ob,4)
rmOrderRec(ob,3)
rmOrderRec(ob,1)


##2. insert a  trade

tr1 <- mkTrade(strat=2, acc=10, 'ES', "2012-12-20 23:59:59", 2, 1410.25, 1409, 1420, 1430)


#add 2 orders to this trade, filled market order and limit stop order
co <- mkMarketOrder(getTid(tr1), getTaccount(tr1), getTticker(tr1)
                   ,getTincDate(tr1), getTqty(tr1), getEntry(tr1), 0.25 )


setTradeOrder(tr1)<-co
co <- mkLimitOrder( getTid(tr1), getTaccount(tr1), getTticker(tr1)
                   ,getTincDate(tr1), -1*getTqty(tr1), getStop(tr1) ) 
setTradeOrder(tr1)<-co

setTstatus(tr1) <- 'y'

expect_equal(nrow(ob@tdf), 1)

#browser()

insTradeRec(ob, tr1)

#browser()

ctr1 <- getTradeFromRec(ob, tid=1)
expect_true( is(ctr1, 'Trade'))
expect_true( hasOrdersattached(ctr1))
expect_equal( length( getTorders(ctr1)),2)
expect_equal( getTstatus(ctr1), 'y')

## update order (fill it), updateTradeRec
cord <- getTradeOrder(ctr1, oid=6)
expect_equal(getType(cord),'l')
fillOrder(cord, "2012-12-21 23:59:59", fillprice=1410.50)
setTradeOrder(ctr1)<- cord
setTstatus(ctr1)<-'n'
updTradeRec(ob,ctr1)


## rm trade and its orders (admin ops)
rmTradeRec(ob, getTid(ctr1))
expect_equal(nrow(ob@tdf),0)

##test to reinsert trade
ntr <- mkTrade(strat=2, acc=10, 'ES', "2012-12-20 23:59:59", 2, 1410.25, 1409, 1420, 1430)
setComment(ntr) <- "SOS + long term trend up"
#add 2 orders to this trade, filled market order and limit stop order
co <- mkMarketOrder(getTid(ntr), getTaccount(ntr), getTticker(ntr)
                   ,getTincDate(ntr), getTqty(ntr), getEntry(ntr), 0.25 )
setTradeOrder(ntr)<-co
co <- mkLimitOrder( getTid(ntr), getTaccount(ntr), getTticker(ntr)
                   ,getTincDate(ntr), -1*getTqty(ntr), getStop(ntr)) 
setTradeOrder(ntr)<-co
setTstatus(ntr) <- 'y'

cc <- mkLimitOrder( getTid(ntr), getTaccount(ntr), getTticker(ntr)
                   ,getTincDate(ntr), -2*getTqty(ntr), getStop(ntr))

cancelOrder(cc, "2012-12-21 23:59:59")
setTradeOrder(ntr) <- cc

insTradeRec(ob, ntr)


#cat("\n")
##printTrade(ntr, mmprice=0, co=TRUE)
#printTrade(ntr, mmprice=0, co=FALSE)

#browser() 
})}


context("Orderbook user biz methods")
doNext <- TRUE
if(doNext || doAll){
test_that(" user ops in Orderbook " , {

## configure metadata
     cfgfname <- "~/googledrive/cdat/order/test_tradebook_cfg.csv"
     portf <- "mportf"
     startcapital <- 100000
     iniDate <- "1995-01-01"
     risktr <- 0.01
     acc <- 100
     strat <- 1      
     activeinstrs <- c("CL", "EC")
     ppars <- initPorfolioParams(accid=acc, stratid=strat, pname=portf, iniDate=iniDate
                                ,iniEquity=startcapital
                                ,risktradepct=risktr,actinstrs=activeinstrs)
     ob <- initOrderbookcsv(ppars,cfgfname)

     #browser()
     ## 1. check for potential trade
     check4trade(ob, 'ES', '2012-12-19', 1400, 1398.25, 1410, 1420, verbose=T)
     expect_equal( getEntry(ob@tmptrade),1400)

     ## 2.1  open Market  trade
     ## test openTrade NOK on uninitialized check4trade OK

     openTrade(ob,'m', '2012-12-20 23:59:59', 10, 1400, "trend up + SOS", verbose=F )
     ## 2.2  open Limit   trade
     check4trade(ob, 'CL', '2011-12-19', 90.05, 89.10, 91, 95, verbose=F)
     openTrade(ob,'l', '2012-12-20 23:59:58', 1, 90.04 , verbose=F)
     expect_equal(nrow(ob@tdf),2)
     expect_equal(nrow(ob@odf),3)

    ##2 cancel trade with tid =2
    cancelTrade(ob, tid=2, '2012-12-21 23:59:59')
    ## can not cancel trade with id=1  as it has >0 exposure, as expected
    #cancelTrade(ob, id=1, '2012-12-21 23:59:59')
    expect_equal( getTstatus( getTradeFromRec(ob, tid=2)) ,'c' )


    #3.  eraseTrade for admin reasons
     check4trade(ob, 'CL', '2012-12-19', 90.95, 89.15, 91, 95, verbose=F)
     openTrade(ob,'m', '2012-12-22 00:00:00', 1, 90.04 , verbose=F)
          
     eraseTrade(ob, tid=3)
     expect_equal(nrow(ob@tdf),2)

    
 
    #4. test addTradeOrder
     check4trade(ob, 'CL', '2012-12-19', 90.99, 89.20, 91, 95, verbose=F)  
     openTrade(ob,'l', '2012-12-23 23:59:59', 1, 90.99 , verbose=F ) #tid=4
     addTradeOrder(ob, tid=4, "2012-12-24 23:59:59", "m", 1, 91.10)
     addTradeOrder(ob, tid=4, "2012-12-24 23:59:59", "l", 1, 91.20)
     tr <- getTradeFromRec(ob, tid=4)
     expect_equal( length(tr@torders),3 )


     #5. test for updInstrumentOrderFills
     load('~/googledrive/cdat/wldata.rdat')
     d <- iwld$icoll[[2]]$cs$daily$data

     check4trade(ob, 'CL', '2012-12-19', 93,  91, 95, 97, verbose=F)
     openTrade(ob,'l', '2012-08-03 00:00:00', 1 ,93, "trade 1" , verbose=F)
     openTrade(ob,'l', '2012-08-06 00:00:00', 2 ,94.5, "trade 2", verbose=F )
     cdt <- '2012-08-06 23:59:59'
     sd <- d[paste('::',cdt,sep="")]
  
     #browser()
   
     procOrderFills(ob, 'CL', cdt, sd)  ## no fills
     #
     cdt <- '2012-08-07 23:59:59'
     sd <- d[paste('::',cdt, sep="")]
     procOrderFills(ob, 'CL', cdt, sd, verbose=T)  ## no fills

     
     cdt <- '2012-08-08 23:59:59'
     sd <- d[paste('::',cdt,sep="")]
     procOrderFills(ob, 'CL', cdt, sd, verbose=T)  ## no fills
     expect_equal( calcInstrumentPosition(ob, 'CL', edate=cdt)$gpos, 3 )
     
  


     cdt <- '2012-08-09 23:59:59'
     addTradeOrder(ob, tid=6, cdt, "l", -2, 95.5) #set target

     cdt <- '2012-08-10 23:59:59'
     sd <- d[paste('::',cdt,sep="")]
     procOrderFills(ob, 'CL', cdt, sd, verbose=T)  ## no fills
     expect_equal( calcInstrumentPosition(ob, 'CL', edate=cdt)$gpos, 3 )
     
     #browser()

     cdt <- '2012-08-16 23:59:59'
     sd <- d[paste('::',cdt,sep="")]
     procOrderFills(ob, 'CL', cdt, sd, verbose=T)  ## no fills
     expect_equal( calcInstrumentPosition(ob, 'CL', edate=cdt)$gpos, 1 )
     

     ##6 test printTrades
     ##printTrades(ob, 'CL',edate='2012-12-24', n=2, ct=F)
     printTrades(ob, 'CL',edate='2012-12-24', n=-1, ct=F)


    #5 test updTradeOrder
    #5.1  NOK cancel order
     updTradeOrder(ob, oid=7, '2012-12-26 23:59:59', param='cancel') # OK
     expect_false( getStatus( getOrderFromRec(ob, id=7)) == 'c')

     #5.2 ok cancel order
     check4trade(ob, 'CL', '2011-12-19', 91.01, 89.87, 94, 99, verbose=F)  
     openTrade(ob,'l', '2012-12-25 23:59:59', 1, 91.01 , verbose=F ) #tid=7, oid=12
     updTradeOrder(ob, oid=12, '2012-12-26 23:59:59', param='cancel', verbose=F) # OK
     expect_equal( getStatus( getOrderFromRec(ob, id=12)), 'c')
     expect_equal( getTstatus( getTradeFromRec(ob, tid=7) ), 'c')
     
     #5.3 OK tomarket order    
     openTrade(ob,'l', '2012-12-25 23:59:59', 1, 91.01 , verbose=F ) #tid=8, oid=13

      
     updTradeOrder(ob, oid=13, '2012-12-26 23:59:59', param='tomarket'
                      , value=91.10, verbose=TRUE) # OK

    #browser()
     

     expect_equal( getStatus( getOrderFromRec(ob, id=14)), 'f')
     expect_equal( getType( getOrderFromRec(ob, id=14)), 'm')
    


     #5.4 OK unfill market order
     updTradeOrder(ob, oid=14, '2012-12-26 23:59:59', param='unfill'
                      , verbose=TRUE) # OK
     expect_equal( getStatus( getOrderFromRec(ob, id=14)), 'w')
     expect_equal( getType( getOrderFromRec(ob, id=14)), 'l')
     expect_equal( getFillprice( getOrderFromRec(ob, id=14)), 0)

     #5.5  change price level #OK
     updTradeOrder(ob, oid=14, '2012-12-26 23:59:59', param='price'
                      ,value=90.99, verbose=TRUE) 
     expect_equal( getRegprice( getOrderFromRec(ob, id=14)), 90.99)

     #5.6  change qty level #OK
     updTradeOrder(ob, oid=14, '2012-12-26 23:59:59', param='qty'
                      ,value=-1, verbose=TRUE) 
     expect_equal( getQty( getOrderFromRec(ob, id=14)), -1)


    # #6 test Trades and Orders subsets
    trs <- getTradesSubset(ob, ticker='CL', crit="uncanceled")
    tids <- trs$tid  
    expect_equal( paste(tids, collapse=" "), "4 5 6 8"  ) 

    ors <- getOrdersSubset(ob, ticker='CL', tcrit='uncanceled', ocrit='fall')
    expect_equal( paste(ors$id, collapse=" "), "7 9 10 11"   )


    ors <- getOrdersSubset(ob, ticker='CL', tcrit='uncanceled', ocrit='wall')
    expect_equal( paste(ors$id, collapse=" "), "6 8 14"   )

    ors <- getOrdersSubset(ob, ticker='CL', tcrit='uncanceled', ocrit='uncanceled')
    expect_equal( paste(ors$id, collapse=" "), "6 7 8 9 10 11 14"   )


        ###test gettting meta info
    expect_equal( getInstrmetinfo(ob, "XOM", mtype="itype"), "eq" )
    expect_equal( getInstrmetinfo(ob, "CL", mtype="itype"), "fut" )

    expect_equal( getInstrmetinfo(ob, "XOM", mtype="ticksize"), 0.01 )


     # #7 erase order for admin reasons
     # addTradeOrder(ob, tid=3, "2012-12-22 00:00:00", "l", 1, 90.10)
     eraseOrder(ob, oid=14)
     expect_true( is.na(match(14, ob@odf$id)) )


    #browser()

})}

context("Orderbook in Watchlist")
doNext <- TRUE
if(doNext || doAll){
test_that(" user ops in Orderbook " , {

## configure metadata
     cfgfname <- "~/googledrive/cdat/order/gehdmcd_tradebook_cfg.csv"
     portf <- "eqportf"
     startcapital <- 100000
     iniDate <- "1991-01-01"
     risktr <- 0.01
     acc <- 10
     strat <- 1    
     activeinstrs <- c("HD", "MCD", "GE")

     ppars <- initPorfolioParams(accid=acc, stratid=strat, pname=portf, iniDate=iniDate
                                ,iniEquity=startcapital, risktradepct=risktr
                                ,actinstrs=activeinstrs )

     

     ob <- initOrderbookcsv(ppars,cfgfname)

    wl <- lwl('~/googledrive/cdat/wls/gehdmcd.rdat')

    wl@orderbook <- ob

    
})}



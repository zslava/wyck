#rm(list=ls() )
#

#source('rwyahoo.R')
#source('rwtblox.R')
#
#source('Mldata.R')




context("InstrumentList Marketdata")

doNext <- F #pmktata OK
if ( doNext){

test_that("Mldata methods" , {

   instr <- c('SP500', 'XOM','CVX', 'GE')
   tickr <- c('^GSPC', 'XOM', 'CVX', 'GE')
   itype <- c('ix', 'eq', 'eq', 'eq')
   ilbl <- c('SP 500', 'Exxon Mobile Corp', 'Chevron Corp', 'General Electric')
   igrp <- c('Index', 'Energy', 'Energy', 'Industrial')

   grpsorted <- c('Energy', 'Industrial','Index')

   startdate<-"1970-01-01"
   enddate <-"1990-01-01"
   mldh <- instrlistdata( instr, tickr, itype, ilbl,igrp, grpsorted, mode='historic'
                             ,startdate=startdate, enddate=enddate
					         ,dsrc='yahoo', dld=T)


   setCdate(mldh) <- '1970-01-01'
   expect_equal( strftime(mldh@cdate, format="%Y-%m-%d"), "1970-01-02")

   ### test fetchNextDay  method
   fetchNextDay(mldh)
   expect_equal( strftime(mldh@cdate, format="%Y-%m-%d"), "1970-01-05")
   d1 <-getCdatedata(mldh,'SP500','d1')
   w1 <-getCdatedata(mldh,"SP500","w1")
   expect_equal( as.numeric(Cl(last(d1))), as.numeric(Cl(last(w1))) )

   fetchNextDay(mldh)
   expect_equal( strftime(mldh@cdate, format="%Y-%m-%d"), "1970-01-06")

   #browser()
   })}

doNext <- T ## OK crate Mldata from contract seq list on disk
if ( doNext){

  test_that("Mldata from ContractSeq " , {

  grpsorted <- c("ix", "ir","fx","energy")

  sdt <- "2008-01-01"
  edt <- strftime(Sys.Date())

  csvfile <- "~/Dropbox/cs/osx/osxwr/mtrlchart-batch/wyck-cre/mfut_intra_tst.csv"
  csdir <-"~/googledrive/mkdata/dtn/tcontrseq"
  cslistconfigfile <- "~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg"
  #intrd <- "all"
  intrd <- "now"
  
  #mldata instance
  mktd <- instrlistdatacsv(csvfile, grpsorted,  mode='live'
                          ,startdate=sdt,enddate=edt
                          , dsrc='dtn', dld=F)
  

   loadUp2dateContracts(mktd,rollon="volume"
                  ,skipintraday=FALSE,intrd=intrd,freq=60 
                  ,cslistconf=cslistconfigfile)
 
    # mktd@idata<-nloadDataContractsDTN(mktd
    #              ,rollon="volume"
    #              ,skipintraday=FALSE, intrd="now",freq=60
    #              ,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg")$data


  tcs <- nreadcs('ES',csdir,freq=60,intrd=intrd) ## read current contract sequence from nodata_rdat
  tcl <- nreadcs('CL',csdir,freq=60,intrd=intrd)

  expect_true(length(mktd@idata) > 1  )
  expect_true(length(mktd@idata[[1]]$intraday) == length(tcs@intrdconf$intradayfreq)  )
  
  #browser()
  save(mktd, file = "/tmp/t.rdat")
 
 })}


doNext <- F ## OK pmktdata
if ( doNext){

test_that("Mldata with yahoo data" , {
   instr <- c('SP500', 'XOM')
   tickr <- c('^GSPC', 'XOM')
   itype <- c('ix', 'eq')
   ilbl <- c('SP 500', 'Exxon Mobile Corp')
   igrp <- c('Index', 'Energy')

   grpsorted <- c('Energy', 'Index')

   mldl <- instrlistdata( instr, tickr,itype,ilbl,igrp, grpsorted
				         ,mode='live',  dsrc='yahoo',  dld=T)
   lng <- nrow( mldl@idata[[1]]$d1 )
   lstdate <- index( last( mldl@idata[[1]]$d1 ))
   removeDatatail(mldl,rmd=T,n=1,tf="d1")
   #mldl@idata[[1]]$d1 <- mldl@idata[[1]]$d1[1:(lng-2)] # trick before call fetchNextDay()
   #mldl@idata[[2]]$d1 <- mldl@idata[[2]]$d1[1:(lng-2)]
   fetchNextDay(mldl)  ##calls update download data
   nlstdate <- index( last( mldl@idata[[1]]$d1 ))
   fmt <- "%Y-%m-%d"
   expect_equal( strftime(lstdate, format=fmt), strftime(nlstdate, format=fmt) )

   ##testing method which gets d1 data up to mldl@cdate
   sp <- getcd1(mldl)
   expect_equal( strftime(nlstdate, format=fmt), strftime(index(last(sp)), format=fmt) )

   removeDatatail(mldl,rmd=T,n=1,tf="d1")
   ##test add, rm market data
   #addMarketdata(mldl, 'GOOG', 'GOOG', 'eq', 'Google Inc', 'Technology')
   insertGroup(mldl,"Technology", 1)
   expect_equal(length(mldl@groupsordered), 3)
   expect_equal(mldl@groupsordered[2] , "Technology")

   addMarketdata(mldl, 'GOOG', 'GOOG', 'eq', 'Google Inc', 'Technology')
   
   
   fetchNextDay(mldl)

   
   expect_true ( nrow( mldl@idata[[3]]$d1 ) > 3 )

   rmMarketdata(mldl, 'GOOG')
   expect_equal( length(mldl@idata), 2)

   #browser()
   
})
}

doNext <- F  #pmktdata OK
if ( doNext){

test_that("Mldata with tblox data" , {

   instr <- c("ES", "ND", "ED", "TY", "EC", "AD", "BP", "CD", "JY", "MP", "SF"
             ,"CL", "HU", "HO", "NG", "GC","HG", "PL", "SI", "BO", "C",  "O"
             ,"S",  "W",  "CC", "CT", "KC", "LB", "SB", "LC", "LH")

   tickr <- c("ES", "ND", "ED", "TY", "EC", "AD", "BP", "CD", "JY", "MP", "SF"
             ,"CL", "HU", "HO", "NG", "GC","HG", "PL", "SI", "BO", "C",  "O"
             ,"S",  "W",  "CC", "CT", "KC", "LB", "SB", "LC", "LH")

  itype <-  rep('fut', length(tickr))

  ilbl <-  c("mini-S&P",        "Nasdaq 100",      "Libor 3mnth",     "T-Note 10 Yr"
            ,"EUR/USD",         "AUD/USD",         "GBP/USD",         "CAD/USD"
            ,"JPY/USD",         "MXN/USD",         "CHF/USD",         "Light Crude Oil"
            ,"RBOB Gasoline",   "Heating Oil",     "Natural Gas",     "Gold"
            ,"Copper",          "Platinum",        "Silver",          "Soybean Oil"
            ,"Corn",            "Oats",            "Soybeans",        "Wheat"
            ,"Cocoa",           "Cotton",          "Coffee",          "Lumber"
            ,"Sugar",           "Cattle",          "Hogs Lean" )

  igrp <- c("ix",     "ix",     "ir",     "ir",     "fx",     "fx",     "fx",     "fx",     "fx"
            ,"fx",     "fx",     "energy", "energy", "energy", "energy", "metal",  "metal",  "metal"
            ,"metal",  "grain",  "grain",  "grain",  "grain",  "grain",  "soft",   "soft",   "soft"
            ,"soft",   "soft",   "meat",   "meat")

   grpsorted <- c("ix", "ir", "fx","energy", "metal", "grain", "soft", "meat")

   mldl <- instrlistdata( instr, tickr, itype, ilbl,igrp, grpsorted, mode='live',dsrc='tblox',dld=T)


   expect_equal( length(mldl@idata), 31)

   expect_equal( getCinstrument(mldl), "ES")

   setInstrumentPosition(mldl, 'first')
   expect_equal( getCinstrument(mldl), "ES")

   setInstrumentPosition(mldl, 'next')
   expect_equal( getCinstrument(mldl), "ND")
   setInstrumentPosition(mldl, 'next')
   setInstrumentPosition(mldl, 'next')
   expect_equal( getCinstrument(mldl), "TY")

   setInstrumentPosition(mldl, 'previous')
   expect_equal( getCinstrument(mldl), "ED")

   setInstrumentPosition(mldl, 'last')
   expect_equal( getCinstrument(mldl), "LH")

   setInstrumentPosition(mldl, 'next')
   expect_equal( getCinstrument(mldl), "ES")

   #browser()

})}




doNext <- F #pmktdata OK
if ( doNext){

test_that("Mldata with tblox data methods" , {

   instr <- c("ES", 'CL')
   tickr <- c("ES", 'CL')
   itype <-  rep('fut', length(tickr))
   ilbl <-  c("mini-S&P", "Light Crude Oil")
   igrp <- c("ix",  "energy")

   grpsorted <- c("ix", "ir", "fx","energy", "metal", "grain", "soft", "meat")

   mldl <- instrlistdata( instr, tickr, itype, ilbl,igrp, grpsorted, mode='live',dsrc='tblox',dld=T)
   expect_equal( length(mldl@idata), 2)
   

   
   ### test fetchNextDay  method
   lng <- nrow( mldl@idata[[1]]$d1 )
   lstdate <- index( last( mldl@idata[[1]]$d1 ))
   mldl@idata[[1]]$d1 <- mldl@idata[[1]]$d1[1:(lng-2)] # trick bef call fetchNextDay()
   bklstdate <- index( last( mldl@idata[[1]]$d1 ))

   fetchNextDay(mldl)  ##calls update download data
   nlstdate <- index( last( mldl@idata[[1]]$d1 ))
   fmt <- "%Y-%m-%d"
   expect_equal( strftime(lstdate, format=fmt), strftime(nlstdate, format=fmt) )

   ##testing method which gets d1 data up to mldl@cdate
   sp <- getcd1(mldl)
   expect_equal( strftime(nlstdate, format=fmt), strftime(index(last(sp)), format=fmt) )

   #browser()
})
}
doNext <- F
if(doNext){
test_that("test checkYahooDblquoteDaily", {

    tindx <- c("2012-06-01", "2012-06-02", "2012-06-02")
    mdat <- matrix( c(1,2,3,4,100,11,12,13,14,202,21,22,23,24,200), ncol=5,byrow=T)
    tx <-xts(mdat, order.by=as.POSIXct(tindx))
    colnames(tx)<-c("Open","High", "Low","Close","Volume")

    res <- checkYahooDblquoteDaily(tx)
    expect_equal( nrow(res), 2)
    expect_equal( as.numeric(Vo(last(res))), 202)
})
}


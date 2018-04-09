#source('Contract.R')
#source('ContractSeq.R')
#source('~/Dropbox/rlab/rcw/rwdata/rwdtn.R')


##config dtn

  #library(iqfeed)
  #vmIP<-"192.168.50.129"
  #iqConf(host=vmIP, ports = list(level1=4009,historic=8100,level2=8200))

context("ContractSeq biz logic. Can be time consuming..")


doNext <- F
if (doNext){

test_that("ContractSeq  populate sequence from a config file CL", {

	cfn <- 'CL.cfg'
    cdir <- "~/googledrive/mkdata/dtn/cfgseq"
    cs <-  csfromcfg(fname=cfn,cfgdir=cdir)

    expect_true( nrow(cs@contrseq) > 1)
    expect_true( cs@rollrule[[1]]$rollprd > 1  )
    expect_true( cs@rollrule[[1]]$fmvolpct > 0.1  )
  
    iniDownload(cs, lstqtime=Sys.time(), persist=F,findvrol=F)
    findRollonvol(cs,mstore=F)
    findRollonvol(cs,mstore=T)
    expect_true( length(cs@contrseq$volroll) > 1 )
 
    persistSeq(cs)

    ## add more months to cover the present (cdate = '2013-08-07')
    addFutureContracts(cs,nmonths=6)
    updateEOD(cs, scontract='2013-06-01',ctime=Sys.time(),updMeta=TRUE,persist=FALSE)
    updateExpiries(cs) #optional , updMeta=TRUE includes this
    updateMeta(cs,persist=TRUE)
    persistSeq(cs) 

})}

doNext <- T
if (doNext){
test_that("ContractSeq  populate sequence from a config file ES", {

	cfn <- 'ES.cfg'
    cdir <- "~/googledrive/mkdata/dtn/tcfgseq"
    cs <-  csfromcfg(fname=cfn,cfgdir=cdir)

    expect_true( nrow(cs@contrseq) > 1)
    expect_true( cs@rollrule[[1]]$rollprd > 1  )
    expect_true( cs@rollrule[[1]]$fmvolpct > 0.1  )
  
    iniDownload(cs, lstqtime=Sys.time(), persist=F,findvrol=F)
    findRollonvol(cs,mstore=F)
    findRollonvol(cs,mstore=T)
    expect_true( length(cs@contrseq$volroll) > 1 )
 
    fillExpiries(cs, persist=FALSE) 
    persistSeq(cs)


    ## add more months to cover the present (cdate = '2013-08-07')
    tctime_d <- as.POSIXct("2013-08-08 18:00:05")
    
    addFutureContracts(cs,nmonths=6)
    updateEOD(cs, scontract='2013-06-01',ctime=tctime_d,updMeta=TRUE,persist=FALSE)
    updateExpiries(cs) #optional , updMeta=TRUE includes this
    updateMeta(cs,persist=TRUE)
    persistSeq(cs) 

    ##test intraday data
    tctime_intr <- as.POSIXct("2013-08-09 12:00:04")

    ##to be refactored in wrapper function
    intrval <- 3600
    cs@intrdconf <- c(3600,7200)
    #iniDownloadIntraDTN(cs, lstqtime=tctime_intr, interval=intrval
    #                   ,startContract='2013-03-01',verbose=TRUE)
    iniDownloadIntra(cs, lstqtime=tctime_intr, interval=intrval
                	,startContract='2013-03-01',verbose=TRUE,persist=FALSE)

   
    tctime_uintr <- as.POSIXct("2013-08-09 14:59:59")

    ##test append intraday
    #to be refactored in wrapper function
    updateIntraDTN(cs, lstqtime=tctime_uintr,interval=intrval,startContract=NA,verbose=TRUE)
    
    #test to convert to all periods
    allPrdIntra(cs)

    #test persisting intraday contracts data in individual rdat files
    persistIntra(cs,intrval)

    #test show intraday metadata
    show(cs)

    #
    #
    #browser()

})}


doNext <- F
if (doNext){

test_that("ContractSeq  biz findStoreHistoricExpiryDates CL", {

 datasrc <-'dtn'
iname<-'CL'
tkpref<-'QCL'
topstorage ='~/googledrive/mkdata/dtn/tcontrseq'

  yrcontr <- 1:12
  stime <- '2013-01-01'
  etime <-'2013-07-31'
  lstquote <- '2011-07-22'
  nctime <-  '2011-07-24'
  #etime <-Sys.time()
  
  yS <- stime ; yE<-etime

  ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc) 
   
  roll_window = 5
  front_vol_pct_thresh = 0.35
  setRegularRollRule(ess, rollprd=roll_window, fmvolpct=front_vol_pct_thresh)

  iniDownload(ess, lstqtime=lstquote, persist=F)

  
  expect_true( nrow(getContractSequence(ess)) > 2)
 
  updateEOD(ess,ctime=nctime, persist=F)
  #browser()
  
 })}





			

##this test should be normall run
doNext <- F
if ( doNext){
	test_that("ContractSeq   bare seqence CL", {
				
				datasrc <-'dtn'
				iname<-'CL'
				tkpref<-'QCL'
				topstorage ='~/googledrive/mkdata/dtn/contrseq'
				d1fmt = "%Y-%m-%d"
				yrcontr <- 1:12

				cctime<-Sys.time()
				yS <- '2011-01-01'
				yE<-cctime
				ss <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
				
				#browser()
				
})}

doNext <- F
if ( doNext){
	test_that("ContractSeq   getGlued  CL", {
				fn <- '~/googledrive/mkdata/dtn/contrseq/CL/rdat/CL.rdat'
				ess <- loadseq(fn)
				stime<-"2012-01-01" 
				etime<-"2012-06-10"
                gls <- getGluedsequence(ess,stime=stime,etime=etime, rollon="expiry")
				expect_equal( gls[[length(gls)]]$cfmonth, "2012-7-1" )
				etime<-"2012-08-29"
				gls <- getGluedsequence(ess,stime=stime,etime=etime, rollon="expiry")
				expect_equal( gls[[length(gls)]]$cfmonth, "2012-10-1" )
				
				#browser()
				
				##test on ES
				fn <- '~/googledrive/mkdata/dtn/contrseq/ES/rdat/ES.rdat'
				ess <- loadseq(fn)
				stime<-"2011-01-01" 
				etime<-"2012-06-01"
				gls <- getGluedsequence(ess,stime=stime,etime=etime, rollon="expiry")
				expect_equal( gls[[length(gls)]]$cfmonth, "2012-6-1" )

				etime<-"2012-06-25"
				gls <- getGluedsequence(ess,stime=stime,etime=etime, rollon="volume")
				expect_equal( gls[[length(gls)]]$cfmonth, "2012-9-1" )
				
				
				##tests to get glued series based on glued sequence
				#xde <- getGluedserie(ess,stime="2012-01-01", etime=Sys.time(), rollon='expiry' )
				#xdv <- getGluedserie(ess,stime="2012-01-01", etime=Sys.time(), rollon='volume' )
				#xdvv <- getGluedserie(ess,stime="2012-01-01", etime=Sys.time(), rollon='volume', dblvol=T )
					
})}				

####### below  all tests are false

doNext <- F
if (doNext){
	
	test_that("ContractSeq  updateEOD CL", {				
				datasrc <-'dtn'
				iname<-'CL'
				tkpref<-'QCL'
				topstorage ='~/googledrive/mkdata/dtn/contrseq'
				d1fmt = "%Y-%m-%d"
				yrcontr <- 1:12
				#cctime<-Sys.time()
				cctime <-'2012-08-16'
				
				yS <- '2012-06-01'
				yE<-cctime
				ss <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
				
				iniDownload(ss, ctime=cctime, persist=F)			
				expect_equal( strftime(ss@lastDataDaily,format=d1fmt),  cctime)
				findRollonvol(ss)
				##update   expct a detection of new roll date
				nctime <- "2012-08-20"
				updateEOD(ss,ctime=nctime, persist=F)
				expect_equal( strftime(ss@lastDataDaily,format=d1fmt),  nctime)
				#browser()
				

				##update  here a new expiry should be detected !!!
				nctime <- "2012-08-22"
				updateEOD(ss,ctime=nctime, persist=F)
				expect_equal( strftime(ss@lastDataDaily,format=d1fmt),  nctime)
                browser()
				
				##update  expect detect expiration
				nctime <- "2012-08-23"
				updateEOD(ss,ctime=nctime, persist=F)
				expect_equal( strftime(ss@lastDataDaily,format=d1fmt),  nctime)
						
				browser()
			})}

doNext <- F
if (doNext){
				
	test_that("ContractSeq  updateEOD ES", {				
					datasrc <-'dtn'
					iname<-'ES'
					tkpref<-'@ES'
					topstorage ='~/googledrive/mkdata/dtn/contrseq'
					d1fmt = "%Y-%m-%d"
					yrcontr <- c(3,6,9,12)
					#cctime<-Sys.time()
					cctime <-'2012-06-07'
					
					yS <- '2011-11-01'
					yE<-cctime
					ss <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
					
					iniDownload(ss, ctime=cctime, persist=F)			
					expect_equal( strftime(ss@lastDataDaily,format=d1fmt),  cctime)
					findRollonvol(ss)
					browser()
					##update   expct a detection of new roll date
					nctime <- "2012-06-08"
					updateEOD(ss,ctime=nctime, persist=F)
					expect_equal( strftime(ss@lastDataDaily,format=d1fmt),  nctime)
					browser()
					
					##update  here a new expiry should be detected !!!
					nctime <- "2012-06-18"
					updateEOD(ss,ctime=nctime, persist=F)
					expect_equal( strftime(ss@lastDataDaily,format=d1fmt),  nctime)
					browser()
							
							
})}
			
####### tests to check instrument contract past boundaries
doNext <- F
if (doNext){
	
	test_that("ContractSeq  biz findStoreHistoricExpiryDates ES", {
				
				
				topstorage ='~/googledrive/mkdata/dtn/contrseq'
				datasrc <-'dtn'
				
				cctime<-Sys.time()
				yE <-cctime
				
				iname<-'ES'
				tkpref<-'@ES'
				yrcontr <- c(3,6,9,12)
				yS <- '1999-06-01' ; 
				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
				iniDownload(ess, ctime=cctime)				
				
			})}
doNext <- F
if (doNext){
	
	test_that("ContractSeq  biz findStoreHistoricExpiryDates EU", {
				
				
				topstorage ='~/googledrive/mkdata/dtn/contrseq'
				datasrc <-'dtn'
				
				cctime<-Sys.time()
				yE <-cctime
				
				iname<-'6E'
				tkpref<-'@EU'
				yrcontr <- c(3,6,9,12)
				yS <- '2002-06-01' ; 
				#yS <- '2012-01-01' ; 
				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
				iniDownload(ess, ctime=cctime)		
				#browser()	
			})}

doNext <- F
if (doNext){
	
	test_that("ContractSeq  biz findStoreHistoricExpiryDates TY", {
				
				
				topstorage ='~/googledrive/mkdata/dtn/contrseq'
				datasrc <-'dtn'
				
				cctime<-Sys.time()
				yE <-cctime
				
				iname<-'TY'
				tkpref<-'@TY'
				yrcontr <- c(3,6,9,12)
				yS <- '2003-06-01' ; 
				#yS <- '2012-01-01' ; 
				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
				
				iniDownload(ess, ctime=cctime)
				#browser()
			})}

doNext <- F
if (doNext){
	
	test_that("ContractSeq  biz findStoreHistoricExpiryDates GC", {
				
				
				topstorage ='~/googledrive/mkdata/dtn/contrseq'
				datasrc <-'dtn'
				
				cctime<-Sys.time()
				yE <-cctime
				
				
				iname<-'GC'
				tkpref<-'QGC'
				yrcontr <- c(2,4,6,8,12)
				yS <- '2004-08-01' ; 
				#yS <- '2011-06-01' ; 
				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
				iniDownload(ess, ctime=cctime)
				browser()
			})}


#### ini download  for a list of instruments
doNext <- F
if (doNext){
	test_that("ContractSeq  ini load for a number of sequences, (takes some time)", {
				
				topstorage ='~/googledrive/mkdata/dtn/contrseq'
				datasrc <-'dtn'
				yE <-Sys.time()
				cctime<-Sys.time()
				
#				iname<-'EB'
#				tkpref<-'EB'
#				yrcontr <- 1:12
#				yS <- '2006-01-01' ; 
#				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
#				iniDownload(ess, ctime=cctime)
#				
#				iname<-'CL'
#				tkpref<-'QCL'
#				yrcontr <- 1:12
#				yS <- '2006-01-01' ; 
#				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
#				iniDownload(ess, ctime=cctime)
#				
#				iname<-'GO'
#				tkpref<-'EPI'
#				yrcontr <- 1:12
#				yS <- '2006-01-01' ; 
#				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
#				iniDownload(ess, ctime=cctime)
#				
#				iname<-'ES'
#				tkpref<-'@ES'
#				yrcontr <- c(3,6,9,12)
#				yS <- '1999-06-01' ; 
#				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
#				iniDownload(ess, ctime=cctime)
				
#				iname<-'6E'
#				tkpref<-'@EU'
#				yrcontr <- c(3,6,9,12)
#				yS <- '2002-06-01' ; 
#				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
#				iniDownload(ess, ctime=cctime)
				
#				iname<-'TY'
#				tkpref<-'@TY'
#				yrcontr <- c(3,6,9,12)
#				yS <- '2003-06-01' ; 
#				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
#				iniDownload(ess, ctime=cctime)
				
#				iname<-'GC'
#				tkpref<-'QGC'
#				yrcontr <- c(2,4,6,8,12)
#				yS <- '2004-06-01' ; 
#				ess <- crBareContractSeq(iname, tkpref, yrcontr,  dstart=yS, dend=yE, datasrc=datasrc,topdir=topstorage)
#				iniDownload(ess, ctime=cctime)				

				
			})}



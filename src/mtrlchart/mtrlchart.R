### R code from vignette source 'inst/doc/mtrlchart.Rnw'

###################################################
### code chunk number 1: l
###################################################
require(Defaults, lib.loc='~/R/library')
require(TTR, lib.loc='~/R/library')
require(quantmod, lib.loc='~/R/library')
require(iqfeed, lib.loc='~/R/library')
require(logging, lib.loc='~/R/library')

require(FinancialInstrument, lib.loc='~/R/library')
require(PerformanceAnalytics, lib.loc='~/R/library')
require(blotter, lib.loc='~/R/library')
require(pmktdata, lib.loc='~/R/library')


###################################################
### code chunk number 2: l2
###################################################
##loading this package
require(mtrlchart, lib.loc='~/R/library')
## loading  the watchlist rdat file
wl <- lwl('~/googledrive/cdat/wls/geaa_rnw.rdat')


###################################################
### code chunk number 3: hlp
###################################################
ohelp(wl)


###################################################
### code chunk number 4: pri
###################################################
wl


###################################################
### code chunk number 5: modes
###################################################
##switch to live mode
dmode(wl, mode='live', startdate='1980-01-01', enddate=Sys.date())
##print current instrument last data
pd(wl,n=1)
##switch to historic mode
dmode(wl, mode='historic', startdate='1980-01-01', enddate=Sys.Date() )
##go to specific date in the past
dcdate(wl,date='1985-01-01')
pd(wl)


###################################################
### code chunk number 6: gnd
###################################################
gnd(wl)
pd(wl,n=1)


###################################################
### code chunk number 7: wlconf
###################################################
wlcfg <- "~/Dropbox/cs/osx/osxwr/mtrlchart-batch/wyck-cre/ge_aa_ref.csv"
read.csv(wlcfg)


###################################################
### code chunk number 8: ccd
###################################################
ccd(wl)


###################################################
### code chunk number 9: trcfg
###################################################
cfgtradefname <- '~/Dropbox/cs/osx/osxwr/mtrlchart-batch/order/geaa_tradebook_cfg_ref.csv'
read.csv(cfgtradefname)


###################################################
### code chunk number 10: mtrlchart.Rnw:127-128
###################################################
ccd(wl)


###################################################
### code chunk number 11: tck
###################################################
tdck(wl,pent=2.41, pstop=2.31, ft=2.45, mt=2.90)


###################################################
### code chunk number 12: mtrlchart.Rnw:133-134
###################################################
dev.off()


###################################################
### code chunk number 13: mtrlchart.Rnw:137-138
###################################################
tdpt(wl) ## print details of the potential trade, ftrr, mtrr are risk reward ratios


###################################################
### code chunk number 14: plo
###################################################
tdpl(wl,qty=1000,mkto=FALSE,plstop=TRUE)


###################################################
### code chunk number 15: cf
###################################################
gnf(wl)


###################################################
### code chunk number 16: tp
###################################################
tdp(wl)


###################################################
### code chunk number 17: trca
###################################################
tdca(wl,tid=1) # tid is a numeric id for each trade


###################################################
### code chunk number 18: wlmk (eval = FALSE)
###################################################
## library(quantmod)
## library(mtrlchart)
## grpsorted <- c("ix", "energy","metals","consumer" ,"financial"
##                       ,"healthcare", "industrial", "materials"
##                       , "technology", "utilities")
## 
## sdt <- "1980-01-01"
## edt <- "2015-10-02"
## csvfile <- "~/Dropbox/cs/osx/osxwr/mtrlchart-batch/wyck-cre/ge_aa_ref.csv"
## #create and download market data for the list from the csvfile
## mktd <- instrlistdatacsv(csvfile, grpsorted,  mode='live'
##                         ,startdate=sdt,enddate=edt
##                         , dsrc='yahoo', dld=T)
## 
## #Create Watchlist object
## wl <- wwlistfromdata(mktd)
## 
## setRdatfile(wl) <- '~/googledrive/cdat/wls/geaa-ref.rdat'
## #persistence
## persistMdata(wl)
## persist(wl,withmarketdata=FALSE,verbose=TRUE)


###################################################
### code chunk number 19: mtrlchart.Rnw:195-212 (eval = FALSE)
###################################################
## #orderbook config file
## cfgtradefname <- '~/Dropbox/cs/osx/osxwr/mtrlchart-batch/order/geaa_tradebook_cfg_ref.csv'
## #values metadata
## portf <- "eqportf"
## startcapital <- 100000  # initial trading capital nav
## iniDate <- "1990-01-01"  #earliest trading date
## risktr <- 0.01  # risk per trade as a percent of total NAV
## acc <- 10
## strat <- 1    
## activeinstrs <- c("AA","GE") # set traded  instruments
## ppars <- initPorfolioParams(accid=acc, stratid=strat, pname=portf, iniDate=iniDate
##                                 ,iniEquity=startcapital, risktradepct=risktr
##                                 ,actinstrs=activeinstrs )
## ob <- initOrderbookcsv(ppars,cfgtradefname)
## setWatchInstruments(ob)<-c("AA","GE")
## wl@orderbook <- ob
## persist(wl,withmarketdata=FALSE,verbose=TRUE)


###################################################
### code chunk number 20: wyck
###################################################
whelp(wl)



#rm(list=ls() )
#source('rwyahoo.R')
#source('rwtblox.R')
#
#
#source('Artifact.R')
#source('Wfact.R')
#source('Trendfact.R')
#source('Levelfact.R')
#source('Nfact.R')
#source('addTrendfact.R')
#
#source('addWfact.R')
#source('addLevelfact.R')
#
#source('addEp.R')
#source('addVoep.R')
#
#source('zooompick.R')
#
#
#source('Mldata.R')
#source('Wgconf.R')
#
#source('Wdict.R')
#source('Wstory.R')
#source('Wwatchlist.R')




doNext <- T
if ( doNext) {

test_that("Mldata with live yahoo data" , {
   instr <- c('SP500', 'XOM')
   tickr <- c('^GSPC', 'XOM')
   itype <- c('ix', 'eq')
   ilbl <- c('SP 500', 'Exxon Mobile Corp')
   igrp <- c('Index', 'Energy')

   grpsorted <- c('Energy', 'Index')

   mldl <- instrlistdata( instr, tickr, itype, ilbl,igrp, grpsorted
				        ,mode='live'
				        ,dsrc='yahoo',dld=T)
   expect_equal( length(mldl@idata), 2)

  wwl <- wwlistfromdata(mldl)

  setMarketdata(wwl) <- mldl
  setRdatfile(wwl) <- '~/Dropbox/cdat/wls/ttl.rdat'
  persist(wwl)

   ##test with chaning data mode  ## for mode historic  specifying startdate is mandatory
   dmode(wwl, 'historic', startdate='1985-06-01', enddate='2012-01-02')

})
}

doNext <- T
if ( doNext) {
	
	test_that("Mldata with historic yahoo data" , {
				instr <- c('SP500', 'IBM')
				tickr <- c('^GSPC', 'IBM')
				itype <- c('ix', 'eq')
				ilbl <- c('SP 500', 'Intl Business Machines Corp')
				igrp <- c('Index', 'Technology')
				
				grpsorted <- c('Technology', 'Index')
				
				mldl <- instrlistdata( instr, tickr, itype, ilbl,igrp, grpsorted
						,mode='historic'
                        ,startdate="1965-01-01", enddate="1991-01-01"
						,dsrc='yahoo',dld=T)
				
				expect_equal( length(mldl@idata), 2)
				
				wwl <- wwlistfromdata(mldl)
				
				setMarketdata(wwl) <- mldl
				setRdatfile(wwl) <- '~/Dropbox/cdat/wls/ttl.rdat'
				persist(wwl)				
			})
}


doNext <- T
if ( doNext) {

test_that("Wwatchlist operations methods" , {

  grpsorted <- c("ix", "ir", "fx","energy", "metal", "grain", "soft", "meat")

  instr <- c("ES", 'CL')
   tickr <- c("ES", 'CL')
   itype <-  rep('fut', length(tickr))
   ilbl <-  c("mini-S&P", "Light Crude Oil")
   igrp <- c("ix",  "energy")

   grpsorted <- c("ix", "ir", "fx","energy", "metal", "grain", "soft", "meat")

   mldl <- instrlistdata( instr, tickr, itype, ilbl,igrp, grpsorted, mode='live',dsrc='tblox',dld=T)
   expect_equal( length(mldl@idata), 2)

  wwl <- wwlistfromdata(mldl)

  setMarketdata(wwl) <- mldl

  ##add instrument with data
  addWatchInstrument(wwl, 'RB','RB', 'fut', 'Rbob', 'energy')
  expect_equal( length(wwl@mdata@idata), 3)

  #show(wwl)

  #rmWatchInstrument(wwl, 'RB')
  #expect_equal( length(wwl@mdata@idata), 2)

  setRdatfile(wwl) <- '~/Dropbox/cdat/wls/ttl.rdat'
  persist(wwl)

})
}

doNext <- T
if ( doNext) {
context("Wyckoff watchlist with tblox data")
test_that("Wwatchlist full tblox list from csv" , {

  grpsorted <- c("ix", "ir", "fx","energy", "metal", "grain", "soft", "meat")
  csvfile <- "~/Dropbox/cdat/tblox/szwl_tblox.csv"
  mldl <- instrlistdatacsv(csvfile, grpsorted, mode='live', enddate=strftime(Sys.Date()), dsrc='tblox', dld=T)

  expect_equal( length(mldl@idata), 22 )

  wwl <-  wwlistfromdata(mldl)

  setRdatfile(wwl) <- '~/Dropbox/cdat/wls/ttl.rdat'
  persist(wwl)
})
}


doAll <- FALSE

context("Wyckof dict")
context("Trend line")
doNext <- TRUE
if(doNext || doAll){

test_that("Wdict methods" , {

 wd <- wDict()
 expect_error( contains(wd, 1) , 'validation' )
 expect_equal( contains(wd, 'mama'), FALSE)
 expect_equal( contains(wd, 'UT'), TRUE)

 expect_output ( getWykLongname(wd, 'SOS'), 'Sign' )
 expect_output ( getWykLongname(wd, 'mama'), 'NA' )

} )
}



context("Trend line")
doNext <- TRUE
if(doNext || doAll){
test_that("trendline basic creation", {

  ss <- seq(from=as.POSIXct('2001-01-01',tz="UTC"),to=as.POSIXct('2001-06-01',tz="UTC"), by=86400)
  dd <- 100+1.9*rnorm(length(ss))
  xd <- xts(dd, order.by=ss)

  tf <- defTrend('fakeInstr'
                     ,'2001-03-01', xd['2001-03-01']
                     ,'2001-04-01', xd['2001-04-01']
                     ,xd)
  #browser()
  expect_equal( tf@dunit, "days")
  expect_equal( tf@dfrequency, 86400)
  cmpIntercept(tf, xd['2001-02-01::2001-05-01'])
  expect_false( is.na(getIntercept(tf) ))

  cmpIntercept(tf, xd['2001-04-02::2001-05-01'])
  expect_false( is.na(getIntercept(tf) ))

})

test_that("trendline with real ohlc creation", {

  idx <- as.POSIXct( c(
 "2011-10-03 14:29:08", "2011-10-04 14:29:08","2011-10-05 14:29:08", "2011-10-06 14:29:08",
 "2011-10-07 14:29:08", "2011-10-10 14:29:08", "2011-10-11 14:29:08", "2011-10-12 14:29:08",
 "2011-10-13 14:29:08", "2011-10-14 14:29:08", "2011-10-17 14:29:08", "2011-10-18 14:29:08",
 "2011-10-19 14:29:08", "2011-10-20 14:29:08", "2011-10-21 14:29:08", "2011-10-24 14:29:08",
 "2011-10-25 14:29:08", "2011-10-26 14:29:08", "2011-10-27 14:29:08", "2011-10-28 14:29:08",
 "2011-10-31 14:29:08", "2011-11-01 14:29:08", "2011-11-02 14:29:08", "2011-11-03 14:29:08",
 "2011-11-04 14:29:08", "2011-11-07 14:29:08","2011-11-08 14:29:08", "2011-11-09 14:29:08",
 "2011-11-10 14:29:08", "2011-11-11 14:29:08", "2011-11-14 14:29:08", "2011-11-15 14:29:08")
                   ,tz="UTC")

 data <- c( 76.25, 74.95, 76.94, 79.08, 81.36, 82.75, 83.97, 84.52, 83.17, 83.77, 85.88, 85.75,
         86.03, 84.22, 85.95, 87.00, 91.10, 90.00, 90.74, 92.01, 91.36, 89.17, 90.97, 90.87,
         92.87, 93.23, 95.23, 94.54, 95.20, 97.35, 97.19, 97.51 )

  d <- xts(data,order.by=idx)
  colnames(d) <-("Low")
  tf <- defTrend('CL' ,'2011-10-04', d['2011-10-04']$Low
                          ,'2011-11-01', d['2011-11-01']$Low, d)
  expect_equal( getSlope(tf), 0.711)

  #cmpShowMask(tf, d)
  #browser()
})
}


context("Wyckoff story")
doNext <- TRUE
if(doNext || doAll){

test_that("Wstory methods" , {
 m <-'CL'
 wd <- wDict()
 ws <- wstory(m)

 sos <- defWfact('CL','SOW', '2012-12-14', 94.21, getWykLongname(wd, 'SOS') , 'days', 86400, "UTC")
 sow <- defWfact('CL', 'SOT','2012-12-23', 100.23, getWykLongname(wd, 'SOT') , 'days', 86400,"UTC")

 addFact(ws) <- sos
 addFact(ws) <- sow

expect_equal( length(ws@facts)  , 2)

##test bulkreplace here
sow <- defWfact('CL', 'SOT','2012-12-24', 100.23, getWykLongname(wd, 'SOT') , 'days', 86400,"UTC")
addFact(ws) <- sow
sow <- defWfact('CL', 'SOT','2012-12-25', 100.23, getWykLongname(wd, 'SOT') , 'days', 86400,"UTC")
addFact(ws) <- sow
sow <- defWfact('CL', 'SOT','2012-12-30', 100.23, getWykLongname(wd, 'SOT') , 'days', 604800,"UTC")
addFact(ws) <- sow

bulkreplaceWyklabel(ws,'SOT','SOB', 'Sign of bears')

slbs <- unlist(lapply(ws@facts, function(x) { x@slabel } ))
mcrit <- which(slbs=='SOB')
expect_equal( length(mcrit), 4 )

idx2d <- getArtifact(ws,'SOB', '2012-12-23', 86400)

#browser()

delFact(ws,idx2d)
idx2d <- getArtifact(ws,'SOB', '2012-12-24',86400)
delFact(ws,idx2d)
idx2d <- getArtifact(ws,'SOB', '2012-12-25', 86400)
delFact(ws,idx2d)
idx2d <- getArtifact(ws,'SOB', '2012-12-30',604800)
delFact(ws,idx2d)

 sow <- defWfact('CL', 'SOT','2012-12-23', 100.23, getWykLongname(wd, 'SOT') , 'days', 86400)
 addFact(ws) <- sow

 ## add 2 trend lines
  load('~/googledrive/cdat/referencedata/gspc.rdat')
  d <- GSPC

  lpti <- '2011-10-04'; rpti <- '2011-12-16'
   tf <- defTrend('CL', lpti, d[lpti]$Low, rpti, d[rpti]$Low, d )


  lpti <- '2012-01-03'; rpti <- '2012-01-18'
  ttf <- defTrend('CL', lpti, d[lpti]$Low, rpti, d[rpti]$Low, d )

###
 addFact(ws) <- tf
 addFact(ws) <- ttf


 expect_equal( length(ws@facts)  , 4)

##test mkTechdt
 fdate <- "2012-01-18"
 sos <- defWfact('CL', 'SOS',fdate, 100.23, getWykLongname(wd, 'SOS') , 'days', 86400)

 #browser()

 ## add a couple of notes to the story
nfa <- defNobfact('CL', 'decreasing volume over last 5 days', '2011-12-14', 94.21, 'days', 86400)
nfb <- defNobfact('CL', 'increasing volume over last 3 days', '2012-02-14', 101.84, 'days', 86400)
 addFact(ws) <- nfa
 addFact(ws) <- nfb

 expect_equal( length(ws@facts)  , 6)


 #wfs <- getArtitype(ws,'Wyckoff', 'd1')
 wfs <- getWfacts(ws,'d1')
 expect_equal( length(wfs)  , 2)
 trls <- getTrlfacts(ws, 'd1')
 expect_equal( length(trls)  , 2)
 lvls <- getLvlfacts(ws, 'd1')
 expect_equal( length(lvls), 0 )

 nts <- getNotefacts(ws, 'd1')
expect_equal( length(nts), 2 )



 ## find fact, delete fact
 idx <- getArtifact(ws, 'MOUHAHA', '2012-01-03', 86400)
 expect_true(is.na(idx))

 idx <- getArtifact(ws, 'TRL', '2012-01-03', 86400)
 #browser()
 expect_true(is.na(idx))
 lng <- length(ws@facts)

#find artifact
fidx <- findArtifact(ws, '2012-01-03', 86400)
expect_true(is.na(fidx))



lvl <- defLevel('CL', '2012-01-03', 103.39, d)
addFact(ws) <- lvl
st <- defWfact('CL', 'ST','2012-01-03', 100.23, getWykLongname(wd, 'ST') , 'days', 86400)
addFact(ws) <- st

s<-Sys.time();
fidx <- findArtifact(ws, '2012-01-03', 86400)
e<-Sys.time();
expect_true( !is.na(fidx) && length(fidx) == 2)
#print(paste("findArtifact took", e-s))




 ## add note to a fact
 idx <- getArtifact(ws, 'SOT','2012-12-23', 86400)
 fct <- getFact(ws, idx)
 setNote(fct) <- 'added note'
 setFact(ws,idx) <- fct

 idx <- getArtifact(ws, 'SOT','2012-12-23', 86400)
 fct <- getFact(ws, idx)
 expect_false(is.na(fct@note))
 expect_equal(fct@note, "added note")

})}

context("Wyckoff gconf")
doNext <- FALSE
if(doNext || doAll){

test_that("gconf methods" , {

wg <- wGraphconf()
p <- getWykgparams(wg, 'd1')
expect_equal( p$cex, 0.5)

parsUpsize(wg)
p <- getWykgparams(wg, 'd1')
expect_equal( p$cex, 0.55)

expect_equal(p$color , "red") 
expect_equal(p$shape , "dt" )
flipWykshape(wg)
p <- getWykgparams(wg, 'd1')
expect_equal(p$shape , "ab" )

})
}

context("Wyckoff watchlist")
doNext <- FALSE
if(doNext || doAll){

test_that("Wwatchlist methods" , {


mkets <- c('CL', 'EB' ,'ES')

wwl <- wwlist( mkets )
#setInstrument4curstory(wwl) <- 'CL'
wwl@rdatfile <- '~/googledrive/cdat/wwl.rdat'
			
cstory <- getCurstory(wwl)
#browser()
expect_equal( cstory@instr, 'CL')

#browser()

## add some artifacts

load('~/googledrive/cdat/wldata_freeze.rdat')
d <- iwld$icoll[[2]]$cs$daily$data ## CL daily data

 lpti <- '2011-10-04'; rpti <- '2011-12-16'
   tf <- defTrend('CL', lpti, d[lpti]$Low, rpti, d[rpti]$Low, d )
setStopshowTrend(tf,d)<- '2012-02-01'
addFact(cstory) <- tf


  lpti <- '2012-01-03'; rpti <- '2012-01-18'
  ttf <- defTrend('CL', lpti, d[lpti]$Low, rpti, d[rpti]$Low, d )
  addFact(cstory) <- ttf

##add a couple of weekly trends
 wd <- to.weekly(d, indexAt='startof')

 lpti <- '1998-12-07'; rpti <- '2001-11-19'
 wtf <- defTrend('CL', lpti, Lo(wd[lpti]), rpti, Lo(wd[rpti]), wd )
 addFact(cstory) <- wtf


 lpti <- '2009-07-13'; rpti <- '2010-05-24'
 wtf <- defTrend('CL', lpti, Lo(wd[lpti]), rpti, Lo(wd[rpti]), wd )
 addFact(cstory) <- wtf

##add channel trend
rpti <- '2010-04-05'
cwtf <- defChan(wtf, rpti, Hi(wd[rpti]), wd)
addFact(cstory) <- cwtf




##set a couple of wfacts
 sos <- defWfact('CL','SOW', '2011-12-14', 94.21,  getWykLongname(wwl@wdict, 'SOW') , 'days', 86400)
 sow <- defWfact('CL', 'SOT','2011-12-23', 100.23,  getWykLongname(wwl@wdict, 'SOT') , 'days', 86400)
 ut <- defWfact('CL', 'UT','2012-02-14', 101.84,  getWykLongname(wwl@wdict, 'UT') , 'days', 86400)
 addFact(cstory) <- sos
 addFact(cstory) <- sow
 addFact(cstory) <- ut


## add a couple of notes to the story
nfa <- defNobfact('CL', 'decreasing volume over last 5 days', '2011-12-14', 94.21, 'days', 86400)
nfb <- defNobfact('CL', 'increasing volume over last 3 days', '2012-02-14', 101.84, 'days', 86400)
 addFact(cstory) <- nfa
 addFact(cstory) <- nfb


## add some levels
lvl <- defLevel('CL', '2011-05-31', 103.39, d)
addFact(cstory) <- lvl


lvl <- defLevel('CL', '2011-08-09', 75.71, d)
setStopshowLevel(lvl,d) <- '2011-11-01'
addFact(cstory) <- lvl



lvl <- defLevel('CL', '2011-09-01', 89.9, d)
setStopshowLevel(lvl,d) <- '2011-12-01'
addFact(cstory) <- lvl


lvl <- defLevel('CL', '2011-10-25', 94.65, d)
setStopshowLevel(lvl,d) <- '2012-02-01'
addFact(cstory) <- lvl

## add half lelvel
hlvl <- defHalfLevel('CL', "2011-05-06", 94.63, "2011-05-11", 104.60, d )
setStopshowLevel(hlvl,d) <- '2011-08-15'
addFact(cstory) <- hlvl

hlvl <- defHalfLevel('CL', "2011-07-26", 100.62, "2011-08-09", 75.71, d )
setStopshowLevel(hlvl,d) <- '2011-10-03'
addFact(cstory) <- hlvl


##add  a couple of weekly levels
wlvl <- defLevel('CL', '2010-05-03', 87.15, wd)
addFact(cstory) <- wlvl


wlvl <- defLevel('CL', '2011-04-11', 105.31, wd)
addFact(cstory) <- wlvl

wlvl <- defLevel('CL', '2011-05-02', 114.83, wd)
addFact(cstory) <- wlvl

#printStory(cstory,"d1")

#browser()
updStory(wwl) <- cstory

#save(wwl,file=wwl@rdatfile)
persist(wwl)


})
}


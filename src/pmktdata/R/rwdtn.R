#require(xts)
#require(iqfeed)

## this instance of function is not used  in this package
dtnConf  <- function(wenv="local")
{
    iqfrunner <- ''
    if (wenv =='local') #case for win machine which runs iqfeed
    {
        iqConf();
        print(paste("applying iqfeedconfig for local win machine"))
    }
    if (wenv == 'cern')
    {
        iqfrunner<-"lemvig.cern.ch"
        iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
        print(paste("applying iqfeedconfig for ", iqfrunner))
    }
    if (wenv == 'home')
    {
        iqfrunner<-"192.168.1.4"
        iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
        print(paste("applying iqfeedconfig for ", iqfrunner))
    }
    if (wenv == 'vm')
    {
        iqfrunner<-"192.168.50.129"
        iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
        print(paste("applying iqfeedconfig for ", iqfrunner))
    }
    if (wenv == 'vmp1')
    {
        iqfrunner<-"10.211.55.3"
        iqConf(host=iqfrunner, ports = list(level1=4009,historic=8100,level2=8200))
        print(paste("applying iqfeedconfig for ", iqfrunner))
    }

}

fixDtndailyindex<-function(xtsdata){
	d1fmt<-"%Y-%m-%d"
	newIndex <- as.POSIXct(strftime(index(xtsdata),format=d1fmt))
	nxtsdata <- xts(coredata(xtsdata),order.by=newIndex)
	return(nxtsdata)
}


#' @export
dtnHistDaily <- function(instrument, ndays=150)
{


    daily <- HDX(instrument, days=ndays)
    ohlc <- cbind(daily$Open, daily$High, daily$Low, daily$Close, daily$Volume)
    colnames(ohlc) <- c('Open', 'High', 'Low', 'Close', 'Volume')
	ohlc <- fixDtndailyindex(ohlc)
    return(ohlc)
}
#' @export
dtnHistDailyFS <- function(instrument, ndays = 150, maxfailed=5,verbose=T)
{
     gotInterrupt <- FALSE
     for (kk in 1:maxfailed)
     {
       tryCatch({
        if (gotInterrupt) { break }
        cxts <- dtnHistDaily(instrument, ndays=ndays) #call to donwload data
        aValue <- length(cxts)
        if (aValue > 0 ){
          if (verbose) {
            print(paste("downloaded daily bar serie",instrument, nrow(cxts), "elements" ))
          }
         break
      }
   }, interrupt = function(ex) {
       cat("An interrupt was detected.\n");
       gotInterrupt <- TRUE
       print(ex);
   }, error = function(ex) {
       cat("An error was detected.\n");
       print(ex);
       cat(paste('\n number of failed dtn downloads', kk, '\n' ))
       Sys.sleep(0.5) ## sleep 0.5 seconds in case of error before next download attempt
   }, finally = {
      #cat("Releasing resources...");
   }) # tryCatch()
  } ## for maxfailed loop
  return(cxts)
}

#' @export
dtnHistInterval <-function(instrument, intervalinsec ,startdatetime
                           , enddatetime=format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
                           , tz="America/NewYork")
{
  bars <- HIT(instrument, interval=intervalinsec, start=startdatetime, end=enddatetime, tz=tz)
  ohlc <- cbind(bars$Open, bars$High, bars$Low, bars$Close, bars$Volume)
  colnames(ohlc) <- c('Open', 'High', 'Low', 'Close', 'Volume')
  return(ohlc)
}

#' @export
dtnHistIntervalFS <- function(instrument, intervalinsec
                             ,startdatetime
                             ,enddatetime=format(Sys.time(),format="%Y-%m-%d %H:%M:%S")
                             ,tz="America/NewYork", maxfailed=5, verbose=T)
{
     gotInterrupt <- FALSE
     for (kk in 1:maxfailed)
     {
       tryCatch({
        if (gotInterrupt) { break }
        cxts <- dtnHistInterval(instrument, intervalinsec
                               ,startdatetime=startdatetime,enddatetime=enddatetime,tz=tz)

        aValue <- length(cxts)
        if (aValue > 0 ){
         if(verbose){
           print(paste("downloaded bar serie",instrument,intervalinsec
                                             ,startdatetime,enddatetime
                                             ,nrow(cxts), "elements"))
         }
         break
      }
   }, interrupt = function(ex) {
       cat("An interrupt was detected.\n");
       gotInterrupt <- TRUE
       print(ex);
   }, error = function(ex) {
       cat("An error was detected.\n");
       print(ex);
       cat(paste('\n number of failed dtn downloads', kk, '\n' ))
       Sys.sleep(0.5) ## sleep 0.5 seconds in case of error before next download attempt
   }, finally = {
      #cat("Releasing resources...");
   }) # tryCatch()
  } ## for maxfailed loop
  return(cxts)
}


#' @export
dtnHistTick <- function(instrument, startdatetime
                       , enddatetime=format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
                       , tz="America/NewYork")
{
    ticks <- HTT(instrument, start=startdatetime, end=enddatetime, tz=tz)
    return(ticks)
}

#' @export
dtnWriteBarsCsv <-function( xtsbars, dirname=".", filename="bars.csv", dirsep='/',hasHeader=T)
{
  fname <- paste(dirname,filename, sep=dirsep);
  write.zoo(xtsbars, file=fname, col.names=hasHeader, sep=",")
  #print(paste('attempted to (re)write file ', fname))
}
#' @export
dtnReadBarsCsv <-function(fname, hasHeader=T, isDaily=F)
{
  fdtfmt <- function(...) as.POSIXct(paste(...), format = "%Y-%m-%d %H:%M:%S")

  dfb <- read.csv(fname, sep=",", header=hasHeader)
  if(isDaily){
	 b <- read.zoo(dfb)
   }else{
     b <- read.zoo(dfb, index=list(1), FUN=fdtfmt )
  }
  ohlcdf <- try.xts(b, error="dataformaterror")
  colnames(ohlcdf)=c("Open","High","Low","Close","Volume")
  return(ohlcdf)
}
##deprecated use read,writeCsv instead
dtnWriteBarsTxt <-function( xtsbars, dirname=".", filename="bars.txt", dirsep='/')
{
  fname <- paste(dirname, dirsep,filename, sep='');
  write.zoo(xtsbars, file=fname, col.names=FALSE)
  print(paste('attempted to (re)write file ', fname))
}
##deprecated
dtnReadBarsTxt <-function(fname=bars.txt, hasHeader=T, isDaily=F)
{

  fdtfmt <- function(...) as.POSIXct(paste(...), format = "%Y-%m-%d %H:%M:%S")

  dfb <- read.csv(fname, sep=" ", header=hasHeader)
  if(isDaily){
	  b <- read.zoo(dbf)
  }else{
     b <- read.zoo(dfb, index=list(1,2), FUN=fdtfmt)
  }
  ohlcdf <- try.xts(b, error="dataformaterror")
  colnames(ohlcdf)=c("Open","High","Low","Close","Volume")
  return(ohlcdf)
}



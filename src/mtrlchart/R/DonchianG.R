##indicator to plot donchian around weekly and monthly extrema
#' @keywords internal
#' @export
DonchianG <-function(iohlc, gperiod="weeks",offset=0.25,dlag=T){
  okgperiodvals <- c("weeks","months")
  if (is.na(match(gperiod, okgperiodvals))) {
        stop(cat("valid values for gperiod param: ", okgperiodvals, "\n" ))
  }
  ifreq <- periodicity(iohlc)$frequency
  if( ifreq > 86400 ){
        stop("supplied xts object periodicity exptected to be daily at maximum")
   }
 ##internal functions
  slstweek <-function(x, xdata){
   dfmt <- "%Y-%m-%d"
   sfmt <- "%Y-%m-%d %H:%M:%S"
   cday <- as.POSIXct(x, format=dfmt)
   nwd <- as.numeric(strftime(as.POSIXct(x), format="%w"))
   ##special case for 1st element
   if(x == first(index(xdata))){
      minmax <- c(as.numeric(first(Hi(xdata))), as.numeric(first(Lo(xdata))) )
      return(minmax)
   }
   cday <- cday + 3600 * 23 + 60 * 59 + 59
   yday <- cday - 86400 *1
   sday <- cday - 86400 * (7 + nwd )
   #dsub <- paste(strftime(sday,format=sfmt),"::",strftime(yday,format=sfmt) ,sep="")##!! yday vs today
   dsub <- paste(strftime(sday,format=sfmt),"::",strftime(cday,format=sfmt) ,sep="")##!! yday vs today
   minmax <- c( as.numeric(max(Hi(xdata[dsub]))) ,  as.numeric(min(Lo(xdata[dsub]))) )
   return(minmax)
  }
  slstmonth <-function(x, xdata){
   dfmt <- "%Y-%m-%d"
   sfmt <- "%Y-%m-%d %H:%M:%S"
   cday <- as.POSIXct(x, format=dfmt)
   nwd <- as.numeric(strftime(as.POSIXct(x), format="%w"))
   nmo <- as.numeric(strftime(as.POSIXct(x), format="%m"))
   ny <- as.numeric(strftime(as.POSIXct(x), format="%Y"))

   ##special case for 1st element
   if(x == first(index(xdata))){
      minmax <- c(as.numeric(first(Hi(xdata))), as.numeric(first(Lo(xdata))) )
      return(minmax)
   }
   cday <- cday + 3600 * 23 + 60 * 59 + 59
   yday <- cday - 86400 *1
   if(nmo == 1){
      lmo <- 12
      ny <- ny-1
   }else{
      lmo <- nmo -1
   }
   sday <- as.POSIXct( paste(ny,"-",lmo,"-1 00:00:00",sep=""))
   dsub <- paste(strftime(sday,format=sfmt),"::",strftime(cday,format=sfmt) ,sep="")
   minmax <- c( as.numeric(max(Hi(xdata[dsub]))) ,  as.numeric(min(Lo(xdata[dsub]))) )
   return(minmax)
  }

 ##back to main body of the function

  if( ifreq < 86400 ){  ## case of intraday data
   dohlc <- to.daily(iohlc,indexAt="startof") 
   dohlc <- lag(dohlc) ## strip off last element (unfinished day)
  }else{
    dohlc <- iohlc
    if(dlag) { dohlc <- lag(dohlc)} ## strip off last day (useful for plotting donchian on daily data)
  }
  mixmax <- switch(gperiod,
                   "weeks" = sapply(index(dohlc), FUN=slstweek, xdata=dohlc)
                  ,"months" = sapply(index(dohlc), FUN=slstmonth, xdata=dohlc) )

  mx <- xts(mixmax[1,], order.by=index(dohlc))
  mn <- xts(mixmax[2,], order.by=index(dohlc))
  mxn <- cbind(mx,mn)
  fmaxmin <- mxn
  mid <- 0.5 * ( fmaxmin[,1] + fmaxmin[,2])
  dtopbrd  <- fmaxmin[,1] - offset * ( fmaxmin[,1] - fmaxmin[,2])
  dbotbrd  <- fmaxmin[,2] + offset * ( fmaxmin[,1] - fmaxmin[,2])
  donch <- cbind(fmaxmin[,1], fmaxmin[,2] , mid,  dtopbrd, dbotbrd)
  colnames(donch) <- c('dtop', 'dbot', 'mid', 'dtbord','dbbord')
  if( ifreq < 86400 ){
       idonch <- na.locf( merge( Cl(iohlc), donch, all=TRUE))
       donch<-idonch[,-1]
  }
 return(donch)

}

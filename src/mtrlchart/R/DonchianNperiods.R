#' Compute Donchian indicator on specific data period
#'
#'
#'
#' @keywords internal
#' @export
'DonchianNperiods' <- function (iohlc, period="days", k=1,  n=5, offset=0.25 )
{

  iohlc <- try.xts(iohlc, error=as.matrix)
  if (NCOL(iohlc) < 4 ){
        stop("please specify an xts ohlc object")
  }
  ifreq <- periodicity(iohlc)$frequency

  #if( ifreq > 86400 ){
  #      stop("supplied xts object periodicity exptected to be daily at maximum")
  #}

  hfohlc <- to.period(iohlc, period=period, k=k )
  hfreq <- periodicity(hfohlc)$frequency
  if(  hfreq < ifreq  ) {
        stop("lower periodicity at coarser data aggregation. check input parameters")
  }
  ## 2 cases hfreq  = ifreq, hfreq > ifreq
  #case hfreq == ifreq
  if ( hfreq == ifreq){
      iohlclg <- lag(iohlc,1) # lag by 1 of original data

      maxmin <- cbind ( runMax( Hi(iohlclg), n=n) , runMin( Lo(iohlclg) , n=n) )
      colnames(maxmin) <- c('ndaymax', 'ndaymin')
      fmaxmin <- xts(maxmin)
     #browser()
  }
  #case hfreq > ifreq
  if ( hfreq > ifreq){
     # no lag by 1 of original data
     maxmin <- cbind ( runMax( Hi(hfohlc), n=n), runMin(Lo(hfohlc) , n=n) )
     colnames(maxmin) <- c('ndaymax', 'ndaymin')
     donch <- na.locf( merge( Hi(iohlc), Lo(iohlc), maxmin, all=TRUE))
     fmaxmin <- cbind( donch[,3], donch[,4] )

  }

  mid <- 0.5 * ( fmaxmin[,1] + fmaxmin[,2])
  dtopbrd  <- fmaxmin[,1] - offset * ( fmaxmin[,1] - fmaxmin[,2])
  dbotbrd  <- fmaxmin[,2] + offset * ( fmaxmin[,1] - fmaxmin[,2])
  donch <- cbind(fmaxmin[,1], fmaxmin[,2] , mid,  dtopbrd, dbotbrd)
  colnames(donch) <- c('dtop', 'dbot', 'mid', 'dtbord','dbbord')
  #browser()
 return(donch)

}

# how to plot this indicator
# hl <- cbind(Hi(ohlc),Lo(ohcl))
# zigthresh = 1.5  # 1.5 price percent
# zz <- ZigZag(hl, change=zigthresh)
#zzlegs <-ZigZagLegs(ohlc, zz)
#barChar(ohlc, TA='addCalc1D(zz, onPricePane=T);addCalc1D(zzlegs[,1],OnPricePane=F,label="AvLegVolume")' )
#

#' Compute ZigZagLegs indicator
#'
#' Needs data produced by TTR ZigZag indicator
#'
#' @keywords internal
#' @export
'ZigZagLegs' <- function (ohlc, zigzagdata, epsilon=0.0001, aggresult=F)
{
  stopifnot("package:TTR" %in% search() || require("TTR", quietly = TRUE))

   if ( nrow(Vo(ohlc)) == 0 ){
      print('please specify ohlcv xts with open-high-low-close-volume columns format')
      stop()
   }

   if (nrow(zigzagdata) != nrow(ohlc) || index(first(ohlc)) != index(first(zigzagdata)) ) {
      print('index of zigzagdata does not match the one of the ohlc data')
      stop()
   }

  debug <- F


  ##1. find index of zigzagdata extremes
  #first derivative
  dzz <- zigzagdata - lag(zigzagdata)
  #second derivative
  d2zz <- dzz - lag(dzz)
  #remove NA
  d2zz <- d2zz[!is.na(d2zz)]
  expnts <- d2zz[ abs(d2zz) > epsilon ]

  #old
  #extrindex <- index(expnts -1 )
  #new dev
  fullindex <- index(ohlc)
  extridx <-  which(abs(d2zz) > epsilon )

  ##add last extr of undetermined leg
  nazig <- which(is.na(zigzagdata) == TRUE)
  if(length(nazig) > 1){
    extridx[length(extridx)+1] <- first(nazig)
  }
  extrindex <- fullindex[extridx] ## !!


  ## compute mean volume on each leg, store in on resulting xts
  firstIndex <- index(first(ohlc))
  lastIndex <- index(last(ohlc))
  nsingular <- length(extrindex)
  nlgcols <-5

  lgxts <- xts( matrix(NA, ncol=nlgcols, nrow=1)  , order.by=firstIndex) #initialize lgxts
  lgxc <- c()
  plegrangep <-as.numeric(Cl(first(ohlc)))/100  # 1% of price
  ##loop on legs found in extrindex
for( j in 1:(nsingular+1))
{
  if(j == 1){
      indStart <-firstIndex
  }else{
      indStart <- extrindex[j-1]
  }
  if(j == nsingular+1){
      indEnd <- lastIndex
      indStart <-extrindex[j-1]
  }else{
      indEnd <- extrindex[j]
  }
  #extract substr
  substr <- paste(strftime(indStart, format='%Y-%m-%d %H:%M:%S'),'::'
                 ,strftime(indEnd, format='%Y-%m-%d %H:%M:%S'), sep='')
  legohlc <- ohlc[substr]
  subdzz  <- dzz[substr]

  lastClose <- as.numeric( Cl(last(legohlc)))
  ##legside
  if ( j == 1){
    if ( as.numeric (Cl(ohlc[extrindex[1]])) > as.numeric(Cl(ohlc[firstIndex]))){
        legside <- 1
    }else{
        legside <- -1
    }
  }else{
      legside <- -1 * sign(plegside)
      if ( j > 3){
       if(  ( lastClose < plastClose && plastClose < pplastClose )
          ||( lastClose > plastClose && plastClose > pplastClose ) ) {
         legside <-  plegside
      }
     }
  }


  #leg mean volume
  legmvol <- mean(Vo(legohlc))
  #leg range in percentage from price
  if(legside == 1 ) {
       legrangep <- (as.numeric(Hi(last(legohlc))) /as.numeric(Lo(first(legohlc))) -1 )  * 100
  }else{
       legrangep <- (as.numeric(Hi(last(legohlc))) /as.numeric(Hi(first(legohlc))) -1 )  * 100
  }
  #leg retracement compared to previous leg
  legretrace <- abs(legrangep / plegrangep)
  if (legretrace > 1.1 ) { legretrace <- 1.1 } #cap retrace by 1.1

  if(debug){
   print(paste('substr', substr, 'leg length', nrow(legohlc)
               , 'leg price range', legrangep, 'leg m volume', legmvol))
  }
  ## added new measure
  ##compute cumulative  legs volume
  cumlegvol <- cumsum( Vo(legohlc) )

  #rowser()
  #append legxts
 # subindex <- index( legohlc[2:nrow(legohlc)] )
  subindex <- index( legohlc[1:(nrow(legohlc)-1)] )
  subnpts <- length(subindex)

  #scumlegvol <- cumlegvol[2:nrow(cumlegvol)] ##!!
  scumlegvol <- cumlegvol[1:(nrow(cumlegvol)-1)] ##!!

  legdata <- cbind( scumlegvol
                   ,rep(legmvol, subnpts)
                   ,rep(legrangep, subnpts)
                   ,rep(legretrace,subnpts)
                   ,rep(legside,subnpts )
                    )

  subxts <- xts(legdata, order.by=subindex)
  lgxts <- rbind(lgxts,subxts)

  ##add optionally returned aggregate legs info
  lgxc <- rbind(lgxc, xts(matrix( c( as.numeric(last(scumlegvol))
                                   ,legmvol, legrangep, legretrace,legside, nrow(legohlc)-1 )
                                ,nrow=1),order.by=indEnd) )
  plegrangep <- legrangep
  if( j > 2) {
      pplastClose <-plastClose
  }
  plastClose <- lastClose
  plegside <- legside
}
  colnames(lgxts) <- c('LegCumVol', 'LegAvVol', 'LegRange', 'LegRetrace', 'LegSide' )

  colnames(lgxc) <- c('LegCumVol','LegAvVol', 'LegRange', 'LegRetrace', 'LegSide', 'LegLength' )

  if (!aggresult) {
      return(lgxts)
  }else{
    return(lgxc)
  }

}


##############
# picks a bar from chartSeries chart.
# pickbar should always be applied on a chart with an only price pane drawed 
# (i.e. no volume pane below)
# to be sure, call pickbar(new=T) to chart just a price in a new graph window 
# output = list$t = POSIXct index, list$y = y_value
#############
#' @export 
`pickbar` <-
function (n = 1, eps = 2, new=T, gdev='default',gdevId=dev.cur(),arby=F)
{

    xdata <- quantmod:::get.chob()[[gdevId]]@xdata
    xsubset <- quantmod:::get.chob()[[gdevId]]@xsubset
    spacing <- quantmod:::get.chob()[[gdevId]]@spacing

    if(gdev == 'default') {
      gdev <- attr(dev.cur(), "names")
    }

    if(new){
     do.call(gdev,list())
     barChart(xdata[xsubset], TA=NULL) # ! TA=NULL  very important
    }


     point <- locator(1)

     usr <- par("usr")  ## gets coords of current graphs c(x1,x2, y1, y2)


     x.pos <- 1 + spacing * (1:length(xdata[xsubset]) - 1) #x.values were bars are drawn

     leftdt <- min(abs(floor(point$x) - x.pos))
     rightdt <- min(abs(ceiling(point$x) - x.pos))
     if(leftdt <= rightdt){
        pkIndex <- match( min(leftdt,rightdt), abs(floor(point$x) - x.pos))
      }else{
        pkIndex <- match( min(leftdt,rightdt), abs(ceiling(point$x) - x.pos))
      }

     if (is.na(pkIndex)){
         #pickIndex <- paste( index(last(xdata[xsubset])) ,sep = "") #pkIndex should always be !is.na
         pickIndex <- index(last(xdata[xsubset])) #pkIndex should always be !is.na
     }else{
        #pickIndex <- paste( index(xdata[xsubset])[pkIndex],sep = "")
        pickIndex <- index(xdata[xsubset])[pkIndex]
     }
	 
  

	 bhi <-as.numeric( Hi(xdata[pickIndex]) )
	 blo <-as.numeric( Lo(xdata[pickIndex]) )
	 bop <-as.numeric( Op(xdata[pickIndex]) )
	 bcl <-as.numeric( Cl(xdata[pickIndex]) )
	 diffs <- c(abs(bhi-point$y),abs(blo-point$y),abs(bop-point$y),abs(bcl-point$y))

	 bvals <-c(bhi,blo,bop,bcl)
     #browser()
     if(new){
        dev.off(which=dev.cur() ) ## switch off window, if it was created
     }
	 if(!arby){
	  pickY <- bvals[ match(min(diffs),diffs) ]		  
	  }else{
		  pickY <- point$y
	 }
    strIndex <- strftime(pickIndex,tz=indexTZ(xdata))
    tzval<-indexTZ(xdata)
  return(list(t=strIndex,y=pickY,tz=tzval))
}




#' @export 
`zooomf` <-
function (n = 10,gdev='default',gdevId=dev.cur(),nullta=F)
{

 cIndex <- pickbar(new=F)$t

 gdevId <- dev.cur()
 xdata <- quantmod:::get.chob()[[gdevId]]@xdata
 xsubset <- quantmod:::get.chob()[[gdevId]]@xsubset
 cidx <- match(as.POSIXct(cIndex, tz=indexTZ(xdata)), index( xdata[xsubset] ))
 csubset <- paste( index(xdata[xsubset])[max(1, cidx - n , na.rm = TRUE)]
                  ,index(xdata[xsubset])[min(cidx + n , NROW(xdata[xsubset]), na.rm = TRUE)]
                  ,sep = "::")

 if(gdev == 'default') {
   gdev <- attr(dev.cur(), "names")
 }
 do.call(gdev,list())
 if(nullta ){
   barChart(xdata[csubset], TA=NULL)
 }else{
  barChart(xdata[csubset], TA='addVo()')
 }

}
#' @export 
`getChartDataPeriodicity` <-function( gdevId=dev.cur())
  {
  xdata <- quantmod:::get.chob()[[gdevId]]@xdata
  p <- periodicity(xdata)
  if ( p$units == 'mins' ) { p$frequency <- p$frequency * 60 }
  return ( list(units=p$units, frequency=p$frequency))
 }
 
#' @export 
`getChartOhlcData` <-function( gdevId=dev.cur())
{
  xdata <- quantmod:::get.chob()[[gdevId]]@xdata
  return (xdata)
}

#' @export 
`getChartSubsetOhlcData` <-function( gdevId=dev.cur())
{
  lchob <- quantmod:::get.chob()[[gdevId]]
  xdata <- lchob@xdata
  subxdata <- xdata[lchob@xsubset]
  return (subxdata)
}

#' @export 
`getChartInstrumentName` <-function( gdevId=dev.cur())
  {
  icname <- quantmod:::get.chob()[[gdevId]]@name
  return ( icname)
 }


#' @export 
 `zooompick` <- function( gdevId=dev.cur(), gdev='default', n=10, arby=F)
{

  xdata <- quantmod:::get.chob()[[gdevId]]@xdata
  xsubset <- quantmod:::get.chob()[[gdevId]]@xsubset

 #browser()
 #chart a zooom in a new window with no indics
 zooomf(n=n,nullta=T)
 cat("pick a point of interest bar\n")


 pbar <- pickbar(new=F,arby=arby)
 #kill zoom window
 dev.off(which=dev.cur())
 return(pbar)
}

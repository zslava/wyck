# TODO: check if deprecated  getRefPointDatetime()
# TODO: check if deprecated  getRefPointVal()
# TODO: check if deprecated  setTrendfactColor()
# TODO: check if deprecated  cmpScaledTrendRange()
# Author: zimine
###############################################################################


#' Trendfact type
#'
#' Inherited from Artifact type. This fact placeable on a chart to represent a trend line.
#' Always have defined  a slot \code{showLeftDatetime}. If a slot \code{showRightDatetime} is defined
#' Trendfact stops at that date
#'
#'
#' @keywords internal
#' @rdname Trendfact-class
#' @name Trendfact
#' @exportClass Trendfact
setClass(Class='Trendfact'
         ,representation(leftDatetime='POSIXct'
                        ,leftYval='numeric'
                        ,rightDatetime='POSIXct'
                        ,rightYval='numeric'
                        ,showLeftDatetime='POSIXct'
                        ,showRightDatetime='POSIXct'
                        ,slope='numeric'
                        ,intercept='numeric'
                        ,showleftrefindex='numeric'
                        ,showrightrefindex='numeric'
                        ,x2plot='numeric'
                        ,y2plot='numeric'
                        )

         ,prototype(     leftDatetime=as.POSIXct(NA)
                        ,leftYval=as.numeric(NA)
                        ,rightDatetime=as.POSIXct(NA)
                        ,rightYval=as.numeric(NA)
                        ,showLeftDatetime=as.POSIXct(NA)
                        ,showRightDatetime=as.POSIXct(NA)
                        ,slope=as.numeric(NA)
                        ,intercept=as.numeric(NA)
                        ,showleftrefindex=as.numeric(NA)
                        ,showrightrefindex=as.numeric(NA)
                        ,x2plot=as.numeric(NA)
                        ,y2plot=as.numeric(NA)
                        ,typestr='TrendLine'
                        )
         ,contains='Artifact'
         )

##user friendly function for constructor



#' Define Trendfact
#'
#' Constructor function to create new instance of Trendfact.
#'
#' @param  iname character instrument name
#' @param  leftDtime character string for a left date point in format \code{%Y-%m-%d}
#' @param  leftyval  numeric  price value at this point
#' @param  rightDtime character string for a right date point in format \code{%Y-%m-%d}
#' @param  rightyval  numeric  price value at this point
#' @param  xtsdat    xts ohlc data object on which Trendfact is defined. ( must contain index for \code{leftDtime} and \code{rightDtime  )
#' @return  Trendfact instance
#' @keywords internal
#' @examples
#' \dontrun{  lpti <- '2012-01-03'; rpti <- '2012-01-18' \cr
#'            ttf <- defTrend('CL', lpti, Lo(data[lpti]), rpti, Lo(data[rpti]), data) }
#' @export
defTrend <- function(iname, leftDtime, leftyval, rightDtime, rightyval, xtsdat){

 o <- new("Trendfact", instr=iname
          ,leftYval=as.numeric(leftyval) ,rightYval=as.numeric(rightyval) )

 setDataPeriodicity(o) <- xtsdat
 setTrendfactColor(o)
 o@slabel <- 'TRL'
 o@llabel <- 'Trend Line'

   fmt <- '%Y-%m-%d %H:%M:%S'
 if (o@dunit =='days' && o@dfrequency >= 86400){ ## for daily & weekly xts data
   fmt <- '%Y-%m-%d'
 }


lp <- match( as.POSIXct(strftime(leftDtime,format=fmt), tz=indexTZ(xtsdat))
               , index(xtsdat) )
lr <- match( as.POSIXct(strftime(rightDtime,format=fmt), tz=indexTZ(xtsdat))
               , index(xtsdat) )
  

  if(!is.na(lp)) { o@leftDatetime <- index(xtsdat)[lp]
                    o@showLeftDatetime <-o@leftDatetime
                 }
  if(!is.na(lr)) { o@rightDatetime <- index(xtsdat)[lr]
                   o@ownDatetime <- o@leftDatetime
                 }
   ##compute and store slope
   cmpSlope(o,xtsdat)

 return(o)
}
#' Define Trend channel
#'
#' Constructor function to create new instance of Trendfact to form a trend channel
#'
#' @param  chanbase Trendfact instance which forms a channel base line
#' @param  leftDtime character string for a left date point in format \code{%Y-%m-%d}
#' @param  leftyval  numeric  price value at this point
#' @param  xtsdat    xts ohlc data object on which new Trendfact is defined
#' @return  Trendfact instance
#' @keywords internal
#' @examples
#' \dontrun{  lpti <- '2012-01-03'; rpti <- '2012-01-18' \cr
#'            ttf <- defTrend('CL', lpti, Lo(data[lpti]), rpti, Lo(data[rpti]), data) \cr
#'            rpti <- '2012-03-08' \cr
#'            cwtf <- defChan(ttf, rpti, Hi(data[rpti]), data)   }
#' @export
defChan <- function( chanbase, rightDtime, rightyval, xtsdat){
    #validation
    if ( !is(chanbase, 'Trendfact')){
            stop(paste("[Trendfact defChan validation]"
                       ,"param chanbase should be of type Trendfact"))
    }
    o <- new("Trendfact", instr=chanbase@instr
             ,rightYval=as.numeric(rightyval)
             ,leftDatetime=chanbase@leftDatetime
             ,slope=chanbase@slope )

   setDataPeriodicity(o) <- xtsdat
   setTrendfactColor(o)
   o@slabel <- 'TRL'
   o@llabel <- 'Trend Line'
   fmt <- '%Y-%m-%d %H:%M:%S'

   if (o@dunit =='days' && o@dfrequency >= 86400){ ## for daily & weekly xts data
    fmt <- '%Y-%m-%d'
   }

   lr <- match( as.POSIXct(strftime(rightDtime,format=fmt), tz=indexTZ(xtsdat))
               , index(xtsdat) )
   if(!is.na(lr)){
      o@rightDatetime <- index(xtsdat)[lr]
      o@ownDatetime <- o@rightDatetime
   }
   cmpIntercept(o,xtsdat)
   ##compute  leftYval
  t1 <- match(o@leftDatetime , index(xtsdat))
  y1 <- o@slope * t1 + o@intercept
  o@leftYval <- y1
  return(o)
}

#' copy all members from old object to new 
#'
#' @export 
copyTrendFact <-function ( olvl) 
{

  nt <- new("Trendfact")

  nt@leftDatetime       <-   olvl@leftDatetime        
  nt@leftYval           <-   olvl@leftYval            
  nt@rightDatetime      <-   olvl@rightDatetime       
  nt@rightYval          <-   olvl@rightYval           
  nt@showLeftDatetime   <-   olvl@showLeftDatetime    
  nt@showRightDatetime  <-   olvl@showRightDatetime   
  nt@slope              <-   olvl@slope               
  nt@intercept          <-   olvl@intercept           
  nt@showleftrefindex   <-   olvl@showleftrefindex    
  nt@showrightrefindex  <-   olvl@showrightrefindex   
  nt@x2plot             <-   olvl@x2plot              
  nt@y2plot             <-   olvl@y2plot              
  nt@id                 <-   olvl@id                  
  nt@instr              <-   olvl@instr               
  nt@typestr            <-   olvl@typestr             
  nt@slabel             <-   olvl@slabel              
  nt@llabel             <-   olvl@llabel              
  nt@color              <-   olvl@color               
  nt@ownDatetime        <-   olvl@ownDatetime         
  nt@note               <-   olvl@note                
  nt@dunit              <-   olvl@dunit               
  nt@dfrequency         <-   olvl@dfrequency          
  nt@monthdict          <-   olvl@monthdict           

return(nt)
}


###getters setters
#' @docType methods
setGeneric( name='getSlope'
           ,function(object){standardGeneric("getSlope")})
#' @aliases getSlope,Trendfact,ANY-method
setMethod('getSlope', 'Trendfact'
          ,function(object){
               return(object@slope)
           }
         )
#' @docType methods
setGeneric( name='getIntercept'
           ,function(object){standardGeneric("getIntercept")})
#' @aliases getIntercept,Trendfact,ANY-method
setMethod('getIntercept', 'Trendfact'
          ,function(object){
               return(object@intercept)
           }
         )
#' @docType methods
setGeneric( name='getTrendLine2plot'
           ,function(object){standardGeneric("getTrendLine2plot")})
#' @aliases getTrendLine2plot,Trendfact,ANY-method
setMethod('getTrendLine2plot', 'Trendfact'
          ,function(object){
               return( list(xvals=object@x2plot,yvals=object@y2plot))
           }
         )



#' @docType methods
setGeneric( name='getRefPointDatetime'
           ,function(object){standardGeneric("getRefPointDatetime")})
#' @aliases getRefPointDatetime,Trendfact,ANY-method
setMethod('getRefPointDatetime', 'Trendfact'
          ,function(object){
               return(object@rightDatetime)
           }
         )
#' @docType methods
setGeneric( name='getRefPointVal',function(object){standardGeneric("getRefPointVal")})
#' @aliases getRefPointVal,Trendfact,ANY-method
setMethod('getRefPointVal', 'Trendfact'
          ,function(object){
               return(object@rightYval)
           }
         )
#' @docType methods
setGeneric(name='setStopshowTrend<-'
               ,function(object,dserie,value){standardGeneric('setStopshowTrend<-')})
setReplaceMethod('setStopshowTrend'
                 ,'Trendfact'
                 ,function(object,dserie,value){
                     ##validate input
                     if ( !is(as.POSIXct(value), 'POSIXct'))  {
                          stop(paste("[Trendfact:setStopshowTrend validation]"
                                    ,"input cannot be coerced as POSIXct"))
                     }
                     if ( !is.xts(dserie))  {
                      stop(paste("[Trendfact:setStopshowTrend validation]"
                                 ," input should be xts object"))
                     }
                     if (   periodicity(dserie)$units != object@dunit
                        || periodicity(dserie)$frequency != object@dfrequency ) {
                      stop(paste("[Trendfact:setStopshowTrend validation]"
                                 ," supplied xts periodicity mismatch"))
                    }

                    if (is.na(value)){
                         object@showRightDatetime <- as.POSIXct(NA)
                         object@showrightrefindex <- as.numeric(NA)
                    }else{

                       rp <- match( value,  index(dserie))
                       if(!is.na(rp)) object@showRightDatetime <- index(dserie)[rp] 
                    }
                    return (object)
                 })

#' @docType methods
setGeneric( name='setTrendfactColor',function(.Object){standardGeneric("setTrendfactColor")})
#' @aliases setTrendfactColor,Trendfact,ANY-method
setMethod('setTrendfactColor', 'Trendfact'
          ,function(.Object){
             nameObject<-deparse(substitute(.Object))
             ## 3 use-cases data weekly, daily, hourly
             if ( .Object@dunit == "days" && .Object@dfrequency == 604800){
               .Object@color <- 'red'
             }
             if ( .Object@dunit == "days" && .Object@dfrequency == 86400 ){
               .Object@color <- 'hotpink'
             }
             if ( .Object@dunit == "hours" && .Object@dfrequency == 3600){
               .Object@color <- 'pink'
             }
             assign(nameObject,.Object,envir=parent.frame())
             return(invisible())
           }
         )


###business meethods

#' @docType methods
setGeneric('cmpSlope',function (.Object,dserie){standardGeneric('cmpSlope')})
#' @aliases cmpSlope,Trendfact,ANY-method
setMethod(f='cmpSlope', signature='Trendfact'
          ,def=function(.Object, dserie){
              ##validate input
              if ( !is.xts(dserie))  {
                    stop("[Trendfact:computeLinerParams validation] input should be xts object")
               }
              if (   periodicity(dserie)$units != .Object@dunit
                  || periodicity(dserie)$frequency != .Object@dfrequency ) {
                    stop("[Trendfact:cmpSlope validation] supplied xts periodicity mismatch")
              }
               nameObject<-deparse(substitute(.Object))
               t1 <- match(.Object@leftDatetime , index(dserie))
               t2 <- match(.Object@rightDatetime , index(dserie))
               y1 <- .Object@leftYval
               y2 <- .Object@rightYval
               #browser()
               if (!is.na(t1) && !is.na(t2) ){
                 .Object@slope <-  (y1 - y2 )  / ( t1 - t2 )
               }
               ##this assigns state changed object to a parent environment !! danger
               assign(nameObject,.Object,envir=parent.frame())
               return(invisible())
             }
          )
#' @docType methods
setGeneric('cmpIntercept',function (.Object,dserie){standardGeneric('cmpIntercept')})
#' @aliases cmpIntercept,Trendfact,ANY-method
setMethod(f='cmpIntercept', signature='Trendfact'
          ,def=function(.Object, dserie){
              ##validate input
              if ( !is.xts(dserie))  {
                    stop("[Trendfact:cmpIntercept validation] input should be xts object")
              }
              if (   periodicity(dserie)$units != .Object@dunit
                  || periodicity(dserie)$frequency != .Object@dfrequency ) {
                stop(paste("[Trendfact:cmpIntercept validation]"
                          ," supplied xts periodicity mismatch"))
              }
              if (is.na(.Object@slope)){
                    stop("[Trendfact:cmpIntercept validation] slope undefined")
              }
               nameObject<-deparse(substitute(.Object))
               t2 <- match(.Object@rightDatetime , index(dserie))
               y2 <- .Object@rightYval
               sl <- .Object@slope
               #browser()
               if (!is.na(t2)){
                  intcpt <-  y2 - sl * t2
                 .Object@intercept <- intcpt
               }else{
                 .Object@intercept <- as.numeric(NA)
               }
               ##this assigns state changed object to a parent environment !! danger
               assign(nameObject,.Object,envir=parent.frame())
               return(invisible())
             }
          )
#' @docType methods
setGeneric( name='cmpShowMskTrend'
           ,function(.Object,dserie){standardGeneric("cmpShowMskTrend")})
#' @aliases cmpShowMskTrend,Trendfact,ANY-method
setMethod('cmpShowMskTrend', 'Trendfact'
          ,function(.Object,dserie){
              if ( !is.xts(dserie))  {
                    stop("[Trendfact:cmpShowMskTrend validation] input should be xts object")
              }
              if (   periodicity(dserie)$units != .Object@dunit
                  || periodicity(dserie)$frequency != .Object@dfrequency ) {
                    stop(paste("[Trendfact:cmpShowMskTrend validation]"
                              ," supplied xts periodicity mismatch"))
              }
              nameObject<-deparse(substitute(.Object))
              #left side
              if (!is.na(.Object@leftDatetime)){
                   showLeftDatetime <- .Object@leftDatetime
                   t1 <- match(showLeftDatetime , index(dserie))
                   if(!is.na(t1)) {  .Object@showleftrefindex <- t1 }
              }else{
                .Object@showleftrefindex <- 1
              }
              #right side
              if (!is.na(.Object@showRightDatetime )){
                   showRightDatetime <- .Object@showRightDatetime
                   t2 <- match(showRightDatetime , index(dserie))
                   if(!is.na(t2)) { .Object@showrightrefindex <- t2 }
              }else{
                 .Object@showrightrefindex <- length(index(dserie))
              }
               ##this assigns state changed object to a parent environment !! danger
               assign(nameObject,.Object,envir=parent.frame())
               return(invisible())
            }
         )

#' @docType methods
setGeneric( name='cmpUnscaledTrendRange'
           ,function(.Object,xcvals, xfvals,cdserie){standardGeneric('cmpUnscaledTrendRange')})
#' @aliases cmpUnscaledTrendRange,Trendfact,ANY-method
setMethod('cmpUnscaledTrendRange', 'Trendfact'
          ,function(.Object,xcvals,xfvals,cdserie){
              ##validation
              if (   periodicity(cdserie)$units != .Object@dunit
                  || periodicity(cdserie)$frequency != .Object@dfrequency ) {
                    stop(paste("[Trendfact:cmpUnscaledTrendRange validation]"
                               ," supplied xts periodicity mismatch"))
                  }
              nameObject<-deparse(substitute(.Object))
              #compute trendline values
              yvals <- xcvals * .Object@slope  + .Object@intercept

              ##compute & apply show mask

              ml <- .Object@showleftrefindex
              mr <- .Object@showrightrefindex
              msk <-ifelse( (xcvals >= ml & xcvals <= mr),1,NA)
              yvals <- yvals * msk

              .Object@x2plot <- xfvals
              .Object@y2plot <- yvals
              #browser()
               ##this assigns state changed object to a parent environment
               assign(nameObject,.Object,envir=parent.frame())
               return(invisible())
            }
         )

#' @docType methods
setGeneric( name='cmpScaledTrendRange'
           ,function(.Object,xfvals,cdserie,refdserie)
                    {standardGeneric('cmpScaledTrendRange')})
#' @aliases cmpScaledTrendRange,Trendfact,ANY-method
setMethod('cmpScaledTrendRange', 'Trendfact'
          ,function(.Object,xfvals,cdserie,refdserie){
              ##validation
              if (   periodicity(refdserie)$units != .Object@dunit
                  || periodicity(refdserie)$frequency != .Object@dfrequency ) {
                    stop(paste("[Trendfact:cmpScaledTrendRange validation]"
                               ," supplied xts refdata periodicity mismatch"))
                  }
              nameObject<-deparse(substitute(.Object))

              #default values for  yvals
              yvals <- rep(NA, length(xfvals))

              ## filter on periodicity
              scalingAuthorized <- FALSE
              cdunits <- periodicity(cdserie)$units
              cdfrequency <- periodicity(cdserie)$frequency
              if (cdunits == "mins" ) { cdfrequency = cdfrequency * 60  } # in secs
              if (  .Object@dunit == 'days'
               && ( cdunits  == 'hours' || cdunits  == 'mins' || cdunits  == 'secs')
               && cdfrequency < 86400 ) {
                  scalingAuthorized <- TRUE
                  slopeScaled <- .Object@slope  / .Object@dfrequency
              }
              if ( scalingAuthorized ){
                 csubset <- paste( strftime(index(first(cdserie))),
                                   strftime(index(last(cdserie))), sep="::")
                 linref <- xts(1:nrow(refdserie) * .Object@slope + .Object@intercept
                              ,order.by=as.POSIXct(strftime(index(refdserie),format="%Y-%m-%d")))
                 llinref <-lag(linref)
                 lastlin <- last(linref)
                 currefdata <- llinref[csubset]
                 ##treat last day
                 cdlastdayIndex <- as.POSIXct( strftime(index(last(cdserie))
                                                       ,format='%Y-%m-%d'))
                 if ( cdlastdayIndex > index(last(currefdata))) {
                  currefdata <- rbind( currefdata
                                      ,xts( as.numeric(lastlin),order.by=cdlastdayIndex))
                 }
                  ##create xts with secs from day start
                  hss <- xts(  as.numeric( strftime(index(cdserie), format="%H")) * 3600
                              + as.numeric( strftime(index(cdserie), format="%M")) * 60
                              + as.numeric( strftime(index(cdserie), format="%S")) * 1
                             , order.by = index(cdserie))

                 #perform calculation of scaled line
                 wts <- na.locf(merge(hss, currefdata, all=TRUE)) ##merging

                 scwts <- wts[,1] * slopeScaled + wts[,2]
                 wts <- cbind(wts, scwts)
                 colnames(wts) <- c('dailysec', 'sday', 'sctr')
                 yvals <- as.numeric(scwts)

              ##mask
              if(is.na(.Object@showLeftDatetime )) { mskLeft <- index(first(cdserie)) }
              else { mskLeft <- .Object@showLeftDatetime }
              if(is.na(.Object@showRightDatetime )) { mskRight <- index(last(cdserie)) }
              else { mskRight <- .Object@showRightDatetime }

               msk <-ifelse( (index(scwts)  >= mskLeft & index(scwts)  <= mskRight) ,1,NA)
               yvals <- yvals * msk

               .Object@x2plot <- xfvals
               .Object@y2plot <- yvals
               }else{
               .Object@x2plot <- xfvals
               .Object@y2plot <- rep(NA, length(xfvals))
               }
               ##this assigns state changed object to a parent environment
               assign(nameObject,.Object,envir=parent.frame())
               return(invisible())
            }
         )

##new version
#' @docType methods
setGeneric( name='cmpnScaledTrendRange'
           ,function(.Object,xfvals,srcserie,trgserie,trg)
                    {standardGeneric('cmpnScaledTrendRange')})
#' @aliases cmpnScaledTrendRange,Trendfact,ANY-method
setMethod('cmpnScaledTrendRange', 'Trendfact'
          ,function(.Object,xfvals,srcserie,trgserie,trg){
              ##validation
               sp <- periodicity(srcserie)
               sfreq <- sp$frequency
               if ( sp$units == 'mins') { spfreq <- pfreq * 60 }
              if (   sp$units != .Object@dunit
                  || sfreq   != .Object@dfrequency ) {
                    stop(paste("[Trendfact:cmpnScaledTrendRange validation]"
                               ," supplied xts srcdata periodicity mismatch"))
                  }
              tp <- periodicity(trgserie)
              tpfreq <- tp$frequency
              if ( tp$units == 'mins') { tpfreq <- tpfreq * 60 }
              if ( sfreq  <= tpfreq ) {
                    stop(paste("[Trendfact:cmpnScaledTrendRange validation]"
                               ," srcdata has lower periodicity than trgdata"))
              }

               nameObject<-deparse(substitute(.Object))
              #default values for  yvals
              yvals <- rep(NA, length(xfvals))
              slopeScaled <- .Object@slope  / .Object@dfrequency

              ## filter on periodicity
              ## 2 cases  trg='d1' , trg='subd1'

              ctrgsubset <- paste( strftime(index(first(trgserie))),
                                strftime(index(last(trgserie))), sep="::")

              linsrc <- xts(1:nrow(srcserie) * .Object@slope + .Object@intercept
                              ,order.by=as.POSIXct(strftime(index(srcserie),format="%Y-%m-%d")))
              llinsrc <-lag(linsrc)
              lastlin <- last(linsrc)
              cursrcdata <- llinsrc[ctrgsubset]
                 ##treat last day
                 cdlastdayIndex <- as.POSIXct( strftime(index(last(trgserie))
                                                       ,format='%Y-%m-%d'))
                 if ( cdlastdayIndex > index(last(cursrcdata))) {
                  cursrcdata <- rbind( cursrcdata
                                      ,xts( as.numeric(lastlin),order.by=cdlastdayIndex))
                 }
                  ##create xts with secs from day start
                  if ( trg == 'subd1'){
                   hss <- xts(  as.numeric( strftime(index(trgserie), format="%H")) * 3600
                              + as.numeric( strftime(index(trgserie), format="%M")) * 60
                              + as.numeric( strftime(index(trgserie), format="%S")) * 1
                             , order.by = index(trgserie))
                   }
                  if ( trg == 'd1') {
                   hss <- xts(  as.numeric( strftime(index(trgserie), format="%w")) * 86400
                             #, order.by = index(trgserie) )
                             , order.by =  as.POSIXct(strftime(index(trgserie), format="%Y-%m-%d")) )
                  }
                 #perform calculation of scaled line
                 wts <- na.locf(merge(hss, cursrcdata, all=TRUE)) ##merging

                 scwts <- wts[,1] * slopeScaled + wts[,2]
                 wts <- cbind(wts, scwts)
                 colnames(wts) <- c('dailysec', 'sday', 'sctr')
                 yvals <- as.numeric(scwts)

               #browser()

              ##mask
              if(is.na(.Object@showLeftDatetime )) { mskLeft <- index(first(trgserie)) }
              else { mskLeft <- .Object@showLeftDatetime }
              if(is.na(.Object@showRightDatetime )) { mskRight <- index(last(trgserie)) }
              else { mskRight <- .Object@showRightDatetime }

               msk <-ifelse( (index(scwts)  >= mskLeft & index(scwts)  <= mskRight) ,1,NA)
               yvals <- yvals * msk

               .Object@x2plot <- xfvals
               .Object@y2plot <- yvals
               ##this assigns state changed object to a parent environment
               assign(nameObject,.Object,envir=parent.frame())
               return(invisible())
            }
         )


###printing Trendfact
setMethod('appendStory', 'Trendfact'
          ,function(object,uhist=F,lflag=F){
                 buf <- paste("Trend:")
                 if ( lflag ){  #long version
                   buf <- paste(buf,object@llabel )
                 }else{  #short version
                   buf <- paste(object@slabel )
                 }
                 buf <- paste(buf,",sl@"
                              ,format(object@slope,digits=2,scientific=F),sep="")
					  
                 if (uhist && !is.na(object@note) ){
                   buf <- paste(buf, "(uhist):", object@note)
                 }
                 buf <- paste(buf, "; ",sep="")

            return(buf)
           }
         )

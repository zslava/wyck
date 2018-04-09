# TODO: check if deprecated method setLevelfactColor
# TODO: check if deprecated  method setHalfLevelfactColor
# Author: zimine
###############################################################################

#' Levelfact type
#'
#' Inherited from Artifact type. Fact placeable on a chart to represent a price level.
#' Always have defined  a slot \code{showLeftDatetime}. If a slot \code{showRightDatetime} is defined
#' Levelfact stops at that date
#'
#'
#' @keywords internal
#' @rdname Levelfact-class
#' @name Levelfact
#' @exportClass Levelfact
setClass(Class='Levelfact'
         ,representation(leftDatetime='POSIXct'
                        ,leftYval='numeric'
                        ,showLeftDatetime='POSIXct'
                        ,showRightDatetime='POSIXct'
                        ,showleftrefindex='numeric'
                        ,showrightrefindex='numeric'
                        ,x2plot='numeric'
                        ,y2plot='numeric'
                        )

         ,prototype(     leftDatetime=as.POSIXct(NA)
                        ,leftYval=as.numeric(NA)
                        ,showLeftDatetime=as.POSIXct(NA)
                        ,showRightDatetime=as.POSIXct(NA)
                        ,showleftrefindex=as.numeric(NA)
                        ,showrightrefindex=as.numeric(NA)
                        ,x2plot=as.numeric(NA)
                        ,y2plot=as.numeric(NA)
                        ,typestr='LevelLine'
                        )
         ,contains='Artifact'
         )

#' Define Creek Levelfact
#'
#' Constructor function to create new instance of Creek levelfact.
#'
#' @param  iname character instrument name
#' @param  leftDtime character string for left date start in format \code{%Y-%m-%d}
#' @param  leftyval  numeric  price level for this Levelfact
#' @param  xtsdat    xts ohlc data object on which Levelfact is defined. ( must contain index for \code{leftDtime} )
#' @return  Levelfact instance
#' @keywords internal
#' @examples
#' \dontrun{lvlfct <- defLevel('ES', '2012-02-03', 1240.25, esxts_daily)}
#' @export
defLevel <- function(iname, leftDtime, leftyval, xtsdat, loffset=0 ) {

 o <- new("Levelfact", instr=iname
          ,leftYval=as.numeric(leftyval)  )

 setDataPeriodicity(o) <- xtsdat
 setLevelfactColor(o)
 o@slabel <- 'LVL'
 o@llabel <- 'Creek Level Line'

 if (o@dunit =='days' && o@dfrequency >=  86400 ){ ## for daily & weekly xts data
    fmt <- '%Y-%m-%d'
 }else{ #for subddaily xtsdata
   fmt <- '%Y-%m-%d %H:%M:%S'
 }
 #browser()
 # lp <- match( strftime(as.POSIXct(leftDtime),format=fmt, tz=indexTZ(xtsdat))
 #             ,strftime(index(xtsdat), format=fmt, tz=indexTZ(xtsdat)))

lp <- match( as.POSIXct(strftime(leftDtime,format=fmt), tz=indexTZ(xtsdat))
             ,index(xtsdat) )

 if(!is.na(lp)){
      o@leftDatetime <- index(xtsdat)[lp]
      # o@leftDatetime <- as.POSIXct(strftime(index(xtsdat)[lp]
      #                              ,format=fmt, tz=indexTZ(xtsdat)))
                                  
      # o@showLeftDatetime <- as.POSIXct(strftime(index(xtsdat)[lp+loffset]
      #                              ,format=fmt, tz=indexTZ(xtsdat)))
      o@showLeftDatetime <- index(xtsdat)[lp+loffset]
      o@ownDatetime <- o@leftDatetime ##  o@showLeftDatetime 
 }
 return(o)
}
#' Define Half Levelfact
#'
#' Constructor function to create new instance of Levelfact of special type to represent
#' half level between a bottom point and a top point
#'
#' @param  iname character instrument name
#' @param  leftDtime character string for left point date start in format \code{%Y-%m-%d}
#' @param  leftyval  numeric  price level at this left point
#' @param  rightDtime character string for right point date start in format \code{%Y-%m-%d}
#' @param  righttyval  numeric  price level at this right point
#' @param  xtsdat    xts ohlc data object on which Levelfact is defined. ( must contain index for \code{leftDtime} )
#' @return  Levelfact instance with \code{slabel} slot value \code{HFL}
#' @keywords internal
#' @export
defHalfLevel <- function(iname, leftDtime, leftyval,rightDtime,rightyval,xtsdat,loffset=0 ) {

 if(!is.numeric(loffset)) { loffset <- 0}
 
 o <- new("Levelfact", instr=iname
          ,leftYval=0.5*(as.numeric(leftyval) + as.numeric(rightyval))  )

 setDataPeriodicity(o) <- xtsdat
 setHalfLevelfactColor(o)
 o@slabel <- 'HFL'
 o@llabel <- 'Half Level Line'
 o@note <- paste("@["
                ,formatC(as.numeric(leftyval), digits=2, format='f'), " - "
                ,formatC(as.numeric(rightyval), digits=2, format='f'), "]",sep="" )

 fmt <- '%Y-%m-%d %H:%M:%S'
 if (o@dunit =='days' && o@dfrequency >= 86400 ){ ## for daily & weekly xts data
    fmt <- '%Y-%m-%d'
 }
 lp <- match( as.POSIXct(strftime(leftDtime,format=fmt),tz=indexTZ(xtsdat))
             , index(xtsdat) )
 lr <- match( as.POSIXct(strftime(rightDtime,format=fmt),tz=indexTZ(xtsdat))
             ,index(xtsdat) )

 
 if(!is.na(lp)){
      #o@leftDatetime <- index(xtsdat)[min(lp,lr)]
      o@leftDatetime <- index(xtsdat)[min(lp,lr)]
      o@ownDatetime <- o@leftDatetime
                                      
      #o@showLeftDatetime <-o@leftDatetime
      o@showLeftDatetime <- index(xtsdat)[ min(lp,lr)+loffset ]                                  
 }


 return(o)
}
#' Define Ice Levelfact
#'
#' Constructor function to create new instance of Ice levelfact.
#'
#' @param  iname character instrument name
#' @param  leftDtime character string for left date start in format \code{%Y-%m-%d}
#' @param  leftyval  numeric  price level for this Levelfact
#' @param  xtsdat    xts ohlc data object on which Levelfact is defined. ( must contain index for \code{leftDtime} )
#' @return  Levelfact instance
#' @keywords internal
#' @examples
#' \dontrun{lvlfct <- defIceLevel('ES', '2012-02Q -03', 1240.25, esxts_daily)}
#' @export
defIceLevel <- function(iname, leftDtime, leftyval, xtsdat, loffset=0 ) {
	
  if(!is.numeric(loffset)) { loffset <- 0}

	o <- new("Levelfact", instr=iname
			,leftYval=as.numeric(leftyval)  )
	
	setDataPeriodicity(o) <- xtsdat
	setLevelfactColor(o)
	o@slabel <- 'ICL'
	o@llabel <- 'Ice Level Line'
	
  fmt <- '%Y-%m-%d %H:%M:%S'
	if (o@dunit =='days' && o@dfrequency >= 86400 ){ ## for daily & weekly xts data
		fmt <- '%Y-%m-%d'
	}
	lp <- match( as.POSIXct(strftime(leftDtime,format=fmt), tz=indexTZ(xtsdat))
			         , index(xtsdat) )
	if(!is.na(lp)){
		 o@leftDatetime <- index(xtsdat)[lp]
     o@showLeftDatetime <- index(xtsdat)[lp+loffset]
     o@ownDatetime <- o@leftDatetime ##  o@showLeftDatetime 

	}
	return(o)
}


#' copy all members from old object to new 
#'
#' @export 
copyLevelfact <-function ( olvl) 
{

  nl <- new("Levelfact")

  nl@leftDatetime      <-  olvl@leftDatetime
  nl@leftYval          <-  olvl@leftYval    
  nl@showLeftDatetime  <-  olvl@showLeftDatetime
  nl@showRightDatetime <-  olvl@showRightDatetime 
  nl@showleftrefindex  <-  olvl@showleftrefindex  
  nl@showrightrefindex <-  olvl@showrightrefindex 
  nl@x2plot            <-  olvl@x2plot            
  nl@y2plot            <-  olvl@y2plot            
  nl@id                <-  olvl@id                
  nl@instr             <-  olvl@instr             
  nl@typestr           <-  olvl@typestr           
  nl@slabel            <-  olvl@slabel            
  nl@llabel            <-  olvl@llabel            
  nl@color             <-  olvl@color             
  nl@ownDatetime       <-  olvl@ownDatetime       
  nl@note              <-  olvl@note              
  nl@dunit             <-  olvl@dunit             
  nl@dfrequency        <-  olvl@dfrequency        
  nl@monthdict         <-  olvl@monthdict         

return(nl)

}



###getters setters
#' @docType methods
setGeneric( name='getLevel'
           ,function(object){standardGeneric("getLevel")})
#' @aliases getLevel,Levelfact,ANY-method
setMethod('getLevel', 'Levelfact'
          ,function(object){
               return(object@leftYval)
           })

#' @docType methods
setGeneric( name='getLevelLine2plot'
           ,function(object){standardGeneric("getLevelLine2plot")})
#' @aliases getLevelLine2plot,Levelfact,ANY-method
setMethod('getLevelLine2plot', 'Levelfact'
          ,function(object){
               return( list(xvals=object@x2plot,yvals=object@y2plot))
           })


#' @docType methods
setGeneric( name='setLevelfactColor',function(.Object){standardGeneric("setLevelfactColor")})
#' @aliases setLevelfactColor,Levelfact,ANY-method
setMethod('setLevelfactColor', 'Levelfact'
          ,function(.Object){
             nameObject<-deparse(substitute(.Object))
             ## 3 use-cases data weekly, daily, hourly
             if ( .Object@dunit == "days" && .Object@dfrequency == 604800){
               .Object@color <- 'blue'
             }
             if ( .Object@dunit == "days" && .Object@dfrequency == 86400 ){
               .Object@color <- 'cadetblue4'
             }
             if ( .Object@dunit == "hours" && .Object@dfrequency == 3600){
               .Object@color <- 'blueviolet'
             }
             assign(nameObject,.Object,envir=parent.frame())
             return(invisible())
           }
         )
#' @docType methods
setGeneric( name='setHalfLevelfactColor'
           ,function(.Object){standardGeneric("setHalfLevelfactColor")})
#' @aliases setHalfLevelfactColor,Levelfact,ANY-method
setMethod('setHalfLevelfactColor', 'Levelfact'
          ,function(.Object){
             nameObject<-deparse(substitute(.Object))
             ## 3 use-cases data weekly, daily, hourly
             if ( .Object@dunit == "days" && .Object@dfrequency == 604800){
               .Object@color <- 'darkgreen'
             }
             if ( .Object@dunit == "days" && .Object@dfrequency == 86400 ){
               .Object@color <- 'forestgreen'
             }
             if ( .Object@dunit == "hours" && .Object@dfrequency == 3600){
               .Object@color <- 'green3'
             }
             assign(nameObject,.Object,envir=parent.frame())
             return(invisible())
           }
         )

#' @docType methods
setGeneric(name='setStopshowLevel<-'
               ,function(object,dserie,value){standardGeneric('setStopshowLevel<-')})
setReplaceMethod('setStopshowLevel'
                 ,'Levelfact'
                 ,function(object,dserie,value){
                     ##validate input
                     if ( !is(as.POSIXct(value), 'POSIXct'))  {
                          stop(paste("[Levelfact:setStopShowRightLevel validation]"
                                    ,"input cannot be coerced as POSIXct"))
                     }
                     if ( !is.xts(dserie))  {
                      stop(paste("[Levelfact:setStopShowRightLevel validation]"
                                 ," input should be xts object"))
                     }
                     if (   periodicity(dserie)$units != object@dunit
                        || periodicity(dserie)$frequency != object@dfrequency ) {
                      stop(paste("[Levelfact:setStopShowRightLevel validation]"
                                 ," supplied xts periodicity mismatch"))
                    }
                    #browser()
                     ## for daily & weekly xts data
                    if (is.na(value)){
                         object@showRightDatetime <-as.POSIXct(NA)
                    }else{
                      rp <- match(value, index(dserie))
                      if(!is.na(rp)) object@showRightDatetime <- index(dserie)[rp] 
                    }
                    return (object)
                 }
                )

#' @docType methods
setGeneric( name='cmpLevelRange'
           ,function(.Object,xfvals, cdserie){standardGeneric('cmpLevelRange')})
#' @aliases cmpLevelRange,Levelfact,ANY-method
setMethod('cmpLevelRange', 'Levelfact'
          ,function(.Object,xfvals, cdserie){
              nameObject<-deparse(substitute(.Object))
              #compute trendline values
              yvals <- rep(.Object@leftYval, length(xfvals) )

              ##compute & apply show mask


              ##mask
              if(is.na(.Object@showLeftDatetime )) { mskLeft <- index(first(cdserie)) }
              else { mskLeft <- .Object@showLeftDatetime }
              if(is.na(.Object@showRightDatetime )) { mskRight <- index(last(cdserie)) }
              else { mskRight <- .Object@showRightDatetime }

               msk <-ifelse( (index(cdserie)  >= mskLeft & index(cdserie)  <= mskRight) ,1,NA)
               yvals <- yvals * msk

              #browser()
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
setGeneric( name='hasLevelRange'
           ,function(.Object){standardGeneric('hasLevelRange')})
#' @aliases hasLevelRange,Levelfact,ANY-method
setMethod('hasLevelRange', 'Levelfact'
          ,function(.Object){
              nameObject<-deparse(substitute(.Object))
              uy <- unique(.Object@y2plot)

              out <- TRUE
              if ( length(uy)==1 && is.na(uy[1])  ) {
                out<-FALSE
              } 
              return(out)
})


###printing Levelfact
setMethod('appendStory', 'Levelfact'
          ,function(object,uhist=F,lflag=F){
                 buf <- paste("Line:")
                 if ( lflag ){  #long version
                   buf <- paste(buf,object@llabel )
                 }else{  #short version
                   buf <- paste(object@slabel )
                 }
                 buf <- paste(buf,"@",object@leftYval,sep="")
                 if ( object@dfrequency < 86400){
                    buf <- paste(buf, "[", object@dfrequency,"]",sep="")
                 }                 
                 if (uhist && !is.na(object@note) ){
                   buf <- paste(buf, "(uhist):", object@note)
                 }

                 buf <- paste(buf, "; ",sep="")
            return(buf)
           }
         )

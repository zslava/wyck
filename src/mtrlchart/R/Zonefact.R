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
#' @rdname Zonefact-class
#' @name Zonefact
#' @exportClass Zonefact
setClass(Class='Zonefact'
         ,representation(loYval='numeric'
                        ,hiYval='numeric'
                        ,loLeftDatetime='POSIXct'
                        ,hiLeftDatetime='POSIXct'
                        ,showLeftDatetime='POSIXct'
                        ,showRightDatetime='POSIXct'
                        ,lines2plot='list'
                        )

         ,prototype(     loYval=as.numeric(NA)
                        ,hiYval=as.numeric(NA)
                        ,loLeftDatetime=as.POSIXct(NA)
                        ,hiLeftDatetime=as.POSIXct(NA)
                        ,showLeftDatetime=as.POSIXct(NA)
                        ,showRightDatetime=as.POSIXct(NA)
                        ,lines2plot=list(lx=as.numeric(NA)
                                        ,ly=as.numeric(NA)
                                        ,hx=as.numeric(NA)
                                        ,hy=as.numeric(NA))
                        ,typestr='ZoneLine'
                        )
         ,contains='Artifact'
         )
#' constructor
defZone <- function(iname, loDtime,hiDtime, loYval,hiYval, xtsdat, ndayscale=1,loffset=0 ) {
    o <- new("Zonefact",instr=iname
            ,loYval=as.numeric(loYval)
            ,hiYval=as.numeric(hiYval) )
    setDataPeriodicity(o) <- xtsdat
    #! setZonefactColor(o)
    o@slabel <- 'ZN'
    o@llabel <- 'Level zone'
    fmt <- '%Y-%m-%d %H:%M:%S'
    if (o@dunit =='days' && o@dfrequency >= 86400 ){ ## for daily & weekly xts 
        fmt <- '%Y-%m-%d'
    }
  losubprd <- paste("::",base::format(loDtime,format=fmt),sep="")
  hisubprd <- paste("::",base::format(hiDtime,format=fmt),sep="")
  lolefttime <- index(last(xtsdat[losubprd])) 
  hilefttime <- index(last(xtsdat[hisubprd])) 

  o@loLeftDatetime <- lolefttime
  o@hiLeftDatetime <- hilefttime

  loidx <- match(lolefttime,index(xtsdat)) # expect never NA
  hiidx <- match(hilefttime,index(xtsdat))
  
  o@showLeftDatetime <- index(xtsdat)[min(loidx,hiidx)+loffset]
  o@ownDatetime      <- index(xtsdat)[min(loidx,hiidx)]

  dfreq <- getXtsSecondsFrequency(xtsdat)
  ndayfwd <- getIntraDayWindowDays(dfreq,isToint=TRUE)

  o@showRightDatetime <- o@showLeftDatetime + 86400*ndayfwd*ceiling(ndayscale)


 return(o)
}

#' @export
 #' @docType methods
setGeneric(name='setShowRightDatetime<-'
               ,function(object,value){standardGeneric('setShowRightDatetime<-')})
setReplaceMethod('setShowRightDatetime'
                 ,'Zonefact'
                 ,function(object,value){
                     ##validate input
                     if ( !is.POSIXct(value) )  {
                          stop(paste("[Zonefact:setShowRightDatetime validation]"
                                    ," input value should be of type of POSIXct"))
                     }
                     if ( value < object@ownDatetime){
                          stop(paste("[Zonefact:setShowRightDatetime validation]"
                                    ," input value should be greater than object ownDatetime"))
                     }
                     object@showRightDatetime <-value
                     return (object)
                 })
 
#' computes graphical vectors for plotting zone 2 lines on a current chart 
#'
#' @docType methods
setGeneric( name='cmpZoneRange'
           ,function(.Object,xfvals, cdserie){standardGeneric('cmpZoneRange')})
#' @aliases cmpZoneRange,Zonefact,ANY-method
setMethod('cmpZoneRange', 'Zonefact'
          ,function(.Object,xfvals, cdserie){
              nameObject<-deparse(substitute(.Object))

              lyvals <- rep(.Object@loYval, length(xfvals) )
              hyvals <- rep(.Object@hiYval, length(xfvals) )

              ##compute & apply show mask

              ##mask
              mskLeft <- as.numeric(NA)
              mskRight <- as.numeric(NA)
              if(!is.na(.Object@showLeftDatetime )) { mskLeft <- .Object@showLeftDatetime }

              if(!is.na(.Object@showRightDatetime )) { mskRight <- .Object@showRightDatetime }
              else { mskRight <- index(last(cdserie)) }

              msk <- rep(NA,length(xfvals)) #default value
              #browser()
              if (!is.na(mskLeft) && !is.na(mskRight)){
                msk <-ifelse( (index(cdserie)  >= mskLeft & index(cdserie)  <= mskRight) ,1,NA)
              }
              lyvals <- lyvals * msk
              hyvals <- hyvals * msk

              .Object@lines2plot$lx <- xfvals
              .Object@lines2plot$hx <- xfvals

              .Object@lines2plot$ly <- lyvals
              .Object@lines2plot$hy <- hyvals


              ##add label xposition
              subidx <- which(lyvals == .Object@loYval)
              mid_idx <- ceiling(first(subidx) + 0.5*(last(subidx)-first(subidx)))
              xpos <- xfvals[mid_idx]
              .Object@lines2plot$lblxpos <- xpos

              #browser()
               assign(nameObject,.Object,envir=parent.frame())
               return(invisible())
})

#' @docType methods
setGeneric( name='hasZoneRange'
           ,function(.Object){standardGeneric('hasZoneRange')})
#' @aliases hasZoneRange,Zonefact,ANY-method
setMethod('hasZoneRange', 'Zonefact'
          ,function(.Object){
              nameObject<-deparse(substitute(.Object))
              uly <- unique(.Object@lines2plot$ly)
              uhy <- unique(.Object@lines2plot$hy)

              out <- TRUE
              if ( length(uly)==1 && is.na(uly[1]) && length(uhy)==1 && is.na(uhy[1]) ) {
                out<-FALSE
              } 
              return(out)
})
###printing Zonefact
setMethod('appendStory', 'Zonefact'
          ,function(object,uhist=F,lflag=F){
                 buf <- ""
                 if ( lflag ){  #long version
                   buf <- paste(buf,object@llabel )
                 }else{  #short version
                   buf <- paste(object@slabel )
                 }
                 buf <- paste(buf,"@",object@loYval,"-",object@hiYval,sep="")
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




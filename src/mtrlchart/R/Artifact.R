# TODO: check if deprecated slot color
# TODO: check if deprecated slot id
# TODO: check if deprecated setDefinedDataPeriodicity()
# Author: zimine
###############################################################################

#' Artifact type
#'
#' An abstract class of few types of objects associable with a price chart
#' keywords internal
#' @rdname Artifact-class
#' @name Artifact
setClass(Class="Artifact"
         ,representation(id='numeric'
                        ,instr='character'
                        ,typestr='character'
                        ,slabel='character'
                        ,llabel='character'
                        ,color='character'
                        ,ownDatetime='POSIXct'
                        ,note='character'
                        ,dunit ='character'
                        ,dfrequency='numeric'
                        ,monthdict='character'
                        ,'VIRTUAL')
         ,prototype(id=0
                   ,instr='undefined'
                   ,typestr='undefined'
                   ,slabel='undefined'
                   ,llabel='undefined'
                   ,note=as.character(NA)
                   ,color='grey'
                   ,ownDatetime=as.POSIXct('1970-01-01 00:00:01')
                   ,dunit='undefined'
                   ,dfrequency=as.numeric(NA)
                   ,monthdict=c('F','G','H', 'J','K','M','N','Q','U','V'
                               ,'X', 'Z')
                    )

         )


### common methods getters/ setters
#' @export
#' @docType methods
setGeneric( name='getSlabel',function(object){standardGeneric("getSlabel")})
#' @aliases getSlabel,Artifact,ANY-method
setMethod('getSlabel', 'Artifact'
          ,function(object){
               return(object@slabel)
           })

#' @docType methods
setGeneric(name='setSlabel<-'
               ,function(object,value){standardGeneric('setSlabel<-')})
#' @aliases setSlabel,Artifact,ANY-method
setReplaceMethod('setSlabel'
                 ,'Artifact'
                 ,function(object,value){
                     ##validate input
                     if ( !is.character(value))  {
                      stop("[Artifact:setSlabel validation] input should be character string")
                     }
                     object@slabel <- value
                     return (object)
                 })

#' @export
#' @docType methods
setGeneric( name='getLlabel',function(object){standardGeneric("getLlabel")})
#' @aliases getLlabel,Artifact,ANY-method
setMethod('getLlabel', 'Artifact'
          ,function(object){
               return(object@llabel)
           })

#' @docType methods
setGeneric(name='setLlabel<-'
               ,function(object,value){standardGeneric('setLlabel<-')})
#' @aliases setLlabel,Artifact,ANY-method
setReplaceMethod('setLlabel'
                 ,'Artifact'
                 ,function(object,value){
                     ##validate input
                     if ( !is.character(value))  {
                      stop("[Artifact:setLlabel validation] input should be character string")
                     }
                     object@llabel <- value
                     return (object)
                 })

#' @export
#' @docType methods
setGeneric( name='getDunit',function(object){standardGeneric("getDunit")})
#' @aliases getDunit,Artifact,ANY-method
setMethod('getDunit', 'Artifact'
          ,function(object){
               return(object@dunit)
           })
#' @export
#' @docType methods
setGeneric( name='getDfrequency',function(object){standardGeneric("getDfrequency")})
#' @aliases getDfrequency,Artifact,ANY-method
setMethod('getDfrequency', 'Artifact'
          ,function(object){
               return(object@dfrequency)
           })

#' @export
#' @docType methods
setGeneric( name='getColor',function(object){standardGeneric("getColor")})
#' @aliases getColor,Artifact,ANY-method
setMethod('getColor', 'Artifact'
          ,function(object){
               return(object@color)
           })

#' @export
#' @docType methods
setGeneric( name='getArtiType',function(object){standardGeneric("getArtiType")})
#' @aliases getArtiType,Artifact,ANY-method
setMethod('getArtiType', 'Artifact'
          ,function(object){
               return(object@typestr)
           })

#' @export
#' @docType methods
setGeneric( name='getOwndtime',function(object){standardGeneric("getOwndtime")})
#' @aliases getOwndtime,Artifact,ANY-method
setMethod('getOwndtime', 'Artifact'
          ,function(object){
               return(object@ownDatetime)
           })


#' @export
#' @docType methods
setGeneric( name='getOwndtimeStr',function(object){standardGeneric("getOwndtimeStr")})
#' @aliases getOwndtimeStr,Artifact,ANY-method
setMethod('getOwndtimeStr', 'Artifact'
          ,function(object){
               tzval <- "UTC"
               if(!is.null(attributes(object@ownDatetime)$tzone)) { 
                  tzval <- attributes(object@ownDatetime)$tzone 
               }
               dfmt <- '%Y-%m-%d %H:%M:%S'
               if (  object@dfrequency >= 86400 ){ ## for daily & weekly xts data
                  dfmt <- '%Y-%m-%d' 
                }
                dtstr <- strftime( as.character(object@ownDatetime), format=dfmt )      
               return(dtstr)
           })

#' @export
#' @docType methods
setGeneric( name='getShortnoteOwndtime',function(object){standardGeneric("getShortnoteOwndtime")})
#' @aliases getShortnoteOwndtime,Artifact,ANY-method
setMethod('getShortnoteOwndtime', 'Artifact'
          ,function(object){
                tzv <- attributes(object@ownDatetime)$tzone
                buf <- c()
                ownYrsn <- as.numeric(strftime( object@ownDatetime, format='%y', tz=tzv))
                ownMnth <- as.numeric(strftime( object@ownDatetime, format='%m', tz=tzv))
                ownMnthShrt <- strftime( object@ownDatetime, format='%b', tz=tzv)
                ownMnthchar <- object@monthdict[ownMnth]
                ownDay <-  strftime( object@ownDatetime, format='%d', tz=tzv)
                #buf <- paste(buf, ownYrsn,ownMnthchar, ownDay,sep="")
                buf <- paste(buf, ownYrsn,ownMnthShrt, ownDay,sep="")

               #add hours details for subdaily artifacts
               if ( object@dunit == 'hours' ){
                    ownHour <- strftime( object@ownDatetime, format='%H',tz=tzv)
                    ownMin <- strftime( object@ownDatetime, format='%M', tz=tzv)
                buf <- paste(buf, "_", ownHour,":",ownMin,sep="")
               }
               return(buf)
           })




### biz methods


#' @docType methods
setGeneric(name='setArtifactId<-'
               ,function(object,value){standardGeneric('setArtifactId<-')})
#' @aliases setArtifactId,Artifact,ANY-method
setReplaceMethod('setArtifactId'
                 ,'Artifact'
                 ,function(object,value){
                     ##validate input
                     if ( !is.numeric(value))  {
                          stop("[Artifact:setArtifactId validation] input should be numeric")
                     }
                     object@id <- value
                     return (object)
                })


#' @docType methods
#' @export
setGeneric( name='getNote',function(object){standardGeneric("getNote")})
#' @aliases getNote,Artifact,ANY-method
setMethod('getNote', 'Artifact'
          ,function(object){
               return(object@note)
           })

#' @docType methods
setGeneric(name='setNote<-'
               ,function(object,value){standardGeneric('setNote<-')})
#' @aliases setNote,Artifact,ANY-method
setReplaceMethod('setNote'
                 ,'Artifact'
                 ,function(object,value){
                     ##validate input
                     if ( !is.character(value))  {
                      stop("[Artifact:setNote validation] input should be character string")
                     }
                     object@note <- value
                     return (object)
                 })
#' @docType methods
		 setGeneric(name='apndNote<-'
				 ,function(object,value){standardGeneric('apndNote<-')})
#' @aliases setNote,Artifact,ANY-method
		 setReplaceMethod('apndNote'
				 ,'Artifact'
				 ,function(object,value){
					 ##validate input
					 if ( !is.character(value))  {
						 stop("[Artifact:apndNote validation] input should be character string")
					 }
					 if(is.na(object@note)  ){
					  	object@note <-value
					 }
					 else{
					   	object@note <- paste(object@note,"; " ,value,sep="")
				     }
					 return (object)
				 })		 

#' @docType methods
setGeneric(name='setDataPeriodicity<-'
               ,function(object,value){standardGeneric('setDataPeriodicity<-')})
#' @aliases setDataPeriodicity,Artifact,ANY-method
setReplaceMethod('setDataPeriodicity'
                 ,'Artifact'
                 ,function(object,value){
                     ##validate input
                     if ( !is.xts(value))  {
                          stop(paste("[Artifact:setDataPeriodicity validation]"
                                    ," input should be xts object"))
                     }
                     p <- periodicity(value)
                     object@dunit <- p$units
                     pfreq <- p$frequency
                     if (p$units == 'mins' ) { pfreq <- pfreq * 60 } #convert frequency to seconds
                     object@dfrequency <- pfreq
                     return (object)
                 })
#' @docType methods
setGeneric(name='setDefinedDataPeriodicity<-'
               ,function(object,value){standardGeneric('setDefinedDataPeriodicity<-')})
#' @aliases setDefinedDataPeriodicity,Artifact,ANY-method
setReplaceMethod('setDefinedDataPeriodicity'
                 ,'Artifact'
                 ,function(object,value){
                     ##validate input
                     if ( !is.character(value))  {
                          stop(paste("[Artifact:setDefinedDataPeriodicity validation]"
                                    ," input should be a character"))
                     }
                     if (   value != 'weekly'
                         && value != 'daily'
                         && value != 'hourly')
                       {
                          stop(paste("[Artifact:setDefinedDataPeriodicity validation]"
                                    ,"value should be weekly, daily or hourly"))
                       }
                     if ( value == 'weekly') {
                      object@dunit <- 'days'
                      object@dfrequency <- 604800
                     }
                     if ( value == 'daily') {
                      object@dunit <- 'days'
                      object@dfrequency <- 86400
                     }
                     if ( value == 'hourly') {
                      object@dunit <- 'hours'
                      object@dfrequency <- 3600
                     }
                     return (object)
                 })


#' appends to print buffer Artifact as a asring
#'
#' all Artifact children tyhpes  implemnt this method
#'
#' @param object  instance of Artificat derived type
#' @param lflag  Boolean  a flag for long or short print format
#' @export
#' @docType methods
setGeneric( name= 'appendStory',function(object,uhist,lflag){standardGeneric("appendStory")})
#' @aliases appendStory,Artifact,ANY-method
setMethod('appendStory', 'Artifact'
          ,function(object,uhist,lflag){
                 return(invisible())
           })

 #' makes from an Artifact own Date a count Month's day
#'
#'
#' @param object  instance of Artificat derived type
#' @param xtsdat  serie with instrument data daily
#' @export
#' @docType methods
setGeneric(name='getTechdt'
		   ,function(object,xtsdat){standardGeneric('getTechdt')})
#' @aliases getTechdt,Artifact,ANY-method
   setMethod('getTechdt'
		   ,'Artifact'
		   ,function(object,xtsdat){
			   ## make tech date value  only for daily artifacts
			   if (object@dunit == 'days' && object@dfrequency == 86400){
				   fmtm <- "%Y-%m"
				   fmtd <- "%Y-%m-%d"
				   subl <- strftime(object@ownDatetime, format=fmtm)
				   subr <- strftime(object@ownDatetime, format=fmtd)
				   sub <- paste(subl,subr,sep="::")
				   daycnt <- nrow(xtsdat[sub])
				   nmth <- as.numeric(strftime(object@ownDatetime, format="%m"))
				   techvalue <- paste(object@monthdict[nmth] , formatC(daycnt,width=2,flag="0"),sep="")
				   return(techvalue)
			   }else{
				   return(as.character(NA))
			   }
		   })
   
   
#' @export
setMethod ('show'
           ,'Artifact'
           , function(object){
              tzstr <-""
              tzattr <- attributes(object@ownDatetime)$tzone
              if(!is.null(tzattr)) tzstr <- tzattr 
              cat("*** Class Artifact,*** \n")
              cat(paste("Instrument:", object@instr
                        ,"Date:", object@ownDatetime
                        ,tzstr
                       ,"Label:", object@slabel
                       ,"data frequency:", object@dfrequency, "\n",sep=" "))
           })

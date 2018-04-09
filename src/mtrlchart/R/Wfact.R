# TODO: check if deprecated getLbloffs()
# TODO: check if deprecated getLblcex()
# Author: zimine
###############################################################################

#' Wfact type
#'
#' Inherited from Artifact type. This fact is placeable on a chart is to represent a Labelized fact.
#'
#'
#'
#'
#' @keywords internal
#' @rdname Wfact-class
#' @name Wfact
#' @exportClass Wfact
setClass(Class='Wfact'
         ,representation(candidate='logical'
                        ,yvalue='numeric'
                        ,label2chart='character'
		                    ,lbloffs='numeric'
                        ,lblcex='numeric'
                        ,isconfirmed='logical'
                         )
         ,prototype(candidate=FALSE
                   ,yvalue=as.numeric(NA)
                   ,label2chart=as.character(NA)
                   ,lbloffs=0.25
                   ,lblcex=0.5
                   ,isconfirmed=TRUE
                   ,typestr='Wyckoff')
         ,contains='Artifact'
         )


###helper function to instantiate Wfact Objects
bareWyckoff <-function(iname, oDateTime, yvalue, dunit, dfreq,tzvalue){
     if(missing(tzvalue)){ tzvalue <- "UTC" }
     dfmt <- "%Y-%m-%d %H:%M:%S"
     if(dfreq >= 86400) {dfmt <- "%Y-%m-%d" }
     o <- new('Wfact'
              ,instr=iname
              ,ownDatetime=as.POSIXct(strftime(oDateTime,format=dfmt),tz=tzvalue)
              ,yvalue=as.numeric(yvalue)
              ,dunit=as.character(dunit), dfrequency=as.numeric(dfreq))
     return(o)
 }
#' Define Wfact
#'
#' Constructor function to create new instance of Wfact.
#'
#' @param  iname character instrument name
#' @param  slabel character short label of wfact
#' @param  oDateTime character string for Wfact associated date in format \code{%Y-%m-%d}
#' @param  yvalue  numeric  price value at this point
#' @param  longlabel character long label of wfact
#' @param  dunit  character \code{'weeks'|'days'|'hours'|'minutes' }  for associated xts data periodicity
#' @param  dfreq  numeric  numbr of seconds in an associated xts data periodicity
#' @return  Wfact instance
#' @keywords internal
#' @examples
#' \dontrun{sow <- defWfact('CL','SOW', '2011-12-14', 94.21, 'Sign of Weakness' , 'days', 86400)}
#' @export 
defWfact <-function(iname, slabel, oDateTime, yvalue, longlabel, dunit, dfreq,tzval )
  {
    o <- bareWyckoff(iname, oDateTime, yvalue, dunit, dfreq, tzval)
    o@slabel <- slabel
    o@llabel <- as.character(longlabel)
  return(o)
}

#' copy all members from old object to new 
#'
#' @export 
copyWfact <-function ( owf) 
{
nwf <- new("Wfact")

  nwf@candidate    <-   owf@candidate   
  nwf@yvalue       <-   owf@yvalue      
  nwf@label2chart  <-   owf@label2chart 
  nwf@lbloffs      <-   owf@lbloffs     
  nwf@lblcex       <-   owf@lblcex      
  nwf@id           <-   owf@id          
  nwf@instr        <-   owf@instr       
  nwf@typestr      <-   owf@typestr     
  nwf@slabel       <-   owf@slabel      
  nwf@llabel       <-   owf@llabel      
  nwf@color        <-   owf@color       
  nwf@ownDatetime  <-   owf@ownDatetime 
  nwf@note         <-   owf@note        
  nwf@dunit        <-   owf@dunit       
  nwf@dfrequency   <-   owf@dfrequency  
  nwf@monthdict    <-   nwf@monthdict   

  ##new member nwf@isconfirmed is set to TRUE by default
  return(nwf)
}

##biz methods
#' @docType methods
setGeneric(name='setIsconfirmed<-'
		,function(object,value){standardGeneric('setIsconfirmed<-')})
#' @aliases setIsconfirmed,Wact,ANY-method
setReplaceMethod('setIsconfirmed'
		,'Wfact'
		,function(object,value){
			##validate input
			if ( !is.logical(value))  {
				stop("[Wfact:setIsconfirmed validation] input should be a boolean value")
			}
			object@isconfirmed <- value
			return (object)
		})
#' @docType methods
setGeneric( name='getIsconfirmed',function(object){standardGeneric("getIsconfirmed")})
#' @aliases getIsconfirmed,Wfact,ANY-method
setMethod('getIsconfirmed', 'Wfact'
		,function(object){
			return(object@isconfirmed)
		})

#' @docType methods
setGeneric( name='getYval',function(object){standardGeneric("getYval")})
#' @aliases getYval,Wfact,ANY-method
setMethod('getYval', 'Wfact'
          ,function(object){
               return(object@yvalue)
           })
   
#' @docType methods
   setGeneric(name='setYval<-'
		   ,function(object,value){standardGeneric('setYval<-')})
#' @aliases setYval,Wact,ANY-method
   setReplaceMethod('setYval'
		   ,'Wfact'
		   ,function(object,value){
			   ##validate input
			   if ( !is.numeric(value))  {
				   stop("[Wfact:setYval validation] input should be a numeric value")
			   }
			   object@yvalue <- value
			   return (object)
		   })
   
#' @docType methods
setGeneric( name='getLabel2chart',function(object){standardGeneric("getLabel2chart")})
#' @aliases getLabel2chart,Wfact,ANY-method
setMethod('getLabel2chart', 'Wfact'
          ,function(object){
               return(object@label2chart)
           })

#' @docType method
setGeneric( name='getLbloffs',function(object){standardGeneric("getLbloffs")})
#' @aliases getYval,Wfact,ANY-method
setMethod('getLbloffs', 'Wfact'
          ,function(object){
               return(object@lbloffs)
           })
#' @docType method
setGeneric( name='getLblcex',function(object){standardGeneric("getLblcex")})
#' @aliases getLblcex,Wfact,ANY-method
setMethod('getLblcex', 'Wfact'
          ,function(object){
               return(object@lblcex)
           }
         )

		 		 
#' @docType method
setGeneric( name='cmpLabelParams',function(.Object,lbltype){standardGeneric("cmpLabelParams")})
#' @aliases cmpLabelParams,Wfact,ANY-method
setMethod('cmpLabelParams', 'Wfact'
          ,function(.Object,lbltype){
              ##validate input
              if (   lbltype != 'dt'
                  && lbltype != 'ab' ){
                    stop("[Wfact:cmpLabelParams validation] input lbltype has bad value")
              }
             nameObject<-deparse(substitute(.Object))
             ##case ab
             if (lbltype == 'ab'){
                 .Object@lbloffs <- 0.25
                 .Object@lblcex <- 0.33
                 .Object@label2chart <- .Object@slabel
             }
             if (lbltype == 'dt'){
                 .Object@lbloffs <- 0.1
                 .Object@lblcex <- 0.50
                 ownMnth <- as.numeric(strftime( .Object@ownDatetime, format='%m'))
                 ownMnthchar <- .Object@monthdict[ownMnth]
                 ownDay <- as.numeric(strftime( .Object@ownDatetime, format='%d'))
                 ownWeekyr <- as.numeric(strftime( .Object@ownDatetime, format='%W'))
                 .Object@label2chart <- paste(ownDay,sep="")
                 if(.Object@dfrequency == 604800){
                  .Object@label2chart <- paste(ownWeekyr,sep="")
                 }
			 }
			 ##append ? for unconfirmed wfacts	  
			 if(!.Object@isconfirmed){
				 .Object@label2chart <-paste("?",.Object@label2chart,sep="")
		      } 
			 	 
             assign(nameObject,.Object,envir=parent.frame())
             return(invisible())
           })

   
   
   
###printing Wfact
setMethod('appendStory', 'Wfact'
          ,function(object,uhist=F,lflag=F){
                 buf <- ""
                 #yvalfmt <- formatC(object@yvalue, digits=4, format='f')
                 #buf <- paste(buf, "@" , yvalfmt,sep="")
                 if ( lflag ){  #long version
                   buf <- paste(buf,"Wyckoff:")
                   buf <- paste(buf, object@slabel )
                   buf <- paste(buf," (",object@llabel,") ", sep="")
				   if(!object@isconfirmed){
					 buf <-paste(buf," Candidate, needs confirmation!",sep="") 
				   }
                 }else{  #short version
                   buf <- paste(buf, object@slabel )
				   if(!object@isconfirmed){
					   buf <-paste(buf," [? tbc..]",sep="") 
				   }
                 }
				 if (uhist && !lflag){
					 buf <- paste(buf,"(uhist):",object@note) 
				 }				   
				 
                 buf <- paste(buf, ";",sep="")
                 return(buf)
           }
         )

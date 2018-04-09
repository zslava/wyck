#' Bpfact type
#'
#' Inherited from Artifact type. Fact non placeable on a chart to represent bar (session) properties. i.e. unusual progress,
#' range,change in Volume etc
#' Always have defined  a slot \code{oDateTime}.
#' Bpfacts are a part of Market story sequence of artifacts.
#'
#'
#' @keywords internal
#' @rdname Bpfact-class
#' @name Bpfact
#' @exportClass Bpfact
setClass(Class='Bpfact'
		,representation(yvalue='numeric'
				,content='character'
		)
		,prototype(yvalue=as.numeric(NA)
				,content=as.character(NA)
				,typestr='Bprop')
		,contains='Artifact'
)


##helper function to instantiate

#' Define Bpfact
#'
#' Constructor function to create new instance of Bpfact
#'
#' @param  iname character instrument name
#' @param  content character observation note
#' @param  oDateTime character string to associate Nfact to a date in format \code{%Y-%m-%d}
#' @param  dunit  character \code{'weeks'|'days'|'hours'|'minutes' }  for associated xts data periodicity
#' @param  dfreq  numeric  numbr of seconds in an associated xts data periodicity
#' @return  Bpfact instance
#' @keywords internal
#' @examples
#' \dontrun{bv <- defBpfact('CL', 'decreasing volume means selling pressure falls', '2011-12-14', 94.21, 'days', 86400)}
#' @export
defBpfact <-function(iname, content,oDateTime, yvalue, dunit, dfreq , tzvalue)
{
	if(missing(tzvalue)){ tzvalue <- "UTC" }
    dfmt <- "%Y-%m-%d %H:%M:%S"
    if(dfreq >= 86400) {dfmt <- "%Y-%m-%d" }
	o <- new('Bpfact'
			,instr=iname
			,ownDatetime=as.POSIXct(strftime(oDateTime, format=dfmt), tz=tzvalue)
			,yvalue=as.numeric(yvalue)
			,dunit=as.character(dunit)
			,dfrequency=as.numeric(dfreq) )
	o@slabel <- 'BPR'
	o@llabel <- 'Bar properties'
	o@content <- as.character(content)
	return(o)
}

##' @docType methods
##setGeneric( name='getContent',function(object){standardGeneric("getContent")})
#' @aliases getContent,Bpfact,ANY-method
setMethod('getContent', 'Bpfact'
		,function(object){
			return(object@content)
		})

##' @docType methods
##setGeneric(name='setContent<-',function(object,value){standardGeneric('setContent<-')})
#' @aliases setContent,Bpfact,ANY-method
setReplaceMethod('setContent','Bpfact'
		,function(object,value){
			##validate input
			if ( !is.character(value))  {
				stop("[Bpfact:setContent validation] input should be character string")
			}
			object@content <- value
			return (object)
		})

###printing Wfact
setMethod('appendStory', 'Bpfact'
		,function(object,uhist=F,lflag=F){
			buf <- ""
			if ( lflag ){  #long version
				buf <- paste(buf,object@llabel,":",sep="")
			}else{  #short version
				buf <- paste(buf, object@slabel,":",sep="" )
			}
			buf <- paste(buf, object@content )
			if(uhist && !lflag){
				buf <- paste(buf,"(uhist):",object@note)
			}
			buf <- paste(buf, ";",sep="")
			return(buf)
		}
)



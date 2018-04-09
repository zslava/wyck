#' Bvfact type
#'
#' Inherited from Artifact type. Fact non placeable on a chart to represent a bar interpretation note.
#' Always have defined  a slot \code{oDateTime}.
#' Bvfacts are a part of Market story sequence of artifacts.
#'
#'
#' @keywords internal
#' @rdname Bvfact-class
#' @name Bvfact
#' @exportClass Bvfact
setClass(Class='Bvfact'
		,representation(yvalue='numeric'
				,content='character'
		)
		,prototype(yvalue=as.numeric(NA)
				,content=as.character(NA)
				,typestr='View')
		,contains='Artifact'
)


##helper function to instantiate

#' Define Bvfact
#'
#' Constructor function to create new instance of Bvfact
#'
#' @param  iname character instrument name
#' @param  content character observation note
#' @param  oDateTime character string to associate Nfact to a date in format \code{%Y-%m-%d}
#' @param  dunit  character \code{'weeks'|'days'|'hours'|'minutes' }  for associated xts data periodicity
#' @param  dfreq  numeric  numbr of seconds in an associated xts data periodicity
#' @return  Bvfact instance
#' @keywords internal
#' @examples
#' \dontrun{bv <- defBvfact('CL', 'decreasing volume means selling pressure falls', '2011-12-14', 94.21, 'days', 86400)}
#' @export
defBvfact <-function(iname, content,oDateTime, yvalue, dunit, dfreq, tzvalue)
{
	if(missing(tzvalue)){ tzvalue <- "UTC" }
    dfmt <- "%Y-%m-%d %H:%M:%S"
    if(dfreq >= 86400) {dfmt <- "%Y-%m-%d" }	
	o <- new('Bvfact'
			,instr=iname
			,ownDatetime=as.POSIXct(strftime(oDateTime, format=dfmt), tz=tzvalue)
			,yvalue=as.numeric(yvalue)
			,dunit=as.character(dunit)
			,dfrequency=as.numeric(dfreq) )
	o@slabel <- 'BVW'
	o@llabel <- 'Bar view'
	o@content <- as.character(content)
	return(o)
}


##' @docType methods
## setGeneric( name='getContent',function(object){standardGeneric("getContent")})
#' @aliases getContent,Bvfact,ANY-method
setMethod('getContent', 'Bvfact'
		,function(object){
			return(object@content)
		})

##' @docType methods
##setGeneric(name='setContent<-',function(object,value){standardGeneric('setContent<-')})

#' @aliases setContent,Bvfact,ANY-method
setReplaceMethod('setContent','Bvfact'
		,function(object,value){
			##validate input
			if ( !is.character(value))  {
				stop("[Bvvact:setContent validation] input should be character string")
			}
			object@content <- value
			return (object)
		})


###printing Wfact
setMethod('appendStory', 'Bvfact'
		,function(object,uhist=F,lflag=F){
			buf <- ""
			if ( lflag ){  #long version
				buf <- paste(buf,"Bar view:")
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



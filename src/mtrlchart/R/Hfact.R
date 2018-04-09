#' Hfact type
#'
#' Inherited from Artifact type. Fact non placeable on a chart to represent a hypothesis
#' Hypos are of two types: Trigger (for trades) and Bias for general view
#' Always have defined  a slot \code{oDateTime}.
#' Hfacts are a part of Market story sequence of artifacts.
#'
#'
#' @keywords internal
#' @rdname Hfact-class
#' @name Hfact
#' @exportClass Hfact
setClass(Class='Hfact'
		,representation(yvalue='numeric'
				,content='character'
		)
		,prototype(yvalue=as.numeric(NA)
				,content=as.character(NA)
				,typestr='Hyp')
		,contains='Artifact'
)


##helper function to instantiate

#' Define Hfact
#'
#' Constructor function to create new instance of Hfact
#'
#' @param  iname character instrument name
#' @param  content character observation note
#' @param  oDateTime character string to associate Nfact to a date in format \code{%Y-%m-%d}
#' @param  dunit  character \code{'weeks'|'days'|'hours'|'minutes' }  for associated xts data periodicity
#' @param  dfreq  numeric  numbr of seconds in an associated xts data periodicity
#' @return  Hfact instance
#' @keywords internal
#' @examples
#' \dontrun{bv <- defHfact('CL', 'decreasing volume means selling pressure falls', '2011-12-14', 94.21, 'days', 86400)}
#' @export
defHfact <-function(iname, content,oDateTime, yvalue, dunit, dfreq, tzvalue )
{
	if(missing(tzvalue)){ tzvalue <- "UTC" }
    dfmt <- "%Y-%m-%d %H:%M:%S"
    if(dfreq >= 86400) {dfmt <- "%Y-%m-%d" }
    o <- new('Hfact'
			,instr=iname
			,ownDatetime=as.POSIXct(strftime(oDateTime, format=dfmt), tz=tzvalue)
			,yvalue=as.numeric(yvalue)
			,dunit=as.character(dunit)
			,dfrequency=as.numeric(dfreq) )
	o@slabel <- 'HYP'
	o@llabel <- 'Setup Hypothesis'
	o@content <- as.character(content)
	return(o)
}

##' @docType methods
#setGeneric( name='getContent',function(object){standardGeneric("getContent")})
#' @aliases getContent,Hfact,ANY-method
setMethod('getContent', 'Hfact'
		,function(object){
			return(object@content)
		})

##' @docType methods
#setGeneric(name='setContent<-',function(object,value){standardGeneric('setContent<-')})
#' @aliases setContent,Hfact,ANY-method
setReplaceMethod('setContent','Hfact'
		,function(object,value){
			##validate input
			if ( !is.character(value))  {
				stop("[Hfact:setContent validation] input should be character string")
			}
			object@content <- value
			return (object)
		})

###printing Wfact
setMethod('appendStory', 'Hfact'
		,function(object,uhist=F,lflag=F){
			buf <- ""
			if ( lflag ){  #long version
				buf <- paste(buf,object@llabel,": ",sep="")
			}else{  #short version
				buf <- paste(buf,"##", object@slabel,"##:",sep="" )
			}
			buf <- paste(buf, object@content )
			if(uhist && !lflag){
				buf <- paste(buf,"(uhist):",object@note)
			}
			buf <- paste(buf, ";",sep="")
			return(buf)
		}
)




#' Nfact type
#'
#' Inherited from Artifact type. Fact non placeable on a chart to represent an abritrary observation note.
#' Always have defined  a slot \code{oDateTime}.
#' Nfacts are a part of Market story sequence of artifacts.
#'
#'
#' @keywords internal
#' @rdname Nfact-class
#' @name Nfact
#' @exportClass Nfact
setClass(Class='Nfact'
         ,representation(yvalue='numeric'
                        ,content='character'
                         )
         ,prototype(yvalue=as.numeric(NA)
                   ,content=as.character(NA)
                   ,typestr='Note')
         ,contains='Artifact'
         )


##helper function to instantiate

#' Define Nfact
#'
#' Constructor function to create new instance of Nfact
#'
#' @param  iname character instrument name
#' @param  content character observation note
#' @param  oDateTime character string to associate Nfact to a date in format \code{%Y-%m-%d}
#' @param  dunit  character \code{'weeks'|'days'|'hours'|'minutes' }  for associated xts data periodicity
#' @param  dfreq  numeric  numbr of seconds in an associated xts data periodicity
#' @return  Nfact instance
#' @keywords internal
#' @examples
#' \dontrun{nfa <- defNobfact('CL', 'decreasing volume over last 5 days', '2011-12-14', 94.21, 'days', 86400)}
#' @export
defNobfact <-function(iname, content,oDateTime, yvalue, dunit, dfreq, tzvalue )
  {

    if(missing(tzvalue)){ tzvalue <- "UTC" }
    dfmt <- "%Y-%m-%d %H:%M:%S"
    if(dfreq >= 86400) {dfmt <- "%Y-%m-%d" }
   
    o <- new('Nfact'
             ,instr=iname
             ,ownDatetime=as.POSIXct(strftime(oDateTime, format=dfmt), tz=tzvalue)
             ,yvalue=as.numeric(yvalue)
             ,dunit=as.character(dunit)
             ,dfrequency=as.numeric(dfreq) )
    o@slabel <- 'NOB'
    o@llabel <- 'Note fact'
    o@content <- as.character(content)
  return(o)
}

#' copy all members from old object to new 
#'
#' @export 
copyNFact <-function ( onf) 
{
 nnf <- new("Nfact")

  nnf@yvalue       <-  onf@yvalue        
  nnf@content      <-  onf@content       
  nnf@id           <-  onf@id            
  nnf@instr        <-  onf@instr         
  nnf@typestr      <-  onf@typestr       
  nnf@slabel       <-  onf@slabel        
  nnf@llabel       <-  onf@llabel        
  nnf@color        <-  onf@color         
  nnf@ownDatetime  <-  onf@ownDatetime   
  nnf@note         <-  onf@note          
  nnf@dunit        <-  onf@dunit         
  nnf@dfrequency   <-  onf@dfrequency    
  nnf@monthdict    <-  onf@monthdict     

return(nnf)

}


#' @docType methods
setGeneric( name='getContent',function(object){standardGeneric("getContent")})
#' @aliases getContent,Nfact,ANY-method
setMethod('getContent', 'Nfact'
		,function(object){
			return(object@content)
		})

#' @docType methods
setGeneric(name='setContent<-',function(object,value){standardGeneric('setContent<-')})
#' @aliases setContent,Nfact,ANY-method
setReplaceMethod('setContent','Nfact'
		,function(object,value){
			##validate input
			if ( !is.character(value))  {
				stop("[Nfact:setContent validation] input should be character string")
			}
			object@content <- value
			return (object)
		})

###printing Wfact
setMethod('appendStory', 'Nfact'
          ,function(object,uhist=F,lflag=F){
                 buf <- ""
                 if ( lflag ){  #long version
                   buf <- paste(buf,"Note:")
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

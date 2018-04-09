# TODO: Add comment
# 
# Author: zimine
###############################################################################


#'  collection of market stories
#'
#' S4 container which holds market stories for instrument list
#' and associated methods
#' manage  artifacts  and to produce instrument charts
#' @rdname Wstorybook-class
#' @name Wstorybook
#' @exportClass Wstorybook
setClass(Class='Wstorybook'
		,representation(curwstoryid='numeric'
				       ,wstories='list')
		,prototype( curwstoryid=0
				   ,wstories=list())
  )
   

		
#' Initializes Wstorybook from a vector of instrument ids
#'
#' @export
initWstorybook <-function(instrvec){
	## add wstories for instrument list
	o <- new("Wstorybook", curwstoryid=1)
	for (j in  1:length(instrvec) ){
		ws <- wstory( instrvec[j] )
		addWstory(o) <- ws
	}
	return(o)	
	
}
			
#' repopulate wstorybook
#'
#' @export
repopulateWstorybook<-function( storylist){

 o <- new("Wstorybook", curwstoryid=1)

 
  wsitrs <- unlist(lapply(storylist , function(x) { x@instr } ))
  for( i in 1: length(wsitrs)){
  	 cinstr <- wsitrs[i]
     ofactsl <- storylist[[i]]@facts
     nfactsl <- list()
     nwstr <- wstory(cinstr) #create story
     nwstr@facts <- nfactsl ## set empty list
     ##cat("deb i: ", i, "\n")
     if( length(ofactsl) >0 ){
      for(j in 1:length(ofactsl)){
     	cfact <- ofactsl[[j]]
        ##cat("  deb j: ", j, "\n")
     	nfact <- switch( cfact@typestr,
                         "Wyckoff" = copyWfact(cfact),
                         "LevelLine" = copyLevelfact(cfact) ,
                         "TrendLine" = copyTrendFact(cfact),
                         "Note"      = copyNFact(cfact)
     	               )
     	nfactsl[[ length(nfactsl)+1 ]] <- nfact
      }
     nwstr@facts <- nfactsl  #set non empty list
     }
     ##add this story to the book
     o@wstories[[ length(o@wstories)+1  ]] <- nwstr
  }
  return(o)
}



#' @export
#' @docType methods
setGeneric(name='addWstory<-'
		,function(object,value){standardGeneric('addWstory<-')})
setReplaceMethod('addWstory'
		,'Wstorybook'
		,function(object,value){
			##validate input
			if ( !is(value, 'Wstory'))  {
				stop(paste("[Wstorybook:addStory validation]"
								," input value should be of type of Wstory"))
			}
			wsitrs <- unlist(lapply(object@wstories
							, function(x) { x@instr } ))
			idx <- match(value@instr , wsitrs)
			if(!is.na(idx) ){
				stop(paste("[Wstorybook:addWstory validation]"
								,"input wstory is already present in wstories list"))
			}
			lngh <-length(object@wstories)
			object@wstories[[lngh+1]] <- value
			return (object)
		})
#' @export
#' @docType methods
setGeneric(name='rmWstory',function(.Object, iname){standardGeneric('rmWstory')})
#' @aliases rmWstory,Wstorybook,ANY-method
setMethod('rmWstory','Wstorybook'
		,function(.Object,iname){
			nameObject <- deparse(substitute(.Object))
			wsinames <-  unlist(lapply(.Object@wstories , function(x){ x@instr } ))
			idx <- match(iname, wsinames)
			if ( !is.na(idx)) {
				.Object@wstories[[idx]] <- NULL
			}else{
				print(paste("Wstorybook:rmWstory validation"
								,"warning. story with instrument name",iname
								,"was not present the wstories list." ))
			}
			assign(nameObject,.Object,envir=parent.frame())
			return ( invisible() )
		})

#' @export
#' @docType methods
setGeneric(name='updateStory<-'
		,function(object,value){standardGeneric('updateStory<-')})
setReplaceMethod('updateStory'
		,'Wstorybook'
		,function(object,value){
			##validate input
			if(!is.Wstory(value)){
				stop(paste("[Wstorybook:updateStory validation]"
								,"param value expected to be of type Wstory"))
			}
			wsitrs <- unlist(lapply(object@wstories
							, function(x) { x@instr } ))
			idx <- match(value@instr , wsitrs)
			if(is.na(idx) ){
				stop(paste("[Wstorybook:updStory validation]"
								,"input wstory is missing from wstories list"))
			}
			object@wstories[[idx]] <- value
			object@curwstoryid <- idx
			return(object)
		})

#' @export
#' @docType methods
setGeneric(name='getStory'
		,function(object,instname){standardGeneric('getStory')})
#' @aliases getCurrentstory,Wstorybook,ANY-method
setMethod('getStory'
		,'Wstorybook'
		,function(object,instname){
			wsitrs <- unlist(lapply(object@wstories
							, function(x) { x@instr } ))
			idx <- match( instname,wsitrs)
			if (!is.na(idx)){ ret <- object@wstories[[ idx]] }
			else            { ret <- as.numeric(NA) }
			#if instrument undefined, return first story
			if( is.na(instname)){
				ret <- object@wstories[[1]]
			}
			return ( ret )
		})

#' @export
#' @docType methods
setGeneric(name='getLastupdatedstory'
		,function(object){standardGeneric('getLastupdatedstory')})
#' @aliases getLastupdatedstory,Wstorybook,ANY-method
setMethod('getLastupdatedstory'
		,'Wstorybook'
		,function(object){
			ret <- object@wstories[[ object@curwstoryid]] 
			return ( ret )
		})

##bulk replace wyk label  , admin op rarely used ##
#' @export
#' @docType methods
setGeneric(name='globalreplaceWyklbl',function(.Object,oslb,nslb,nllb)
		{standardGeneric('globalreplaceWyklbl')})
#' @aliases globalreplaceWyklbl,Wstorybook,ANY-method
setMethod('globalreplaceWyklbl','Wstorybook'
		,function(.Object,oslb,nslb,nllb){
			nameObject <- deparse(substitute(.Object))
			fcount <- 0
			for (j in 1:length(.Object@wstories)){
				cstory <- .Object@wstories[[j]]
				cinstr <- getSinstrument(cstory)
				slbls <- unlist(lapply(cstory@facts, function(x) { x@slabel } ))
				sidx <- which(slbls == oslb)
				fcount <- fcount + length(sidx)
			}
			if ( fcount > 0 ){
				qa<-readline(paste('total labels found',fcount
								,'are you sure you want to replace them? (y/n)'))
				if ( qa =="y"){
					for (j in 1:length(.Object@wstories)){
						cstory <- .Object@wstories[[j]]
						cinstr <- getSinstrument(cstory)
						slbls <- unlist(lapply(cstory@facts, function(x) { x@slabel } ))
						sidx <- which(slbls == oslb)
						if(length(sidx)>0){
							for (k in 1:length(sidx) ){
								cid <- sidx[k]
								cfact <- getFact(cstory,cid)
								cfact@slabel <- nslb
								cfact@llabel <- nllb
								setFact(cstory,cid)<- cfact
							}
							cat ("instrument",cinstr,"bulk replaced labels in ",length(sidx), "facts \n" )
						}
						updStory(.Object) <- cstory
					}
					persist(.Object)  #saves to rdat file
				}else{ cat("skipped bulk replacement\n") }
			}
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		})
 
### set TZ for  daily, weekly data
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='setFactsTZ'
		,function(.Object,tz="UTC",dunit="days", dfrequency=c(86400,604800) ){standardGeneric('setFactsTZ')})
#' @aliases setFactsTZ,Wstorybook,ANY-method
setMethod('setFactsTZ','Wstorybook'
		,function(.Object,tz="UTC",dunit="days", dfrequency=c(86400,604800) ) {
			nameObject <- deparse(substitute(.Object))

			#internal funcs
			setStoryTZ <- function(x,tzval,dunit,dfreq){
				ufacts <- lapply(x@facts, FUN=setFactTZ,tzfval=tzval,dunit=dunit,dfreq=dfreq)
				x@facts <- ufacts
				return(x)
			}
			setFactTZ <- function(x,tzfval,dunit,dfreq){
				#ensure to modify facts only for a wanted time frame
			    if(x@dunit != dunit || !(x@dfrequency %in% dfreq) ) { return (x)} 			
				x@ownDatetime <- forceTz(x@ownDatetime, x@dfrequency,tzfval) # true for all facts
                x <-switch(x@typestr,
                			"LevelLine" = tzLevelFact(x,ftz=tzfval)
                			,"TrendLine" = tzTrendFact(x,ftz=tzfval)
                			,x
                	      )
				return(x)
			}
			tzLevelFact <-function(x, ftz){
				x@leftDatetime 		<- forceTz(x@leftDatetime, x@dfrequency,ftz)
				x@showLeftDatetime 	<- forceTz(x@showLeftDatetime, x@dfrequency,ftz)
				x@showRightDatetime <- forceTz(x@showRightDatetime, x@dfrequency,ftz)
				return(x)
			}
			tzTrendFact <-function(x, ftz){
				x@leftDatetime 		<- forceTz(x@leftDatetime, x@dfrequency,ftz)
				x@rightDatetime		<- forceTz(x@rightDatetime, x@dfrequency,ftz)
				x@showLeftDatetime 	<- forceTz(x@showLeftDatetime, x@dfrequency,ftz)
				x@showRightDatetime <- forceTz(x@showRightDatetime, x@dfrequency,ftz)
				return(x)
			}
            #internal funcs end
			uwstories <- lapply(.Object@wstories, FUN=setStoryTZ,tzval=tz,dunit=dunit,dfreq=dfrequency)

			.Object@wstories <- uwstories
			assign(nameObject,.Object,envir=parent.frame())
			return(invisible())
		})

########biz methods###########
##' @export
##' @docType methods
#setGeneric(name='getWstoriesinstruments'
#		,function(object){standardGeneric('getWstoriesinstruments')})
##' @aliases getWstoriesinstruments,Wstorybook,ANY-method
#setMethod('getWstoriesinstruments'
#		,'Wstorybook'
#		,function(object){
#			wsitrs <- unlist(lapply(object@wstories
#							, function(x) { x@instr } ))
#			if(is.null(wsitrs)) { wsitrs <- as.character(NA) }
#			return ( wsitrs )
#		})
#
##return instrument of current story
##' @export
##' @docType methods
#setGeneric(name='getCurinstr'
#		,function(object){standardGeneric('getCurinstr')})
##' @aliases getCurinstr,Wstorybook,ANY-method
#setMethod('getCurinstr'
#		,'Wstorybook'
#		##get it from associated Mldata
#		,function(object){
#			if (is.na( getCinstrument(object@mdata))){
#				return ( getWstoriesinstruments(object)[object@curwstoryid] )
#			}else{
#				return ( getCinstrument(object@mdata)  )
#			}
#		})


##' @export
##' @docType methods
#setGeneric(name='getSpecificStory'
#		,function(object,iname){standardGeneric('getSpecificStory')})
##' @aliases getSpecificStory,Wstorybook,ANY-method
#setMethod('getSpecificStory'
#		,'Wstorybook'
#		,function(object,iname){
#			wsitrs <- unlist(lapply(object@wstories
#							, function(x) { x@instr } ))
#			idx <- match(iname, wsitrs)
#			if ( !is.na(idx)){
#				return ( object@wstories[[idx]] )
#			}else { return (NA) }
#		})

##' @export
##' @docType methods
#setGeneric(name='setInstrument4curstory<-'
#		,function(object,value){
#			standardGeneric('setInstrument4curstory<-')
#		})
#setReplaceMethod('setInstrument4curstory'
#		,'Wstorybook'
#		,function(object,value){
#			##validate input
#			wsitrs <- unlist(lapply(object@wstories
#							, function(x) { x@instr } ))
#			if ( is.na(match(value,wsitrs)))  {
#				stop(paste("[Wstorybook:setCurStory validation]"
#								," input instrument name is missing"
#								,"from stories list"))
#			}
#			# case mdata is defined ( current case )
#			if (!is.na( getCinstrument(object@mdata))){
#				mldat <- object@mdata
#				setSpecificInstrumentPosition(mldat,value)
#				object@mdata <- mldat
#			}
#			#case   mdata is not defined (legacy case)
#			object@curwstoryid <- match(value, wsitrs)
#			return (object)
#		})
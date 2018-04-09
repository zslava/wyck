# TODO: think to add WstoryBook  objet
# Author: zimine
###############################################################################

#' Wstory type for Instrument story
#'
#' Contains a dated sequence of Facts, placeable or non placeable on a chart
#'
#' @keywords internal
#' @rdname Wstory-class
#' @name Wstory
#' @exportClass Wstory
setClass(Class='Wstory'
         ,representation(instr='character'
                         ,facts='list'
                        )
         ,prototype(instr='undefined'
                   ,facts=list()
                   )
         )


##helper function  for construction
#' Initialized Wstory
#'
#' Constructor function to create new instance of Wstory
#'
#' @return  Wstory instance
#' @keywords internal
#' @examples
#' \dontrun{ws <- wstory()}
#' @export 
wstory <-function(iname){

  o <- new('Wstory', instr=iname)
  return(o)
}



####biz methods########
#' @export
#' @docType methods
setGeneric(name='getSinstrument'
               ,function(object){standardGeneric('getSinstrument')})
#' @aliases getSinstrument,Wstory,ANY-method
setMethod('getSinstrument'
                 ,'Wstory'
                 ,function(object){
                     return ( object@instr )
                 }
          )
#### Artifact management methods
#' @export
#' @docType methods
setGeneric(name='getFact'
               ,function(object,index){standardGeneric('getFact')})
#' @aliases getFact,Wstory,ANY-method
setMethod('getFact'
                 ,'Wstory'
                 ,function(object,index){
                     ##validate input
                     if ( !is.numeric(index)
                         || index <=  0
                         || index > length(object@facts) )  {
                          stop(paste("[Wstory:getFact validation]"
                                    ,"index out of border"))
                     }
                     return ( object@facts[[index]] )
                 })
		  
#' append  an artifact to the end of the artifacts list
#' @export
#' @docType methods
setGeneric(name='addFact<-'
               ,function(object,value){standardGeneric('addFact<-')})
setReplaceMethod('addFact'
                 ,'Wstory'
                 ,function(object,value){
                     ##validate input
                     if ( !is(value, 'Artifact'))  {
                          stop(paste("[Wstory:addFact validation]"
                                    ," input should be a sub-type of Artifact"))
                     }
                     lngh <-length(object@facts)
                     object@facts[[lngh+1]] <- value
                     return (object)
                 })

#' @export
#' @docType methods
setGeneric(name='setFact<-'
               ,function(object,index,value){standardGeneric('setFact<-')})
setReplaceMethod('setFact'
                 ,'Wstory'
                 ,function(object,index,value){
                     ##validate input
                     if ( !is(value, 'Artifact'))  {
                          stop(paste("[Wstory:setFact validation]"
                                    ," input should be a sub-type of Artifact"))
                     }
                     if ( !is.numeric(index)
                         || index <=  0
                         || index > length(object@facts) )  {
                          stop(paste("[Wstory:setFact validation]"
                                    ,"index out of border"))
                     }
                     object@facts[[index]] <- value
                     return (object)
                 }
                )


#' @export
#' @docType methods
setGeneric(name='delFact'
               ,function(.Object,index){standardGeneric('delFact')})
#' @aliases delFact,Wstory,ANY-method
setMethod('delFact'
                 ,'Wstory'
                 ,function(.Object,index){
                     ##validate input
                     if ( !is.numeric(index)
                         || index <=  0
                         || index > length(.Object@facts) )  {
                          stop(paste("[Wstory:delFact validation]"
                                    ,"index out of border"))
                     }
                     nameObject <- deparse(substitute(.Object))
                     .Object@facts[[index]] <- NULL
                     assign(nameObject,.Object,envir=parent.frame())
                     return (invisible())
})

			


#' @docType methods
setGeneric(name='findArtifact'
               ,function(object,dtvalue,dfrequency,tzvalue="UTC"){
                 standardGeneric('findArtifact')
               })
#' @aliases findArtifact,Wstory,ANY-method
setMethod(f='findArtifact'
         ,signature='Wstory'
         ,def=function(object,dtvalue,dfrequency,tzvalue="UTC"){
         ##validation
         if ( missing(dtvalue) || missing(dfrequency)) {
              stop(paste("[Wstory:fetchArtifact validation]"
                        ," bad value of tfvalue param"))
         }
         dfmt<-"%Y-%m-%d %H:%M:%S"
         if(dfrequency >= 86400) dfmt<-"%Y-%m-%d"
         dtposixv <- as.POSIXct(strftime(dtvalue,format=dfmt),tz=tzvalue)
         maskfunc <- function(x,dtposix,dfreq){
              if(   !is.na(x@ownDatetime) && x@ownDatetime == dtposix 
                 && !is.na(x@dfrequency)  && x@dfrequency == dfreq){ 
                return (1)
              }else{ 
                return(0)
              }
         }
         lng <- length(object@facts)
         fmask <- unlist(lapply(object@facts, FUN=maskfunc, dtposix=dtposixv,dfreq=dfrequency))
         midx <- 1:lng * fmask
         matchidx <- midx[midx>0]
         #browser()
         if (length(matchidx) == 0) { return (as.numeric(NA)) }
         return(matchidx)
})

#' @docType methods
setGeneric(name='getArtifact'
               ,function(object,slabvalue,dtvalue,dfrequency,tzvalue="UTC"){
                 standardGeneric('getArtifact')
               })
#' @aliases getArtifact,Wstory,ANY-method
setMethod(f='getArtifact'
         ,signature='Wstory'
         ,def=function(object,slabvalue,dtvalue,dfrequency,tzvalue="UTC"){

         matchdateIdx <- findArtifact(object, dtvalue=dtvalue,dfrequency=dfrequency,tzvalue=tzvalue) 
         labels <- unlist(lapply(object@facts, function(x){x@slabel}))
         lblidx <- which(labels == slabvalue)
         if(length(lblidx)==0) { return (as.numeric(NA)) }
         
         bothMatchIdx <- match(TRUE, matchdateIdx %in% lblidx)
         foundIdx <- as.numeric(NA)
         if(!is.na(bothMatchIdx)) {foundIdx = matchdateIdx[bothMatchIdx] }        
         return(foundIdx)

})

#' @docType methods
setGeneric(name='getFactIndex'
               ,function(object,fact){
                 standardGeneric('getFactIndex')
               })
#' @aliases getFactIndex,Wstory,ANY-method
setMethod(f='getFactIndex'
         ,signature='Wstory'
         ,def=function(object,fact){
         ##validation
         if (!is(fact, 'Artifact')){
              stop(paste("[Wstory:getFactIndex validation]"
                        ,"fact param is not of type Artifact"))
         }
         idv <-unlist(lapply(object@facts
                            ,function(x,f){
                              if(   x@slabel      == f@slabel 
                                 && x@ownDatetime == f@ownDatetime
                                 && x@dfrequency  == f@dfrequency
                                 && x@instr       == f@instr )
                                    return (1)
                              else  return(0)      
                            }
                      ,f=fact))
         return( match(1, idv))
})


#' @docType methods
setGeneric(name='getArtitype'
               ,function(object,typvalue,tfvalue){standardGeneric('getArtitype')})
#' @aliases getArtiType,Wstory,ANY-method
setMethod(f='getArtitype'
         ,signature='Wstory'
         ,def=function(object,typvalue,tfvalue){
                     ##validation
                     if (missing(typvalue)){
                          stop(paste("[Wstory:getArtitype validation]"
                                    ,"missing artifact type param"))

                     }
					 okValues <- c('Wyckoff'
								   ,'TrendLine'
								   ,'LevelLine'
                   ,'ZoneLine'
								   ,'Note'
                   ,'View'
                   ,'Bprop'
                   ,'Hyp')
						   
					 if (is.na( match(typvalue,okValues))) {	   
                          stop(paste("[Wstory:getArtitype validation]"
                                    ," bad value of Artifact type"))
                     }
                     if (missing(tfvalue)){
                          stop(paste("[Wstory:getArtitype validation]"
                                    ,"missing artifact time frame periodicity"))
                     }
					 okTfvalues <- c("w1","d1","h1","h2","h4", "m30","m15","m10","m5","m3","m1")
					 if(is.na(match(tfvalue,okTfvalues))){
                          stop(paste("[Wstory:getArtitype validation]"
                                    ," bad tfvalue of artifact time frame periodicity label"))
                     }

                     lng <- length(object@facts)
                     vtypes <- unlist(lapply(object@facts, function(x) { x@typestr } ))
                     msk <- ifelse( vtypes == typvalue , 1, NA )
                     midx <- 1:lng * msk
                     midx <- midx[!is.na(midx)]
                     if (length(midx) == 0 ) {  ## no wfacts of typvalue
                       return (list() )
                     }else{
                     ## 2 sort on ownDate
                       typefacts <- object@facts[midx]
                       vdates <- unlist(lapply(typefacts, function(x) { x@ownDatetime } ))
                       sorted <- order(vdates, decreasing=F)
                       sortedFacts <- typefacts[sorted]
                       ## 3 apply mask on time frame
                       slng <- length(sortedFacts)
                       df <- NA
                       df[ tfvalue %in% 'w1' ] <- 604800
                       df[ tfvalue %in% 'd1' ] <- 86400
                       df[ tfvalue %in% 'h1' ] <- 3600
                       df[ tfvalue %in% 'h2' ] <- 7200
                       df[ tfvalue %in% 'h4' ] <- 14400
                       df[ tfvalue %in% 'm30' ] <- 1800
                       df[ tfvalue %in% 'm15' ] <- 900
                       df[ tfvalue %in% 'm10' ] <- 600
                       df[ tfvalue %in% 'm5' ] <- 300
                       df[ tfvalue %in% 'm3' ] <- 180
                       df[ tfvalue %in% 'm1' ] <- 60

                       dfreq <- unlist(lapply(sortedFacts, function(x) { x@dfrequency } ))
                       msk <- ifelse( dfreq == df , 1, NA )
                       fidx <- 1:slng * msk
                       fidx <- fidx[!is.na(fidx)]
                       if (length(fidx ) == 0 ) { ## no wfacts of tfvalue time frame
                          return ( list() )
                        }else{
                          return( sortedFacts[fidx] )
                        }
                     }
                 })
#' @export
#' @docType methods
setGeneric(name='getWfacts'
               ,function(object,tfvalue){standardGeneric('getWfacts')})
#' @aliases getWfacts,Wstory,ANY-method
setMethod('getWfacts'
                 ,'Wstory'
                 ,function(object,tfvalue){
                     ctype <- 'Wyckoff'
                     cwfs <- getArtitype(object, ctype, tfvalue )
                     return ( cwfs )
                 })


#' @export
#' @docType methods
setGeneric(name='getWfactsPeriod'
               ,function(object,tfvalue, dtperiod){standardGeneric('getWfactsPeriod')})
#' @aliases getWfactsPeriod,Wstory,ANY-method
setMethod('getWfactsPeriod'
                 ,'Wstory'
                 ,function(object,tfvalue,dtperiod){
                     if(missing(dtperiod)){
                        stop(paste("[Wstory:getWfactsPeriod validation]"
                          ,"missing paramter dtperiod"))
                     }
                     fullwfs <- getWfacts(object,tfvalue=tfvalue)
                     limDates <- as.POSIXct(unlist(strsplit(dtperiod,"::")))
                     if(length(limDates)==0){
                        stop(paste("[Wstory:getWfactsPeriod validation]"
                          ,"dtperiod param is not formatted as date1::date2"))
                     }
                     idx <- unlist(lapply(fullwfs, function(x,dtLeft, dtRight){
                                                x@ownDatetime >= dtLeft && 
                                                x@ownDatetime <= dtRight
                                              },dtLeft=limDates[1],dtRight=limDates[2]))
                     #browser()
                     fltwfs <- fullwfs[which(idx==TRUE)]
                     return ( fltwfs )
                 })


#' @docType methods
setGeneric(name='getNotefacts'
               ,function(object,tfvalue){standardGeneric('getNotefacts')})
#' @aliases getNotefacts,Wstory,ANY-method
setMethod('getNotefacts'
                 ,'Wstory'
                 ,function(object,tfvalue){
                     ctype <- 'Note'
                     cwfs <- getArtitype(object, ctype, tfvalue )
                     return ( cwfs )
                 })

#' @docType methods
setGeneric(name='getBviewfacts'
				 ,function(object,tfvalue){standardGeneric('getBviewfacts')})
#' @aliases getNotefacts,Wstory,ANY-method
setMethod('getBviewfacts'
				 ,'Wstory'
				 ,function(object,tfvalue){
					 ctype <- 'View'
					 cwfs <- getArtitype(object, ctype, tfvalue )
					 return ( cwfs )
				 })		 
#' @docType methods
 setGeneric(name='getBpropfacts'
				 ,function(object,tfvalue){standardGeneric('getBpropfacts')})
#' @aliases getNotefacts,Wstory,ANY-method
 setMethod('getBpropfacts'
				 ,'Wstory'
				 ,function(object,tfvalue){
					 ctype <- 'Bprop'
					 cwfs <- getArtitype(object, ctype, tfvalue )
					 return ( cwfs )
				 })		
		 
#' @docType methods
setGeneric(name='getHypfacts'
				 ,function(object,tfvalue){standardGeneric('getHypfacts')})
#' @aliases getNotefacts,Wstory,ANY-method
 setMethod('getHypfacts'
				 ,'Wstory'
				 ,function(object,tfvalue){
					 ctype <- 'Hyp'
					 cwfs <- getArtitype(object, ctype, tfvalue )
					 return ( cwfs )
				 })		
		 
#' @export
#' @docType methods
setGeneric(name='getTrlfacts'
               ,function(object,tfvalue){standardGeneric('getTrlfacts')})
#' @aliases getTrlfacts,Wstory,ANY-method
setMethod('getTrlfacts'
                 ,'Wstory'
                 ,function(object,tfvalue){
                     ctype <- 'TrendLine'
                     cwfs <- getArtitype(object, ctype , tfvalue )
                     return ( cwfs )
                 })
#' @export
#' @docType methods
setGeneric(name='getLvlfacts'
               ,function(object,tfvalue){standardGeneric('getLvlfacts')})
#' @aliases getLvlfacts,Wstory,ANY-method
setMethod('getLvlfacts'
                 ,'Wstory'
                 ,function(object,tfvalue){
                     ctype <- 'LevelLine'
                     cwfs <- getArtitype(object, ctype, tfvalue )
                     return ( cwfs )
                 })
		 
#' @export
#' @docType methods
setGeneric(name='getZnfacts'
               ,function(object,tfvalue){standardGeneric('getZnfacts')})
#' @aliases getZnlfacts,Wstory,ANY-method
setMethod('getZnfacts'
                 ,'Wstory'
                 ,function(object,tfvalue){
                     ctype <- 'ZoneLine'
                     cwfs <- getArtitype(object, ctype, tfvalue )
                     return ( cwfs )
                 })
		 

#' @docType methods
setGeneric(name='getAllIntradayTypefacts'
               ,function(object,ctype, time_limit=-1){standardGeneric('getAllIntradayTypefacts')})
#' @aliases getAllIntradayTuypefacts,Wstory,ANY-method
setMethod('getAllIntradayTypefacts','Wstory'
        ,function(object,ctype,time_limit=-1){
            cfs <- c(getArtitype(object,ctype,'h4')
                   ,getArtitype(object,ctype,'h2')
                   ,getArtitype(object,ctype,'h1')
                   ,getArtitype(object,ctype,'m30')
                   ,getArtitype(object,ctype,'m15')
                   ,getArtitype(object,ctype,'m10')
                   ,getArtitype(object,ctype,'m5')
                   ,getArtitype(object,ctype,'m3')
                   ,getArtitype(object,ctype,'m1'))
            rfs <-cfs 
            if (time_limit != -1){
              msk <- unlist(lapply(cfs,function(x,ltime){
                                        if (x@ownDatetime <= ltime)
                                        return(1) else return(NA)}
                            ,ltime=time_limit))
              idx_v <- which(msk ==1)
              rfs <- cfs[idx_v]
            } 
           return (rfs)        
})
		 
#' @export
#' @docType methods
setGeneric(name='bulkreplaceWyklabel'
               ,function(.Object, oslb,nslb,nllb){standardGeneric('bulkreplaceWyklabel')})
#' @aliases bulkreplaceWyklabel,Wstory,ANY-method
setMethod(f='bulkreplaceWyklabel'
         ,signature='Wstory'
         ,def=function(.Object, oslb,nslb,nllb){
            nameObject <- deparse(substitute(.Object))
            slbls <- unlist(lapply(.Object@facts, function(x) { x@slabel } ))
            sidx <- which(slbls == oslb)
            if(length(sidx)>0){
              for (j in 1:length(sidx) ){
                cid <- sidx[j]
                cfact <- .Object@facts[[cid]]
                cfact@slabel <- nslb
                cfact@llabel <- nllb
                .Object@facts[[cid]] <- cfact
              }
              #cat ("bulk replaced labels in ",length(sidx), "facts \n" )
            }

            assign(nameObject,.Object,envir=parent.frame())
           return (invisible())

         })

#' @export
#' @docType methods
setGeneric(name='mergeArtifacts4story'
               ,function(object,tfvalue,tex=F){standardGeneric('mergeArtifacts4story')})
#' @aliases mergeArtifacts4story,Wstory,ANY-method
setMethod(f='mergeArtifacts4story'
         ,signature='Wstory'
         ,def=function(object,tfvalue,tex=F){
           ##validation
           if (missing(tfvalue)){
                stop(paste("[Wstory:sortArtitypeTimeFrame validation]"
                          ,"missing artifact time frame periodicity"))
           }

           storyfacts <- c()
           ### various groups of facts  to be merged here
           #merge wfs , bprops, views and hyps
           wfacts <- getWfacts(object,tfvalue)
           if (length(wfacts) > 0 ) { storyfacts <- c(storyfacts, wfacts) }
           nfacts <- getNotefacts(object, tfvalue)
           if (length(nfacts) > 0 ) { storyfacts <- c(storyfacts, nfacts) }
					 bfacts <- getBpropfacts(object,tfvalue)
					 if (length(bfacts) > 0 ) { storyfacts <- c(storyfacts, bfacts) }
					 vfacts <- getBviewfacts(object,tfvalue)
					 if (length(vfacts) > 0 ) { storyfacts <- c(storyfacts, vfacts) }
					 #if(!tex){  ## no hyp facts in tex report
					  hfacts <- getHypfacts(object,tfvalue)  # make appear hyp facts in the tex report
					  if (length(hfacts) > 0 ) { storyfacts <- c(storyfacts, hfacts) }
				   # }
           ## sort on ownDate
           if ( length(storyfacts) > 0 ){
            vdates <- unlist(lapply(storyfacts, function(x) { x@ownDatetime } ))
            sorted <- order(vdates, decreasing=F)
            sortedStoryFacts <- storyfacts[sorted]
           }else{ sortedStoryFacts <- storyfacts }
           return(sortedStoryFacts)
       })
		 
#' @export
#' @docType methods
setGeneric(name='printStory'
               ,function(object,tfvalue='d1',limitdate,xtsdata,ndays=90,uhist=F,lflag=F){
		  standardGeneric('printStory')
	  })
#' @aliases printStory,Wstory,ANY-method
setMethod(f='printStory'
         ,signature='Wstory'
         ,def=function(object,tfvalue='d1',limitdate,xtsdata,ndays=90,uhist=F,lflag=F){
            cmaxDt <- as.POSIXct('1000-01-01')
            storyBuf <- paste("Instrument:",getSinstrument(object)," timeframe:",tfvalue,". ",sep="")
            storyFacts <- mergeArtifacts4story(object, tfvalue) ##type of facts to appear in story
            if(length(storyFacts) == 0 ){
				      print("No artefacts defined for current instrument.")
				      return(invisible)
			       }
            if(missing(limitdate)){
                 limitdate <- as.POSIXct(getOwndtimeStr( storyFacts[[ length(storyFacts) ]] ))
             }
             xtz <- indexTZ(xtsdata)
			       ##find left right limit dates to print facts between 
             lastdate <- as.POSIXct(limitdate,tz=xtz)
             firstdate <- lastdate - 86400 * ndays
             #factsdates <-  as.POSIXct(unlist(lapply(storyFacts, function(x){getOwndtimeStr(x)})) )
             factsdates <-  unlist(lapply(storyFacts, function(x){getOwndtime(x)})) 
             m1 <-ifelse(factsdates >= firstdate,1,NA)
             m2 <-ifelse(factsdates <= lastdate,1,NA)
             msk <-m1*m2
             mskidx <- which(msk==1 )
             storyFacts <- storyFacts[mskidx]  ##cut storyfacts outside show window
			 
             if(length(storyFacts) == 0 ){
                 print(paste("No artefacts found between", firstdate, "and",lastdate))
                 return(invisible)
             }
			 dtfcnt <- 0  ##counter of  facts on the same date
             for( j in 1:length(storyFacts) ){
              cfact <- storyFacts[[j]]
              cownDt <- as.POSIXct(getOwndtimeStr(cfact) )
              nwk <-  strftime(cownDt, format="%W")
              if (cownDt > cmaxDt ) { ## moment to print new date
                cmaxDt  <- cownDt ##
				dtfcnt <- 0
                if (lflag ) {
                 storyBuf <- paste(storyBuf,"\n", "Week ",nwk," ",sep="")
                 storyBuf <- paste(storyBuf,getOwndtimeStr(cfact),sep="")
                }else{
                   storyBuf <- paste(storyBuf,"\n", "wk#",nwk," ",sep="")
                   storyBuf <- paste(storyBuf, getShortnoteOwndtime(cfact), sep="" )
                   techdt <- getTechdt(cfact,xtsdata) # technical date for only daily artefacts
                   if ( !is.na(techdt) ){
                     storyBuf <- paste(storyBuf, "[",techdt,"]",sep="" )
                   }
                }
              }
			  if(dtfcnt>0){
				  storyBuf <-paste(storyBuf,"\n                   ")  ##put same date fact on new line
			  }
              storyBuf <- paste(storyBuf, appendStory(cfact,uhist,lflag),sep=" ")
			  dtfcnt <- dtfcnt + 1
             } ##loop over storyfacts
            storyBuf <- paste(storyBuf, "\n")
            cat(storyBuf)
            return(invisible())
         })

#' @export
#' @docType methods
		 setGeneric(name='printlatexStory'
				 ,function(object,tfvalue='d1',limitdate,xtsdata,ndays=90,uhist=F,lflag=F){
					 standardGeneric('printlatexStory')
				 })
#' @aliases printStory,Wstory,ANY-method
		 setMethod(f='printlatexStory'
				 ,signature='Wstory'
				 ,def=function(object,tfvalue='d1',limitdate,xtsdata,ndays=90,uhist=F,lflag=F){
					 
					 ##internal func to fix latex special chars 
					 latexspechars<-function(str){
						 str<-gsub('#','\\#', str, fixed=T)
						 str<-gsub('&','\\&', str, fixed=T)
						 str<-gsub('%','\\%', str, fixed=T)
						 str<-gsub('_','\\_', str, fixed=T)
						 return(str)
					 }
					 cmaxDt <- as.POSIXct('1000-01-01')
					 storyBuf <-""
					 storyBuf<- paste(storyBuf,'\\begin{table}[ht]\n')
					 storyBuf <- paste(storyBuf,"\\caption{Instrument:",getSinstrument(object)," timeframe:",tfvalue,"}\n",sep="")
					 
					 storyFacts <- mergeArtifacts4story(object, tfvalue,tex=TRUE) ##type of facts to appear in story
					 if(length(storyFacts) == 0 ){
						 cat("No artefacts defined for current instrument.\n")
						 return(invisible)
					 }
					 if(missing(limitdate)){
						 limitdate <- as.POSIXct(getOwndtimeStr( storyFacts[[ length(storyFacts) ]] ))
					 }
					 ##find left right limit dates to print facts between 
					 lastdate <- as.POSIXct(limitdate)
					 firstdate <- lastdate - 86400 * ndays
					 factsdates <-  as.POSIXct(unlist(lapply(storyFacts, function(x){getOwndtimeStr(x)})) )
					 m1 <-ifelse(factsdates >= firstdate,1,NA)
					 m2 <-ifelse(factsdates <= lastdate,1,NA)
					 msk <-m1*m2
					 mskidx <- which(msk==1 )
					 storyFacts <- storyFacts[mskidx]  ##cut storyfacts outside show window
					 
					 if(length(storyFacts) == 0 ){
						 print(paste("No artefacts found between ", firstdate, " and ",lastdate,sep=""))
						 return(invisible)
					 }
					 storyBuf<-paste(storyBuf,'\\begin{tabularx}{1\\linewidth}{|l|X|}\n')
					 storyBuf<-paste(storyBuf,'\\hline\n')
					 dtfcnt <- 0  ##counter of  facts on the same date
					 for( j in 1:length(storyFacts) ){
						 cfact <- storyFacts[[j]]
						 cownDt <- as.POSIXct(getOwndtimeStr(cfact) )
						 nwk <-  strftime(cownDt, format="%W")
             wd  <-  strftime(cownDt, format="%a")
						 if (cownDt > cmaxDt ) { ## moment to print new date
							 cmaxDt  <- cownDt ##
							 dtfcnt <- 0
							 storyBuf<-paste(storyBuf,"\\hline\n")						 
							 if (lflag ) {
								 #storyBuf <- paste(storyBuf,"w,\\#",nwk," ",sep="")
                 storyBuf <- paste(storyBuf,"w",nwk," ",wd," ",sep="")
								 storyBuf <- paste(storyBuf,getOwndtimeStr(cfact)," &",sep="")
							 }else{
								 storyBuf <- paste(storyBuf,"w\\#",nwk," ",sep="")
								 storyBuf <- paste(storyBuf, getShortnoteOwndtime(cfact), sep="" )
								 techdt <- getTechdt(cfact,xtsdata) # technical date for only daily artefacts
								 if ( !is.na(techdt) ){
									 storyBuf <- paste(storyBuf, "[",techdt,"]",sep="" )
								 }
								 storyBuf<-paste(storyBuf,"&")
							 }
						 }
						 if(dtfcnt>0){
							 storyBuf <-paste(storyBuf,"      &       ")  ##put same date fact on new line
						 }
						 storyBuf <- paste(storyBuf, latexspechars(appendStory(cfact,uhist,lflag)),"\\\\ \n",sep=" ")
						 dtfcnt <- dtfcnt + 1
					 } ##loop over storyfacts
					 storyBuf <- paste(storyBuf, "\\hline \n")
					 storyBuf <- paste(storyBuf, "\\end{tabularx} \n")
					 storyBuf<- paste(storyBuf,'\\end{table}\n')
					 
					 cat(storyBuf)
					 return(invisible())
				 })
		 
		 
### technical printing
#' @export
#' @docType methods
setGeneric(name='printtStory'
               ,function(object,uhist=F){standardGeneric('printtStory')})
#' @aliases printtStory,Wstory,ANY-method
setMethod(f='printtStory'
         ,signature='Wstory'
         ,def=function(object,uhist=F){

              facts2str <- function(vfacts,uhist){
                sbuf <- NULL
                if (length(vfacts) > 0 ){
                 for( j in 1:length(vfacts) ){
                   cfact <- vfacts[[j]]
                   sbuf <- paste(sbuf, getOwndtimeStr(cfact) )
                   sbuf <- paste(sbuf, attributes(cfact@ownDatetime)$tzone)
                   sbuf <- paste(sbuf, appendStory(cfact,lflag=F,uhist=uhist),sep=" ")
                 }
               }
               return(sbuf)
              }
              cat(paste("Tech print of story for market instrument:", object@instr, "."
                       ,"Story has", length(object@facts), "artifacts defined, of which\n"))
              ### wyckoff facts
              cfs <- getWfacts(object,'w1')
              pref <- "wyckoff facts weekly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              cfs <- getWfacts(object,'d1')
              pref <- "wyckoff facts daily:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              cfs <- getWfacts(object,'h1')
              pref <- "wyckoff facts hourly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              ### note facts
              cfs <- c(getNotefacts(object,'w1')
					  ,getBpropfacts(object,'w1')
			          ,getBviewfacts(object,'w1')
                      ,getHypfacts(object,'w1'))
              pref <- "note facts weekly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
			  
			  cfs <- c(getNotefacts(object,'d1')
					  ,getBpropfacts(object,'d1')
					  ,getBviewfacts(object,'d1')
					  ,getHypfacts(object,'d1'))
              pref <- "char facts daily:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
			  
			  cfs <- c(getNotefacts(object,'h1')
					  ,getBpropfacts(object,'h1')
					  ,getBviewfacts(object,'h1')
					  ,getHypfacts(object,'h1'))
			  pref <- "note facts hourly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              ### trend facts
              cfs <- getTrlfacts(object,'w1')
              pref <- "trend facts weekly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              cfs <- getTrlfacts(object,'d1')
              pref <- "trend facts daily:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              cfs <- getTrlfacts(object,'h1')
              pref <- "trend facts hourly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              ### level facts
              cfs <- getLvlfacts(object,'w1')
              pref <- "level facts weekly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              cfs <- getLvlfacts(object,'d1')
              pref <- "level facts daily:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              cfs <- getLvlfacts(object,'h1')
              pref <- "level facts hourly:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }
              #zone type
              cfs <- getAllIntradayTypefacts(object,'ZoneLine')
              pref <- "zone facts intraday:"
              if ( length(cfs) > 0 )
                   { cat(paste(pref, length(cfs)," ",facts2str(cfs,uhist), ". \n")) }
              else { cat(paste(pref, length(cfs),".\n")) }

         }
         )
###helper methods
setMethod ('show'
           ,'Wstory'
           , function(object){
              cat(paste("Story for market instrument:", object@instr
                       ,"has", length(object@facts), "artifacts defined, of which\n"))
              if (length(object@facts) > 0 ){
               cat(paste("wyckoff facts:", length(getWfacts(object,'w1')), "weekly"
                        ,length(getWfacts(object,'d1')), "daily"
                        ,length(getWfacts(object,'h1')), "hourly.\n"))
               cat(paste("note facts:", length(getNotefacts(object,'w1')), "weekly"
                        ,length(getNotefacts(object,'d1')), "daily"
                        ,length(getNotefacts(object,'h1')), "hourly.\n"))
               cat(paste("trend facts:", length(getTrlfacts(object,'w1')), "weekly"
                        ,length(getTrlfacts(object,'d1')), "daily"
                        ,length(getTrlfacts(object,'h1')), "hourly.\n"))
               cat(paste("level facts:", length(getLvlfacts(object,'w1')), "weekly"
                        ,length(getLvlfacts(object,'d1')), "daily"
                        ,length(getLvlfacts(object,'h1')), "hourly.\n"))
               cat(paste("zone facts:", length(getZnfacts(object,'w1')), "weekly"
                        ,length(getZnfacts(object,'d1')), "daily"
                        ,length(getZnfacts(object,'h1')), "hourly.\n"))
               fmt <- "%Y-%m-%d"
               artfdates <- unlist(lapply(object@facts
                                 ,function(x){ strftime(x@ownDatetime,format=fmt) }))
               #browser()
               if (!is.null(artfdates)){
                 cat(paste("artifact earliest date", min(as.POSIXct(artfdates))
                          ,"latest date",    max(as.POSIXct(artfdates)), "\n"  ))
               }
             }
               cat("*** Class Wstory End.*** \n")
            })
	
#' @export
#' @docType methods
setGeneric( name='is.Wstory',function(object){standardGeneric("is.Wstory")})
setMethod('is.Wstory'
          ,'Wstory'
          ,function(object){
            return ( is(object, 'Wstory') )
          }
          )




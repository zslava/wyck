

#' fetch contracts from loaded contractseqlist object
#' 
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='fetchDataContractsDTN'
					,function(object,contractseqlist,skipintraday=FALSE,rollon="volume")
					{standardGeneric('fetchDataContractsDTN')})
#' @aliases fetchDataContractsDTN,Mldata,ANY-method
setMethod('fetchDataContractsDTN','Mldata'			
					,function(object, contractseqlist,skipintraday=FALSE,rollon="volume"){

			##input valition
			if(!is(contractseqlist,'ContractSeqList')){
				   stop(paste("[MldataDTN:fetchDataContractsDTN validation],"
                    			, contractseqlist, "param is not of type "
                    			, "ContractSeqList"))
			}     

            csl <- contractseqlist

			instrs <- as.character(object@imetadata$IDNAM)			
			idata <- object@idata

			datinstrs <- unlist ( lapply( idata, function(x){x$name} ))
			lstqte_dt <- as.POSIXct("3001-01-01")
			cslInstruments <- unlist(lapply(csl@csl, function(x){x@instName}))
			### main loop
			for (j  in 1: length(instrs)){
				cname <- instrs[j]
				dblvol <- FALSE
               
                insidx <- match(cname,cslInstruments)
                if(is.na(insidx)) { next } # no Contract sequence found in list for 
                                            # current instrument name from Mldata
                cs <- csl@csl[[insidx]]
				if(!skipintraday){
					## get roll params from cs individual configuration
					dblvol <- cs@intrdconf$daydoublevolume
					#convert high freq to low frequencies from cs@intrdconf$intradayseq
					###allPrdIntra(cs)
				}
                ## match position of current instrument in Mldata instruments
				m <- match(cname, datinstrs)				
				cidx <- match(cname, object@imetadata$IDNAM)
				ctkr <- as.character(object@imetadata$TKR[cidx])
				ctyp <- as.character(object@imetadata$TYP[cidx])
				
				
				cinstd <- mkbareIdat(cname)
				cinstd <- setIdatDtype(cinstd,ctyp)
				## daily data
				cdd1 <- getGluedserie(cs,freq=86400,rollon=rollon
					                    ,dblvol=dblvol,idxchk=TRUE)
				cinstd <- setIdatD1(cinstd,cdd1)
				if (nrow(cdd1) > 0 ) {
					cdw1 <- to.weekly(cdd1, indexAt='startof')
					index(cdw1) <- as.POSIXct( index( cdw1) )
					colnames(cdw1) <- colnames(cdd1)
					cinstd <- setIdatW1(cinstd,cdw1)
				}
				##set rollon & glued serie & dblvol in metadata
				if(ctyp =="fut"){
					cinstd <- setIdatMetaRollon(cinstd,rollon) 
					glseq <- getGluedsequence(cs,rollon=rollon)
					cinstd <- setIdatMetaGluedseq(cinstd,glseq)
					cinstd <- setIdatMetaVolccount(cinstd,dblvol)
					# !! set list of intraday data frequencies
				}
                 
				# a place to prepare and add a list of intraday glued series 
				# to current instrument Mldata@idata (perf problem)
				if(length(cs@intrdata)>0 ){
					cinstd$intraday <- ncrIntraDayList(cs)
 					## !! store just 1 ref intraday frequency xts
				}

				lstqte_dt <- min(lstqte_dt, getLastQuoteDatePosix(cs)) 
				
				##update or append new list object to the end of its container list
				if(is.na(m)){
					idata[[  length(idata)+1 ]]  <- cinstd
				}else{
					idata[[ m ]] <- cinstd
				}
			}#loop over instrument list
			
			return ( list(data=idata,lastqte_date=lstqte_dt ))						
})



#' load contracts using contractList config file
#' 
#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='loadDataContractsDTN'
					,function(object
						     ,rollon="volume"
						     ,skipintraday=FALSE,intrd="now",freq=60
						     ,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg")
					{standardGeneric('loadDataContractsDTN')})
#' @aliases loadDataContractsDTN,Mldata,ANY-method
setMethod('loadDataContractsDTN','Mldata'			
					,function(object
						     ,rollon="volume"
						     ,skipintraday=FALSE, intrd="now",freq=60
						     ,cslistconf="~/googledrive/mkdata/dtn/tcfgseqlst/firstseql.cfg"){
            cslCfg <- parseCSListCfg(cslistconf)
            topstoredir <-cslCfg$datadir

			#browser()

            if ( !skipintraday ) {
            	csl <- nCSLfromcfg(cfgfn=cslistconf,freq=freq,intrd=intrd)
            }else{
            	csl <- readCSL(topstoredir=topstoredir)
            }
            return ( fetchDataContractsDTN(object,contractseqlist=csl
            	      				      ,skipintraday=skipintraday
            	      				      ,rollon=rollon)) 

})


## deprecated
#' internal function to add glued intraday data series int Mldata object
#'  from a list of frequencies in ContractSeq object 
#' @keywords internal
crIntraDayList<-function(contrseq, dtstart, dtend,rollon){
	idl <- list()
	freql <- as.list(as.numeric(contrseq@intrdconf$intradayfreq))
	##internal function
	mklel <-function(x,seq,ds,de,rlon){
		glx <- getGluedserie(seq,stime=ds,etime=de,freq=x
			                ,rollon=rlon,dblvol=FALSE)
		return( list(freq=as.character(x),data=glx)  )
	}
	idl <- lapply(freql,FUN=mklel, seq=contrseq,ds=dtstart, de=dtend,rlon=rollon )
	return(idl)
}

#' @keywords internal
ncrIntraDayList<-function(contrseq){
	idl <- list()
	freql <- as.list(as.numeric(contrseq@intrdconf$intradayfreq))

	##internal function
	mklel <-function(x,seq){
		glx <- getRefContserie(seq,freq=x)
		return( list(freq=as.character(x),data=glx)  )
	}
	idl <- lapply(freql,FUN=mklel, seq=contrseq)
	return(idl)
}


#  deprecated ? 

#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='updDataContractsDTN'
		,function(object,rollon="volume",updMeta=T,topstoredir="~/googledrive/mkdata/dtn/contrseq")
		{standardGeneric('updDataContractsDTN')})
#' @aliases updDataContractsDTN,Mldata,ANY-method
setMethod('updDataContractsDTN','Mldata'
		,function(object,rollon="volume",updMeta=T,topstoredir="~/googledrive/mkdata/dtn/contrseq"){
			
			instrs <- as.character(object@imetadata$IDNAM)
			csl <- readCSL(instrs,topstoredir=topstoredir) ##contractseq list
			for(j in 1:length(instrs)){
				       cname <- instrs[j]
				       cs <-readseq(cname,topstoredir)
					   updateEOD(cs,ctime=Sys.time(), updMeta=updMeta,persist=T)  ### updating invividual contract seq 
			}
			idata <- loadDataContractsDTN(object,rollon=rollon,topstoredir=topstoredir)
			return ( idata )						
		})


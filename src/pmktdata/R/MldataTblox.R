#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='iniDataDldTblox',function(.Object){standardGeneric('iniDataDldTblox')})
#' @aliases iniDataDldTblox,Mldata,ANY-method
setMethod('iniDataDldTblox','Mldata'
		,function(.Object){
			tickers <- as.character(.Object@imetadata$TKR)
			idata <- .Object@idata
			tbloxdata <- tbloxListHistDaily(tickers,unadjust=T,verbose=T)  ### temp test  local
			for (j  in 1: length(tbloxdata)){
				#cinstd <- list(name= tbloxdata[[j]]$name )
				cname <- tbloxdata[[j]]$name
				cidx <- match(cname, .Object@imetadata$IDNAM)
				ctyp <- as.character(.Object@imetadata$TYP[cidx])
				
				cinstd <- mkbareIdat(cname)
				cinstd <- setIdatDtype(cinstd,ctyp)
				cdd1 <- tbloxdata[[j]]$data
				#cinstd$d1 <- cdd1
				cinstd <- setIdatD1(cinstd,cdd1)
				if ( nrow(cdd1) > 0 ) {
					cdw1 <- to.weekly(cdd1,indexAt='startof')
					index(cdw1) <- as.POSIXct( index( cdw1) )
					colnames(cdw1) <- colnames(cdd1)
					#cinstd$w1 <- cdw1
					cinstd <- setIdatW1(cinstd,cdw1)
				}
				##append new list object to the end of its container list
				idata[[  length(idata)+1 ]]  <- cinstd
			} #loop over instrument list			
			
			return ( idata )			
		})

#' @keywords internal
#' @export
#' @docType methods
setGeneric(name='updDataDldTblox',function(.Object){standardGeneric('updDataDldTblox')})
#' @aliases updDataDldTblox,Mldata,ANY-method
setMethod('updDataDldTblox','Mldata'
		,function(.Object){
			
			nenddate <- .Object@cdate			
			tickers <- as.character(.Object@imetadata$TKR)
			datinstrs <- unlist ( lapply( .Object@idata, function(x){x$name} ))
			idata <- .Object@idata
			
			tbloxdata <- tbloxListHistDaily(tickers,unadjust=T,verbose=T)
			for (j  in 1: length(tickers)){
				cname <- tickers[j]
				m <- match(cname, datinstrs)
				cinstd <- idata[[m]]  ## get icur instrument data
				
				#cinstd$d1 <- tbloxdata[[j]]$data
				
				print(paste('update download tblox market data to date', nenddate
								,'for instrument',cinstd$name, '...'))
				od1 <- getIdatD1(cinstd)   ## old d1 data
				nd1 <- tbloxdata[[j]]$data ## new d1 data
				lstohlc <- last(od1)
				lstdate <- index(lstohlc)
				nlstdate <- index(last(nd1))
				if ( nenddate > lstdate && nlstdate > lstdate) {
					dat2apnd <- nd1[-(1:nrow(od1))]
					if (nrow(dat2apnd) > 0 ) {
						d1appended <- rbind(od1, dat2apnd)
						cinstd <- setIdatD1(cinstd, d1appended)
					}
					uw1 <- to.weekly( getIdatD1(cinstd),indexAt='startof') # upd w1
					index(uw1) <- as.POSIXct(index(uw1))
					colnames(uw1) <- colnames( getIdatD1(cinstd))
					cinstd <- setIdatW1(cinstd,uw1)
				}
				idata[[m]]  <- cinstd  ## store updated cinstd into its holding container
			}
			return ( idata )
		})
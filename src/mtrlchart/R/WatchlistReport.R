
 
#' @export
#' @docType methods
setGeneric(name='genReport'
          ,function(.Object,rnwfile='simpleReport.Rnw', flipl=T,rnwpath='~/R/library/mtrlchart/data', topdir='~/googledrive/chart', latexcompile='/usr/texbin/pdflatex')
		{standardGeneric('genReport')})
#' @aliases genReport,Wwatchlist,ANY-method
setMethod('genReport','Wwatchlist'
		,function(.Object,rnwfile='simpleReport.Rnw', flipl=T,rnwpath='~/R/library/mtrlchart/data', topdir='~/googledrive/chart', latexcompile='/usr/texbin/pdflatex'){
			
   nameObject <- deparse(substitute(.Object))
   ##flip wyck labels to chars
    
   if(flipl) { flipFctlbl(.Object) }
	d1fmt<-"%Y-%m-%d"		
	cd <- getwd()
	iname<-getCinstrument(.Object@mdata)
	
	
	##daily data to determine last current date
	dd <- getCdatedata(.Object@mdata, iname,'d1')
	lstdate <- strftime( index(last(dd)), format=d1fmt )
	
	nY <- as.numeric(strftime(as.POSIXct(lstdate),format="%Y"))
	nmo <- as.numeric(strftime(as.POSIXct(lstdate),format="%m"))
	nw <- as.numeric(strftime(as.POSIXct(lstdate),format="%W"))
	dow <-           strftime(as.POSIXct(lstdate),format="%a")
	
	##copy files to tmp dir
	tempwdir<-paste(topdir,'cwork',sep='/')
	if( !file.exists(tempwdir)) { dir.create(tempwdir)}
	##change to temp dir
	setwd(tempwdir)
	##clearn old files
	ff<-list.files(tempwdir)
	for(j in (1:length(ff))){
		file.remove(ff[j])
	}
	##copy rnw file to tempdir
	rnwfn<-paste(rnwpath,rnwfile,sep="/")
	if( !file.exists(rnwfn)) {
		stop(paste(rnwfile, rnwfn,'not found. Check paths.'))
	}
	file.copy(from=rnwfn, to=tempwdir, overwrite=T)
	
	fn<-strsplit(basename(rnwfile),'.', fixed=T)[[1]][1]
	##produce charts to pdf files for later graphics in tempdir
	cc(.Object,tf="d1",pl=T,scf=0.7,npf=2,dev=T,devdir=tempwdir)
	cc(.Object,tf="w1",pl=T,scf=0.7,npf=2,dev=T,devdir=tempwdir)
	
	knit(paste(fn,".Rnw",sep=""), output=paste(fn,'.tex',sep="") )
	#browser()
	
	system(paste(latexcompile, fn))
	pdfname <- paste(fn,'.pdf',sep="")
	#system(paste("open ", fn, ".pdf",sep="") )
	if(!file.exists(pdfname)){
	   stop(paste('problems with report compilation', iname, lstdate))
	}
	
	##copy report to a final destination
	
	reportbasename<-paste(lstdate,'-',dow,'_',iname,'.pdf',sep='')
	file.rename(from=pdfname, to=reportbasename)
	
	ydir<-paste(topdir,nY,sep="/")
	if( !file.exists(ydir)) { dir.create(ydir)}
	wkdir <-paste(ydir, "/",nY,"-",nmo ,"-wk",nw,sep="")
	if( !file.exists(wkdir)) { dir.create(wkdir)}
	file.copy(from=reportbasename,to=wkdir, overwrite=T)
	finalreport <- paste(wkdir,reportbasename,sep="/")
	if(file.exists(finalreport)){
		print(paste('final report at:',finalreport))
	}else{
		print(paste('problems with final report. check generation results in',tempwdir))
	}
			
	setwd(cd)
	
	##flip wyck labels back to numbers
	if(flipl) { flipFctlbl(.Object) }
	
	return(invisible())
})


 #' @export
 #' @docType methods
setGeneric(name='prephelp',function(.Object){standardGeneric('prephelp')})
#' @aliases prephelp,Wwatchlist,ANY-method
setMethod('prephelp','Wwatchlist'
          ,function(.Object){
        			cat("genReport(o,rnwfile='simpleReport.rwn',rnwpath,topdir,latexcompile)\n")
        			cat(" ::\t\t  generate  d1+w1 report in pdf stored under topdir \n")

          	})




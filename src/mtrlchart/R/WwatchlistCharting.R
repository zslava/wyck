#####################################################
###   Wwatchlist charting operatins methods####
#####################################################

##graph params operational functions
##flip wyk label id from numeric to slbl
 #' @export
 #' @docType methods
setGeneric(name='flipFctlbl',function(.Object,sav=FALSE){standardGeneric('flipFctlbl')})
 #' flip labels on the chart from numbers to short label abbreviation
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases flipFctlb,Wwatchlist,ANY-method
setMethod('flipFctlbl','Wwatchlist'
					,function(.Object,sav=FALSE){
						nameObject <- deparse(substitute(.Object))
						gc <- .Object@wgconf
						flipWykshape(gc)
						setGraphconf(.Object) <- gc
						 if(sav){ persist(.Object)}
						assign(nameObject,.Object,envir=parent.frame())
						return(invisible())
					})
	#' @export
	#' @docType methods
	setGeneric(name='flipbg',function(.Object,sav=FALSE){standardGeneric('flipbg')})
	#' flip chart background color
	#'
	#' @param Wwatchlist instance
	#' @return void. Wwatchlist instance is modified
	#' @aliases flipbg,Wwatchlist,ANY-method
	setMethod('flipbg','Wwatchlist'
			,function(.Object,sav=FALSE){
				nameObject <- deparse(substitute(.Object))
				gc <- getGraphconf(.Object)
				flipChartbg(gc)
				setGraphconf(.Object) <- gc
				if(sav){ persist(.Object) }
				assign(nameObject,.Object,envir=parent.frame())
				return(invisible())
			})

##increase gpars
 #' @export
 #' @docType methods
setGeneric(name='upgpar',function(.Object,sav=FALSE){standardGeneric('upgpar')})
 #' increase size of lablels on the chart
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases upgpar,Wwatchlist,ANY-method
setMethod('upgpar','Wwatchlist'
					,function(.Object,sav=FALSE){
						nameObject <- deparse(substitute(.Object))
						gc <- .Object@wgconf
						parsUpsize(gc)
						setGraphconf(.Object) <- gc
						if(sav){ persist(.Object) }
						assign(nameObject,.Object,envir=parent.frame())
						return(invisible())
					})
 #' @export
 #' @docType methods
setGeneric(name='dngpar',function(.Object,sav=FALSE){standardGeneric('dngpar')})
 #' decrease size of lablels on the chart
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases upgpar,Wwatchlist,ANY-method
setMethod('dngpar','Wwatchlist'
					,function(.Object,sav=FALSE){
						nameObject <- deparse(substitute(.Object))
						gc <- .Object@wgconf
						parsDnsize(gc)
						setGraphconf(.Object) <- gc
						if(sav){ persist(.Object) }
						assign(nameObject,.Object,envir=parent.frame())
						return(invisible())
					})
 #' @export
 #' @docType methods
setGeneric(name='inigpar',function(.Object,sav=FALSE){standardGeneric('inigpar')})
 #' (re)initialize graphic parameters for labels on a chart
 #'
 #' @param Wwatchlist instance
 #' @return void. Wwatchlist instance is modified
 #' @aliases upgpar,Wwatchlist,ANY-method
setMethod('inigpar','Wwatchlist'
					,function(.Object,sav=FALSE){
						nameObject <- deparse(substitute(.Object))
						gc <- .Object@wgconf
						resetDefaults(gc)
						setGraphconf(.Object) <- gc
						if(sav) { persist(.Object) }
						assign(nameObject,.Object,envir=parent.frame())
						return(invisible())
					})


 #' @export
 #' @docType methods
setGeneric(name='cc',function(.Object,tf,nof=F,pl=F,plf=F,csl=T,vcolf=F,candles=F,nbp=1,scf=1,dev=F,npf=1,devdir='/tmp',nlayout=F){standardGeneric('cc')})
 #'  produce a chart with labels
 #'
 #' sets time frame frame according t parameter tf
 #' @param Wwatchlist instance
 #' @param tf character  timeframe with value \code{'w1'|'d1'}. If not specified a current watchlist timeframe is used
 #' @param nof Boolean   If true no labels are presented on a chart. Default value false.
 #' @param pl Boolean  If true chart draws vertical lines on aggregrated periods. Default value false.
 #' @param plf Boolean  If true chart draws vertical lines on Wyckoff facts. Default value false.
 #' @param csl Boolean  If true chart vertical lines on roll for expiring instruments(futurs. Default value TRUE
 #' @param candles Boolean  If true chart candlesticks instead of bars
 #' @param vcolf Boolean  If true chart volume on wyckoff facts bars with specific color. Default value false
 #' @param nbp numeric   with value =  n > 1 charts n data windows ago. Default value 1
 #' @param scf numeric   Scale factor to increase the charted past window. Default value 1
 #' @param npf numeric   a position in stack for a parent environment. (tech param) Default value 1
 #' @return void. Wwatchlist instance is modified
 #' @aliases cc,Wwatchlist,ANY-method
setMethod('cc','Wwatchlist'
					,function(.Object, tf, nof=F,pl=F,plf=F,csl=T,vcolf=F,candles=F,nbp=1,scf=1,dev=F,npf=1,devdir='/tmp',nlayout=F){

			 if(missing(tf)){
					 tf <- getCurtimeframe(.Object)
			 }
			 if ((  tf != 'w1'
										&& tf != 'd1'
										&& tf != 'h1'
										 )){
										stop(paste("[Wstory:ichart validation]"
															," bad value of tf param"))
				}

			ctf <- tf
			nameObject <- deparse(substitute(.Object))
			##reset cur time frame
			setCurtimeframe(.Object) <- ctf
			mldl <- .Object@mdata
			cdate <- getCdate(mldl)
			cdt_tz <- base::format(cdate, format="%Z")
			iname<-getCinstrument(mldl)
			tfn <- NA
			tfn[ ctf %in% 'w1' ] <- "weekly"
			tfn[ ctf %in% 'd1' ] <- 'daily'
			chartName <- paste(iname," ","(",tfn,")",sep="")
						##chartName case for contract sequence
			fc<-getIdatMetaFrontcontract(mldl,iname,ctime=cdate,format="ch")
			if( length(fc)>0 ){
				chartName <- paste(iname,".",fc$unrolled," (",tfn,")",sep="")			  
			}
			
			##compute subset string
			csubs <- chartDatesRange2String(edate=cdate, dsubset=getSubset(.Object,ctf), nbp=nbp,scf=scf,tz=cdt_tz)
			
			wg <- getGraphconf(.Object)
			
			 cstory <- getCurstory(.Object)
			
			wdict <- getWdict(.Object)
			wkside <- getWykside(wdict)
			assign('wkside',wkside,envir=parent.frame(n=npf))
			assign('pl',pl,envir=parent.frame(n=npf))
			assign('plf',plf,envir=parent.frame(n=npf))
			assign('vcolf',vcolf,envir=parent.frame(n=npf))
			
			wtrls<- getTrlfacts(cstory, 'w1')
			trls <- getTrlfacts(cstory, 'd1')
			wlvls<- getLvlfacts(cstory, 'w1')
			lvls <- getLvlfacts(cstory, 'd1')
             			

			wfs  <- getWfacts(cstory,  'd1')
			wwfs <- getWfacts(cstory, 'w1')

			#browser()
			
			assign('wtrls',wtrls,envir=parent.frame(n=npf))
			assign('trls',trls,envir=parent.frame(n=npf))
			assign('wwfs',wwfs,envir=parent.frame(n=npf))
			assign('wfs',wfs,envir=parent.frame(n=npf))
			assign('wlvls',wlvls,envir=parent.frame(n=npf))
			assign('lvls',lvls,envir=parent.frame(n=npf))



			##daily data
			dd <- getCdatedata(mldl, getCinstrument(mldl),'d1')

			##weekly data
			wd <- getCdatedata(mldl, getCinstrument(mldl),'w1')

			assign('wd',wd,envir=parent.frame(n=npf))

						## graph params
			wp <- getWykgparams(wg, 'd1')
			wpw1 <- getWykgparams(wg, 'w1')
			
			 ##level graph params
			lp <- getLvlgparams(wg, 'd1')
			lpw1 <- getLvlgparams(wg, 'w1')

			tp <- getTrlgparams(wg, 'd1')
			tpw1 <- getTrlgparams(wg, 'w1')

			assign('wp',wp,envir=parent.frame(n=npf))
			assign('wpw1',wpw1,envir=parent.frame(n=npf))
			assign('lp',lp,envir=parent.frame(n=npf))
			assign('lpw1',lpw1,envir=parent.frame(n=npf))
			assign('tp',tp,envir=parent.frame(n=npf))
			assign('tpw1',tpw1,envir=parent.frame(n=npf))



			theme.pars <- getThemepars(wg)
			bartyp <- theme.pars$bartype
			gtheme <-  theme.pars$themecolor
			upcol <-   theme.pars$up.col
			dncol <-   theme.pars$dn.col

			if(candles){ btype <- "candlesticks" }
			else       { btype <- "bars" }
						

			## ep in case of for contract sequences instrument
			if(csl){
				d1rolls <- getIdatMetaRolls(mldl,iname)
				}else{
				 d1rolls <- list()
			}
						w1rolls <- list() #rolls only defined on daily data
			
			assign('d1rolls',d1rolls,envir=parent.frame(n=npf))
			assign('w1rolls',w1rolls,envir=parent.frame(n=npf))
			
			
			##optionally print chart to a file
			if(dev){
				wwidth <- 12
				wheight <- 10				
				#if(.Platform$OS.type == 'unix') { devdir <- '/tmp'}
				#                            else{  devdir <- 'c:/Windows/temp'}
				gf<-paste(devdir,'/',iname,'_',ctf,'.pdf',sep='')							
				pdf(file=gf,width=wwidth,heigh=wheight)
				print(paste("printed chart to file:",gf))
			}			
						
	        #test
	        if(!nlayout) {
				layout(matrix(1:2, nrow=2, ncol=1 ))
            }
			if (ctf == 'd1'){
				if(!nof && !pl) {
						
					chartSeries( dd
								,name=chartName,
								,layout=NULL
								,TA='addTrendfact(tfs=trls,tcol=tp$color);
										addscTrendfact(wtrls,wd,src="w1",trg="d1",tcol=tpw1$color);
										 addLevelfact(lvls,lcex=lp$cex,loff=lp$offs,lcol=lp$lvcolor,hcol=lp$hfcolor,iccol=lp$iccolor, ltyp="solid");
										 addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor,ltyp="longdash");
										 addWfact(wfs,lbltype=wp$shape,lcex=wp$cex,loff=wp$offs,lupcol=wp$upcolor,ldncol=wp$dncolor,cepw=plf,mside=wkside,rolls=d1rolls);
										 addwVo(wfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=d1rolls);'
								,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
								,subset=csubs )

							 }
				 if(!nof && pl) {
							 chartSeries( dd
								,name=chartName
								,layout=NULL
								,TA='addTrendfact(trls,tcol=tp$color);
										 addscTrendfact(wtrls,wd,src="w1",trg="d1",tcol=tpw1$color);
										 addLevelfact(lvls,lcex=lp$cex,loff=lp$offs,lcol=lp$lvcolor,hcol=lp$hfcolor,iccol=lp$iccolor,ltyp="solid");
										 addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor,ltyp="longdash");
										 addWfact(wfs,lbltype=wp$shape,lcex=wp$cex,loff=wp$offs,lupcol=wp$upcolor,ldncol=wp$dncolor,cepw=plf,mside=wkside,rolls=d1rolls);
										 addEp(cep=pl);
									 addwVo(wfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=d1rolls);'
								,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
								,subset=csubs )
							}
				if(nof && !pl){
								chartSeries( dd
									,name=chartName
									,layout=NULL
									,TA='addfVoep(cep=FALSE);'
									,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
									,subset=csubs )
					 }
				if(nof && pl){
								chartSeries( dd
									,name=chartName
									,layout=NULL
									,TA='addfVoep(cep=TRUE);addEp(cep=TRUE)'
									,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
									,subset=csubs )
							}
					 }
					 if (ctf == 'w1'){
							if(!nof && !pl){
								chartSeries(wd
										 ,name=chartName
										 ,layout=NULL
										 ,TA='addTrendfact(wtrls,tcol=tpw1$color);
													addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor);
													addWfact(wwfs,lbltype=wpw1$shape,lcex=wpw1$cex,loff=wpw1$offs,lupcol=wpw1$upcolor,ldncol=wpw1$dncolor,cepw=plf,mside=wkside,rolls=w1rolls);
											addwVo(wwfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=w1rolls);'				  
					,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol
										 ,theme=gtheme
										 ,subset=csubs )
							}
				if(!nof && pl){
								chartSeries(wd
										 ,name=chartName
										 ,layout=NULL
										 ,TA='addTrendfact(wtrls,tcol=tpw1$color);
													addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor);
													addWfact(wwfs,lbltype=wpw1$shape,lcex=wpw1$cex,loff=wpw1$offs,lupcol=wpw1$upcolor,ldncol=wpw1$dncolor,cepw=plf,mside=wkside,rolls=w1rolls);
													addEp(cep=pl);
												addwVo(wwfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=w1rolls);'                          
										,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol
										 ,theme=gtheme
										 ,subset=csubs )
							}
							if(nof && !pl){
								chartSeries(wd
										 ,name=chartName
										 ,layout=NULL
										 ,TA='addfVoep(cep=FALSE);'
					 					,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol
										 ,theme=gtheme
										 ,subset=csubs )
							}
				if(nof && pl){
								chartSeries(wd
										 ,name=chartName
										 ,layout=NULL
										 ,TA='addfVoep(cep=TRUE);addEp(cep=TRUE)'
					 ,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol
										 ,theme=gtheme
										 ,subset=csubs )
							}
					 }
						 ## remove objects from parent.frame
						rm(wd,envir=parent.frame(n=npf))

						rm(wtrls,envir=parent.frame(n=npf))
						rm(wwfs,envir=parent.frame(n=npf))
						rm(wlvls,envir=parent.frame(n=npf))
						rm(trls,envir=parent.frame(n=npf))
						rm(wfs,envir=parent.frame(n=npf))
						rm(lvls,envir=parent.frame(n=npf))

						rm('wp',envir=parent.frame(n=npf))
						rm('wpw1',envir=parent.frame(n=npf))
						rm('lp',envir=parent.frame(n=npf))
						rm('lpw1',envir=parent.frame(n=npf))
						rm('tp',envir=parent.frame(n=npf))
						rm('tpw1',envir=parent.frame(n=npf))
						
			rm('wkside',envir=parent.frame(n=npf))
			rm('pl',envir=parent.frame(n=npf))
			rm('plf',envir=parent.frame(n=npf))
			rm('vcolf',envir=parent.frame(n=npf))
			
			rm('d1rolls',envir=parent.frame(n=npf))
			rm('w1rolls',envir=parent.frame(n=npf))
			
			if(dev){
				dev.off()
			}

						assign(nameObject,.Object,envir=parent.frame())
						return( invisible() )
					})


 #' @export
 #' @docType methods
setGeneric(name='ccd',function(.Object,nof=F,pl=F,plf=F,csl=T,vcolf=F,scf=1,nbp=1,npf=2,dev=F,sig=F){standardGeneric('ccd')})
 #'  a wrapper to produce a chart with labels on daily data
 #'
 #' sets  data current timeframe as daily
 #' @param Wwatchlist instance
 #' @param ep Boolean  If true chart draws vertical lines on periods. Default value false.
 #' @param nof Boolean   If true no labels are presented on a chart. Default value false.
 #' @param nbp numeric   with value =  n > 1 charts n data windows ago. Default value 1
 #' @param scf numeric   Scale factor to increase the charted past window. Default value 1
 #' #' @return void. Wwatchlist instance is modified
 #' @aliases ccd,Wwatchlist,ANY-method
setMethod('ccd','Wwatchlist'
					,function(.Object,nof=F,pl=F,plf=F,csl=T,vcolf=F,scf=1,nbp=1,npf=2,dev=F,sig=F){

						nameObject <- deparse(substitute(.Object))
						cc(.Object,tf="d1",nof=nof,pl=pl,csl=csl,plf=plf,vcolf=vcolf,nbp=nbp,scf=scf,npf=npf,dev=dev)
						##reprint story
						ps(.Object,tf="d1",sig=sig)
						assign(nameObject,.Object,envir=parent.frame())
						return( invisible() )
					})

 #' @export
 #' @docType methods
setGeneric(name='ccw',function(.Object,nof=F,pl=T,plf=F,vcolf=F,scf=1,nbp=1,npf=2,dev=F){standardGeneric('ccw')})
 #' a wrapper to produce a chart with labels on weekly data
 #'
 #' @param Wwatchlist instance
 #' @param ep Boolean  If true chart draws vertical lines on periods. Default value false.
 #' @param nof Boolean   If true no labels are presented on a chart. Default value false.
 #' @param nbp numeric   with value =  n > 1 charts n data windows ago. Default value 1
 #' @param scf numeric   Scale factor to increase the charted past window. Default value 1
 #' @return void. Wwatchlist instance is modified
 #' @aliases ccw,Wwatchlist,ANY-method
setMethod('ccw','Wwatchlist'
					,function(.Object,nof=F,pl=T,plf=F,vcolf=F,scf=1,nbp=1,npf=2,dev=F){

						nameObject <- deparse(substitute(.Object))
						cc(.Object,tf="w1",nof=nof,pl=pl,plf=plf,vcolf=vcolf,scf=scf,nbp=nbp,npf=npf,dev=dev)
						##reprint story
						ps(.Object,tf="w1")
						assign(nameObject,.Object,envir=parent.frame())

						return( invisible() )
					})


 #' @export
 #' @docType methods
setGeneric(name='ccc',function(.Object,nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1,npf=1,dev=F){standardGeneric('ccc')})
 #'  produce a combined daily and weely chart with labels
 #'
 #' sets  data current timeframe as weekly
 #' @param Wwatchlist instance
 #' @param pl Boolean  If true chart draws vertical lines on periods. Default value false.
 #' @param nof Boolean   If true no labels are presented on a chart. Default value false.
 #' @param nbp numeric   with value =  n > 1 charts n data windows ago. Default value 1
 #' @param scf numeric   Scale factor to increase the charted past window. Default value 1
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases ccc,Wwatchlist,ANY-method
setMethod('ccc','Wwatchlist'
					,function(.Object,nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1,npf=1,dev=F){

	
		ctf <- getCurtimeframe(.Object)
		nameObject <- deparse(substitute(.Object))

		mldl <- .Object@mdata
		iname<-getCinstrument(mldl)

		 ##compute subset string
		fmt="%Y-%m-%d"
		##csubs for d1 tf
		subswidth <- getSubset(.Object,"d1")
		cdate <- getCdate(mldl)
		cdt_tz <- base::format(cdate, format="%Z")

		subrdate <- cdate - (nbp-1)* scf * subswidth * 86400
		subldate <- cdate  -  nbp * scf * subswidth * 86400
		csubs_d1 <- paste( strftime(subldate, format=fmt),'::', strftime(subrdate, format=fmt),sep="")

		csubs_d1 <- chartDatesRange2String(edate=cdate, dsubset=getSubset(.Object,tf='d1'), nbp=nbp,scf=scf,tz=cdt_tz)

		##csubs for w1 tf
		subswidth <- getSubset(.Object,"w1")
		cdate <- getCdate(mldl)
		subrdate <- cdate - (nbp-1)* scf * subswidth * 86400
		subldate <- cdate  -  nbp * scf * subswidth * 86400
		csubs_w1 <- paste( strftime(subldate, format=fmt),'::', strftime(subrdate, format=fmt),sep="")
		csubs_w1 <- chartDatesRange2String(edate=cdate, dsubset=getSubset(.Object,tf='w1'), nbp=nbp,scf=scf,tz=cdt_tz)


		cstory <- getCurstory(.Object)

		wdict <- getWdict(.Object)
		wkside <- getWykside(wdict)
		assign('wkside',wkside,envir=parent.frame(n=npf))
		assign('pl',pl,envir=parent.frame(n=npf))
		assign('plf',plf,envir=parent.frame(n=npf))
		assign('vcolf',vcolf,envir=parent.frame(n=npf))


		wtrls<- getTrlfacts(cstory, 'w1')
		trls <- getTrlfacts(cstory, 'd1')
		wlvls<- getLvlfacts(cstory, 'w1')
		lvls <- getLvlfacts(cstory, 'd1')
		wwfs <- getWfacts(cstory, 'w1')
		wfs  <- getWfacts(cstory,  'd1')


		assign('wtrls',wtrls,envir=parent.frame(n=npf))
		assign('trls',trls,envir=parent.frame(n=npf))
		assign('wwfs',wwfs,envir=parent.frame(n=npf))
		assign('wfs',wfs,envir=parent.frame(n=npf))
		assign('wlvls',wlvls,envir=parent.frame(n=npf))
		assign('lvls',lvls,envir=parent.frame(n=npf))



		##daily data
		dd <- getCdatedata(mldl, getCinstrument(mldl),'d1')
		##weekly data
		wd <- getCdatedata(mldl, getCinstrument(mldl),'w1')
		assign('wd',wd,envir=parent.frame(n=npf))

		## graph params
	 wg <- getGraphconf(.Object)

	 wp <- getWykgparams(wg, 'd1')
	 wpw1 <- getWykgparams(wg, 'w1')

	 lp <- getLvlgparams(wg, 'd1')
	 lpw1 <- getLvlgparams(wg, 'w1')

	 tp <- getTrlgparams(wg, 'd1')
	 tpw1 <- getTrlgparams(wg, 'w1')

	 assign('wp',wp,envir=parent.frame(n=npf))
	 assign('wpw1',wpw1,envir=parent.frame(n=npf))
	 assign('lp',lp,envir=parent.frame(n=npf))
	 assign('lpw1',lpw1,envir=parent.frame(n=npf))
	 assign('tp',tp,envir=parent.frame(n=npf))
	 assign('tpw1',tpw1,envir=parent.frame(n=npf))

		## ep in case of for contract sequences instrument
		d1rolls <- getIdatMetaRolls(mldl,iname) # list() of null is returned
		w1rolls <- list() #rolls only defined on daily data
		
		assign('d1rolls',d1rolls,envir=parent.frame(n=npf))
		assign('w1rolls',w1rolls,envir=parent.frame(n=npf))


		theme.pars <- getThemepars(wg)
		bartyp <- theme.pars$bartype
		gtheme <-  theme.pars$themecolor
		upcol <-   theme.pars$up.col
		dncol <-   theme.pars$dn.col
		
		btype<-"bars"
		
		##optionally print chart to a file
		if(dev){
			wwidth <- 14
			wheight <- 8				
			gf<-tempfile("chart",fileext=".pdf")
			pdf(file=gf,width=wwidth,heigh=wheight)
			print(paste("printed chart to file:",gf))
		}					

	 	if(npf==1) { layout(matrix(1:4, nrow=4, ncol=1  )) }
				
		if(!nof){  
				chartName <- paste(iname," ","(daily)",sep="")
				if(!pl){
							 chartSeries( dd
								,name=chartName
								,type="bars",layout=NULL
								,TA='addTrendfact(trls,tcol=tp$color);
										 addscTrendfact(wtrls,wd,src="w1",trg="d1",tcol=tpw1$color);
										 addLevelfact(lvls,lcex=lp$cex,loff=lp$offs,lcol=lp$lvcolor,hcol=lp$hfcolor,iccol=lp$iccolor,ltyp="solid");
										 addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor,ltyp="longdash");
										 addWfact(wfs,lbltype=wp$shape,lcex=wp$cex,loff=wp$offs,lupcol=wp$upcolor,ldncol=wp$dncolor,cepw=plf,mside=wkside,rolls=d1rolls);
										 addwVo(wfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=d1rolls);'
								,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
								,subset=csubs_d1)

				}else{
							 chartSeries( dd
								,name=chartName
								,type="bars",layout=NULL
								,TA='addTrendfact(trls,tcol=tp$color);
										 addscTrendfact(wtrls,wd,src="w1",trg="d1",tcol=tpw1$color);
										 addLevelfact(lvls,lcex=lp$cex,loff=lp$offs,lcol=lp$lvcolor,hcol=lp$hfcolor,iccol=lp$iccolor);
										 addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor);
										 addWfact(wfs,lbltype=wp$shape,lcex=wp$cex,loff=wp$offs,lupcol=wp$upcolor,ldncol=wp$dncolor,cepw=plf,mside=wkside,rolls=d1rolls);
										 addEp(cep=pl);
									 addwVo(wfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=d1rolls);'                     
								,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
								,subset=csubs_d1)
				}
							##plot w1 chart
				chartName <- paste(iname," ","(weekly)",sep="")
				if(!pl){
								chartSeries(wd
										,name=chartName
										,type="bars",layout=NULL
										,TA='addTrendfact(wtrls,tcol=tpw1$color);
												 addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor);
												 addWfact(wwfs,lbltype=wpw1$shape,lcex=wpw1$cex,loff=wpw1$offs,lupcol=wpw1$upcolor,ldncol=wpw1$dncolor,cepw=plf,mside=wkside,rolls=w1rolls);
												 addwVo(wwfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=w1rolls);'                          
										,bar.type=bartyp, up.col=upcol, dn.col=dncol
										,theme=gtheme
										,subset=csubs_w1)
				}else{
								chartSeries(wd
										 ,name=chartName
										 ,type="bars",layout=NULL
										 ,TA='addTrendfact(wtrls,tcol=tpw1$color);
													addLevelfact(wlvls,lcex=lpw1$cex,loff=lpw1$offs,lcol=lpw1$lvcolor,hcol=lpw1$hfcolor,iccol=lpw1$iccolor);
													addWfact(wwfs,lbltype=wpw1$shape,lcex=wpw1$cex,loff=wpw1$offs,lupcol=wpw1$upcolor,ldncol=wpw1$dncolor,cepw=plf,mside=wkside,rolls=w1rolls);
													addEp(cep=pl);
													addwVo(wwfs,cep=pl,cepw=plf,wcol=vcolf,mside=wkside,rolls=w1rolls);'                                               
										 ,bar.type=bartyp, up.col=upcol, dn.col=dncol
										 ,theme=gtheme
										 ,subset=csubs_w1)
				}
			 }#!nof
			 if(nof){
				chartName <- paste(iname," ","(daily)",sep="")        
				if(!pl){
								chartSeries( dd
									,name=chartName
									,type="bars",layout=NULL
									,TA='addfVoep(cep=FALSE);'
									,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
									,subset=csubs_d1 )          
				}else{
							 chartSeries( dd
									,name=chartName
									,type="bars",layout=NULL
									,TA='addfVoep(cep=TRUE);addEp(cep=TRUE)'
									,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme
									,subset=csubs_d1 )          
				}
				chartName <- paste(iname," ","(weekly)",sep="")
				if(!pl){
							 chartSeries(wd
										 ,name=chartName
										 ,type="bars",layout=NULL
										 ,TA='addfVoep(cep=FALSE);'
										 ,bar.type=bartyp, up.col=upcol, dn.col=dncol
										 ,theme=gtheme
										 ,subset=csubs_w1 )
					}else{
								chartSeries(wd
										 ,name=chartName
										 ,type="bars",layout=NULL
										 ,TA='addfVoep(cep=TRUE);addEp(cep=TRUE)'
										 ,bar.type=bartyp, up.col=upcol, dn.col=dncol
										 ,theme=gtheme
										 ,subset=csubs_w1 )
					}
			 }

			if(npf==1){ layout(1) }
				## remove objects from parent.frame
			rm(wd,envir=parent.frame(n=npf))

			rm(wtrls,envir=parent.frame(n=npf))
			rm(wwfs,envir=parent.frame(n=npf))
			rm(wlvls,envir=parent.frame(n=npf))
			rm(trls,envir=parent.frame(n=npf))
			rm(wfs,envir=parent.frame(n=npf))
			rm(lvls,envir=parent.frame(n=npf))

			rm('wp',envir=parent.frame(n=npf))
			rm('wpw1',envir=parent.frame(n=npf))
			rm('lp',envir=parent.frame(n=npf))
			rm('lpw1',envir=parent.frame(n=npf))
			rm('tp',envir=parent.frame(n=npf))
			rm('tpw1',envir=parent.frame(n=npf))
		
			rm('wkside',envir=parent.frame(n=npf))
			rm('pl',envir=parent.frame(n=npf))
			rm('plf',envir=parent.frame(n=npf))
			rm('vcolf',envir=parent.frame(n=npf))

			rm('d1rolls',envir=parent.frame(n=npf))
			rm('w1rolls',envir=parent.frame(n=npf))
			
						
			if(dev){ dev.off() }
			#swithch time frame to 'd1'
			setCurtimeframe(.Object) <- 'd1'
			assign(nameObject,.Object,envir=parent.frame())
			return( invisible() )
})


#' @export
 #' @docType methods
setGeneric(name='ccm',function(.Object,vinst=c(),nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1,npf=2,dev=F){standardGeneric('ccm')})
 #' a wrapper to produce a chart with labels on weekly data
 #'
 #' @param Wwatchlist instance
 #' @param ep Boolean  If true chart draws vertical lines on periods. Default value false.
 #' @param nof Boolean   If true no labels are presented on a chart. Default value false.
 #' @param nbp numeric   with value =  n > 1 charts n data windows ago. Default value 1
 #' @param scf numeric   Scale factor to increase the charted past window. Default value 1
 #' @return void. Wwatchlist instance is modified
 #' @aliases cc,Wwatchlist,ANY-method
setMethod('ccm','Wwatchlist'
					,function(.Object,vinst=c(),nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1,npf=2,dev=F){
						nameObject <- deparse(substitute(.Object))
            if(length(vinst)==0){
            	vinst <- getWatchInstruments(getOrderbook(.Object))
            }
            mldl <- .Object@mdata
            lngv <- length(vinst)
            layout(matrix(1:(4*lngv), nrow=4, ncol=1*lngv ))
            for(j in 1:length(vinst)){
            	instr <- vinst[j]
            	setSpecificInstrumentPosition(mldl, instr)
            	.Object@mdata <- mldl
						  ccc(.Object,nof=nof,pl=pl,plf=plf,vcolf=vcolf,scf=scf,nbp=nbp,npf=npf,dev=dev)
            }
            layout(1)
						assign(nameObject,.Object,envir=parent.frame())

						return( invisible() )
					})

#' @export
 #' @docType methods
setGeneric(name='ccs',function(.Object,tf="d1",ncol=2,vinst=c(),nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1,npf=2,dev=F){standardGeneric('ccs')})
 #' a wrapper to produce  several charts with labels on  specific time frame
 #'
 #' @param Wwatchlist instance
 #' @param ep Boolean  If true chart draws vertical lines on periods. Default value false.
 #' @param nof Boolean   If true no labels are presented on a chart. Default value false.
 #' @param nbp numeric   with value =  n > 1 charts n data windows ago. Default value 1
 #' @param scf numeric   Scale factor to increase the charted past window. Default value 1
 #' @return void. Wwatchlist instance is modified
 #' @aliases cc,Wwatchlist,ANY-method
setMethod('ccs','Wwatchlist'
					,function(.Object,tf="d1",ncol=2,vinst=c(),nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1,npf=2,dev=F){
						nameObject <- deparse(substitute(.Object))
            if(length(vinst)==0){
            	vinst <- getWatchInstruments(getOrderbook(.Object))
            }
            
            mldl <- .Object@mdata
            lngv <- length(vinst)
            if(lngv %% ncol > 0) {
            	nacrow <- lngv %/% ncol  + 1  
            }else{
            	nacrow <- lngv%/% ncol
            }
            #browser()
            layout(matrix(1:(2*nacrow*ncol), nrow=2*nacrow, ncol=ncol ))
            for(j in 1:length(vinst)){
            	instr <- vinst[j]
            	setSpecificInstrumentPosition(mldl, instr)
            	.Object@mdata <- mldl
            	switch(tf,
						"d1" = cc(.Object,tf="d1",nof=nof,pl=pl,csl=T,plf=plf,vcolf=vcolf,nbp=nbp,scf=scf,npf=npf,dev=dev,nlayout=T)
					   ,"w1" = cc(.Object,tf="w1",nof=nof,pl=pl,plf=plf,vcolf=vcolf,scf=scf,nbp=nbp,npf=npf,dev=dev,nlayout=T)
				)
            }
            layout(1)
			assign(nameObject,.Object,envir=parent.frame())

			return( invisible() )
})

#' intraday charting
 #' @export
 #' @docType methods
setGeneric(name='itcc',function(.Object,dfreqs=c(3600,600),scf=0.6,npf=1,nof=F,dev=F){standardGeneric('itcc')})
 #' @aliases itcc,Wwatchlist,ANY-method
setMethod('itcc','Wwatchlist'
					,function(.Object, dfreqs=c(3600,600),scf=0.6,npf=1,nof=F,dev=F){

		nameObject <- deparse(substitute(.Object))
		mldl <- getMarketdata(.Object)
		iname<-getCinstrument(mldl)
		ctime <- getCintradate(mldl)
		cstory <- getCurstory(.Object)
		zones_itr <- getAllIntradayTypefacts(cstory,'ZoneLine')

		#list of objects to pass for chart params
		lo <- list(ctime=ctime)
		lo$zone_itr <- zones_itr

		wlvls<- getLvlfacts(cstory, 'w1')
		lvls <- getLvlfacts(cstory, 'd1')

        lo$dlvl<-lvls
        lo$wlvl<-wlvls 
   
		assign('lo',lo,envir=parent.frame(n=npf))
         
		if(nof){
			TAstr <- 'addVo()'  ##  omit showing facts
		}else{
			#TAstr <- 'addVo();addLeveleod(lo$ctime)'
			TAstr <- 'addVo();addLeveleod(lo$ctime);addZonefact(lo$zone_itr)'
			#TAstr <- 'addVo();addLeveleod(lo$ctime);addZonefact(lo$zone_itr);addLevelfactIntrad(lo$dlvl)'
		}
		#browser()
		#set layout		
		layout(matrix(1:(2*length(dfreqs)),nrow=length(dfreqs)*2,ncol=1  ))
		
		for(j in 1:length(dfreqs)){
			chartName <- paste(iname,'_',dfreqs[j],sep='')
			ndd <- 1+ceiling(scf*getIntraDayWindowDays(dfreqs[j],isToint=TRUE)) #scf is a scale factor
			#ndd <- 30 # fixed number of days
			cxts <-getIntraCdatedata(mldl, iname, dfreq=dfreqs[j], ndays=ndd)
			#browser()
			d_offset <- ceiling(scf*getNbarsUnit(cxts))
			chartSeries(cxts[(nrow(cxts)-d_offset):nrow(cxts)]  #window
						,name=chartName,type='bars',layout=NULL
						,TA=TAstr )
						#,TA='addVo();addLeveleod(lo$ctime);addZonefact(lo$zone_itr)')
		}
		layout(1) # reset back layout
        rm('lo',envir=parent.frame(n=npf)) 
		assign(nameObject,.Object,envir=parent.frame())
		return( invisible() )

})

 #' intraday charting a single chart
 #'
 #' @export
 #' @docType methods
setGeneric(name='itcs',function(.Object,dfreq=3600,scf=1,npf=2,nof=F,dev=F){standardGeneric('itcs')})
 #'  a wrapper to produce a chart with labels on daily data
 #'
 #' sets  data current timeframe as daily
 #' @param Wwatchlist instance
 #' @param ep Boolean  If true chart draws vertical lines on periods. Default value false.
 #' @param nof Boolean   If true no labels are presented on a chart. Default value false.
 #' @param nbp numeric   with value =  n > 1 charts n data windows ago. Default value 1
 #' @param scf numeric   Scale factor to increase the charted past window. Default value 1
 #' #' @return void. Wwatchlist instance is modified
 #' @aliases itcs,Wwatchlist,ANY-method
setMethod('itcs','Wwatchlist'
					,function(.Object,dfreq=3600,scf=1,npf=2,nof=F,dev=F){

						nameObject <- deparse(substitute(.Object))
						itcc(.Object,dfreqs=dfreq,scf=scf,npf=npf,nof=nof,dev=dev)
						assign(nameObject,.Object,envir=parent.frame())
						return( invisible() )
					})



 #' @export
 #' @docType methods
setGeneric(name='cci',function(.Object,n=22, wave=TRUE, dlag=T,dev=F){standardGeneric('cci')})
 #'  produce a chart with selected indicators
 #'
 #' requires an active graphic device with a chart which will be recharted
 #' @param Wwatchlist instance
 #' @param n numeric  period for DonchianChannel indicator. Default value is 22
 #' @return void. Wwatchlist instance is unmodified
 #' @aliases cci,Wwatchlist,ANY-method
setMethod('cci','Wwatchlist'
					,function(.Object, n=22, wave=TRUE, dlag=T,dev=F){

						ctf <- getCurtimeframe(.Object)
						mldl <- .Object@mdata
						iname<-getCinstrument(mldl)
						tfn <- NA
						tfn[ ctf %in% 'w1' ] <- "weekly"
						tfn[ ctf %in% 'd1' ] <- 'daily'
						chartName <- paste(iname," ","(",tfn,")",sep="")

						dp <- getChartDataPeriodicity() ## get chart periodicity

						if ( dp$frequency == 86400){
							##daily data
							xdata <- getChartSubsetOhlcData()

							#donch <- DonchianNperiods(xdata, period="days", k=1, n=9, offset=0.25 )
							#donchmo <- DonchianNperiods(xdata, period="weeks", k=1, n=7, offset=0.25 )
							donch <- DonchianG(xdata, gperiod="weeks",offset=0.25 , dlag=dlag) 
							donchmo <- DonchianG(xdata, gperiod="months",offset=0.25, dlag=T)
							
							if(wave){      
								##zigzaglegs d1
								epsil <- 0.0001
								changeFact <- 0.99
								w1 <- to.weekly(xdata)
								avwrng <- mean( ( Hi(w1) - Lo(w1) ) / Cl(w1), na.rm=TRUE)
								zigChangeVal <- changeFact * avwrng * 100
								dhl <- cbind(Hi(xdata), Lo(xdata))
								zz <- as.xts(ZigZag(dhl, change=zigChangeVal, percent=TRUE )) #zigzag indic
								zzlegs <-ZigZagLegs(xdata, zz,epsilon=epsil)
							}
							## assign for monthly donchian
							mdtop <- donchmo$dtop
							mdbot <- donchmo$dbot
							mdtbord <- donchmo$dtbord
							mdbbord <- donchmo$dbbord
							assign('mdtop',mdtop,envir=parent.frame())
							assign('mdbot',mdbot,envir=parent.frame())
							assign('mdtbord',mdtbord,envir=parent.frame())
							assign('mdbbord',mdbbord,envir=parent.frame())

						}
						if ( dp$frequency == 604800  ){
							 ##weekly data
							 xdata <- getChartSubsetOhlcData()
							 donch <- DonchianNperiods(xdata, period="weeks", k=1, n=n, offset=0.25 )
							 #ma w1
						 if(wave){
							 #zigzaglegs w1
							 epsil <- 0.0001
							 changeFact <- 0.75
							 mo1 <-to.monthly(xdata)
							 avmorng <- mean( ( Hi(mo1) - Lo(mo1) ) / Cl(mo1), na.rm=TRUE)
							 zigChangeVal <- changeFact * avmorng * 100
							 whl <- cbind(Hi(xdata), Lo(xdata))
							 zz <- as.xts(ZigZag(whl, change=zigChangeVal, percent=TRUE )) #zigzag indic
							 zzlegs <-ZigZagLegs(xdata, zz,epsilon=epsil)
						 }
					 }

					 dtop <- donch$dtop
					 dbot <- donch$dbot
					 dmid <- donch$mid
					 dtbord <- donch$dtbord
					 dbbord <- donch$dbbord

					 assign('dtop',dtop,envir=parent.frame())
					 assign('dbot',dbot,envir=parent.frame())
					 assign('dmid',dmid,envir=parent.frame())
					 assign('dtbord',dtbord,envir=parent.frame())
					 assign('dbbord',dbbord,envir=parent.frame())
					 if(wave){
						 assign('zz',zz,envir=parent.frame())
						 assign('zzlegs',zzlegs,envir=parent.frame())
					 }

					wg <- getGraphconf(.Object)

			 theme.pars <- getThemepars(wg)
			 bartyp <- theme.pars$bartype
			 gtheme <-  theme.pars$themecolor
			 upcol <-   theme.pars$up.col
			 dncol <-   theme.pars$dn.col
			 btype<-"bars"
			 ##optionally print chart to a file
			 if(dev){
				 wwidth <- 14
				 wheight <- 8				
				 gf<-tempfile("chart",fileext=".pdf")
				 pdf(file=gf,width=wwidth,heigh=wheight)
				 print(paste("printed chart to file:",gf))
			 }				   
			 
					 ##plotting
					 if ( dp$frequency == 86400){
						if(wave){
						 chartSeries( xdata
								,name=chartName
								,TA='addVo();
										 addTA(dtop, on=1,   col="forestgreen", lty="dotted");
										 addTA(dtbord, on=1, col="brown", lty="dotted");
										 addTA(dmid, on=1,    col="blue", lty="dotted");
										 addTA(dbbord, on=1, col="magenta", lty="dotted");
										 addTA(dbot, on=1,   col="red", lty="dotted");
										 addTA(zz, on=1,   col="gray", lty="dashed", lwd=1);
										 addZigZagLegVo(zzlegs$LegCumVol,zzlegs$LegSide );
										 addTA(mdtop, on=1,   col="forestgreen", lty="solid");
										 addTA(mdbot, on=1, col="red", lty="solid");'
							 ,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme )
					 }else{
						 chartSeries( xdata
								,name=chartName
								,TA='addVo();
										 addTA(dtop, on=1,   col="forestgreen", lty="dotted");
										 addTA(dtbord, on=1, col="brown", lty="dotted");
										 addTA(dmid, on=1,    col="blue", lty="dotted");
										 addTA(dbbord, on=1, col="magenta", lty="dotted");
										 addTA(dbot, on=1,   col="red", lty="dotted");
										 addTA(mdtop, on=1,   col="forestgreen", lty="solid");
										 addTA(mdtbord, on=1,   col="brown", lty="dashed");
										 addTA(mdbbord, on=1, col="magenta", lty="dashed");
										 addTA(mdbot, on=1, col="red", lty="solid");'
							 ,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme )
					 }
					}
					if ( dp$frequency == 604800  ){
						if(wave){
						 chartSeries( xdata
								,name=chartName
								,TA='addVo();
										 addTA(dtop, on=1,   col="forestgreen", lty="dotted");
										 addTA(dtbord, on=1, col="brown", lty="dotted");
										 addTA(dmid, on=1,    col="blue", lty="dotted");
										 addTA(dbbord, on=1, col="magenta", lty="dotted");
										 addTA(dbot, on=1,   col="red", lty="dotted");
										 addTA(zz, on=1,   col="gray", lty="dashed", lwd=1);
										 addZigZagLegVo(zzlegs$LegCumVol,zzlegs$LegSide );'
						 ,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme )
						}else{
						 chartSeries( xdata
								,name=chartName
								,TA='addVo();
										 addTA(dtop, on=1,   col="forestgreen", lty="dotted");
										 addTA(dtbord, on=1, col="brown", lty="dotted");
										 addTA(dmid, on=1,    col="blue", lty="dotted");
										 addTA(dbbord, on=1, col="magenta", lty="dotted");
										 addTA(dbot, on=1,   col="red", lty="dotted");'
						 ,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme )

						}
					} 
					rm(dtop,envir=parent.frame())
					rm(dbot,envir=parent.frame())
					rm(dmid,envir=parent.frame())
					rm(dtbord,envir=parent.frame())
					rm(dbbord,envir=parent.frame())
					if(wave){
					 rm(zz,envir=parent.frame())
					 rm(zzlegs,envir=parent.frame())
					}
					 if(dev){dev.off(
						 )}
					 return( invisible() )
					})

	#' @export
	#' @docType methods
	setGeneric(name='ccv',function(.Object,tl=0.1,th=0.8,tuh=0.9,ep=T,wf=F,dev=F){standardGeneric('ccv')})
	#' print chart with low high and ultra-high volumes
	#' requires an active graphic device with a chart which will be recharted
	#'
	#' @param Wwatchlist instance
	#' @param tl,th,tuh numeric  thresholds for low, high and ultrahigh volumes
	#' @return void. Wwatchlist instance is unmodified
	#' @aliases ccv,Wwatchlist,ANY-method
	setMethod('ccv','Wwatchlist'
			,function(.Object, tl=0.1,th=0.8,tuh=0.9,ep=T,wf=F,dev=F){

				ctf <- getCurtimeframe(.Object)
				cstory <- getCurstory(.Object)
				mldl <- .Object@mdata
				iname<-getCinstrument(mldl)
				
				wdict <- getWdict(.Object)
				wkside <- getWykside(wdict)
				assign('wkside',wkside,envir=parent.frame(n=1))
				
				
				tfn <- NA
				tfn[ ctf %in% 'w1' ] <- "weekly"
				tfn[ ctf %in% 'd1' ] <- 'daily'
				chartName <- paste(iname," ","(",tfn,")",sep="")

				dp <- getChartDataPeriodicity() ## get chart periodicity
				##find out abnormal volumes
				xdata <- getChartSubsetOhlcData()
				vols <- Vo(xdata)
				tzxts <- indexTZ(xdata)

				#browser()
				lvt <- quantile(vols,tl)
				hvt <- quantile(vols,th)
				uhvt <- quantile(vols,tuh)
				lvls <-  ifelse(vols < lvt,-1,0)
				hvols <- ifelse(vols > hvt, 1,0)
				uhvols <- ifelse(vols > uhvt,2,0)
				allvols <- lvls + hvols + uhvols

			lvfacts <- lapply( index(vols[allvols==-1])
											 ,function(x,y){defWfact(iname,'L',x,y[x],'Low Volume',dp$units,dp$frequency,tzxts)}
									 ,y=Hi(xdata) )
			hvfacts <- lapply( index(vols[allvols==1])
								,function(x,y){defWfact(iname,'H',x,y[x],'High Volume',dp$units,dp$frequency,tzxts)}
								,y=Hi(xdata) )
			uhvfacts <- lapply( index(vols[allvols==3])
								,function(x,y){defWfact(iname,'U',x,y[x],'Ultra High Volume',dp$units,dp$frequency,tzxts)}
								,y=Hi(xdata) )

			vfacts <- c(lvfacts,hvfacts,uhvfacts)

			##find out abnormal progress
			clcl <- Cl(xdata)-Cl(lag(xdata))
			sp <- quantile(abs(clcl),tl,na.rm=T)
			gp <- quantile(abs(clcl),th,na.rm=T)
			rp <- quantile(abs(clcl),tuh,na.rm=T)


			sps <-  ifelse(abs(clcl) < sp,-1,0)
			gps <- ifelse(abs(clcl) > gp, 1,0)
			rps <- ifelse(abs(clcl) > rp,2,0)
			allps <- sps + gps + rps


			spfacts <- lapply( index(clcl[allps==-1])
					,function(x,y){defWfact(iname,'S',x,y[x],'Small Progress',dp$units,dp$frequency,tzxts)}
					,y=Lo(xdata) )
			gpfacts <- lapply( index(clcl[allps==1])
					,function(x,y){defWfact(iname,'G',x,y[x],'Large Progress',dp$units,dp$frequency,tzxts)}
					,y=Lo(xdata) )
			rpfacts <- lapply( index(clcl[allps==3])
					,function(x,y){defWfact(iname,'R',x,y[x],'Rare Ultra Progress',dp$units,dp$frequency,tzxts)}
					,y=Lo(xdata) )

			pfacts <- c(spfacts,gpfacts,rpfacts)

			#browser()
						npf <- 1


			assign('vfacts',vfacts,envir=parent.frame(n=npf))
			assign('pfacts',pfacts,envir=parent.frame(n=npf))
			### get wyckof facts for optional display
			wfs  <- getWfacts(cstory,  ctf)
			assign('wfs',wfs,envir=parent.frame(n=npf))

			## graph params
			wg <- getGraphconf(.Object)
			wp <- getWykgparams(wg, ctf)
			lp <- getLvlgparams(wg, ctf)

			assign('wp',wp,envir=parent.frame(n=npf))
			assign('lp',wp,envir=parent.frame(n=npf))

			theme.pars <- getThemepars(wg)
			bartyp <- theme.pars$bartype
			gtheme <-  theme.pars$themecolor
			upcol <-   theme.pars$up.col
			dncol <-   theme.pars$dn.col
						btype<-"bars"
			#fix lbltype and color
			vlbltype<-"ab"
			vcolor<-"blue"
			pcolor<-"darkmagenta"
			assign('vlbltype',vlbltype,envir=parent.frame(n=npf))
			assign('vcolor',vcolor,envir=parent.frame(n=npf))
			assign('pcolor',pcolor,envir=parent.frame(n=npf))

			##optionally print chart to a file
			if(dev){
				wwidth <- 14
				wheight <- 8				
				gf<-tempfile("chart",fileext=".pdf")
				pdf(file=gf,width=wwidth,heigh=wheight)
				print(paste("printed chart to file:",gf))
			}					
			
			##plotting
			if(!ep && !wf){
			 offsc <-1.0
			 assign('offsc',offsc,envir=parent.frame(n=npf))
			 chartSeries(xdata
					,name=chartName
					,TA='addfVo();
										 addWfact(vfacts,lbltype=vlbltype,lcex=wp$cex,loff=offsc*wp$offs,lupcol=vcolor,ldncol=vcolor,mside=wkside);
										 addWfact(pfacts,lbltype=vlbltype,lcex=wp$cex,loff=2.0*offsc*wp$offs,lupcol=pcolor,ldncol=pcolor,mside=wkside);'

						,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme)
				}
			if(!ep && wf){
				offsc <-3.0
				assign('offsc',offsc,envir=parent.frame(n=npf))
					chartSeries(xdata
						,name=chartName
						,TA='addfVo();
														addWfact(wfs,lbltype=wp$shape,lcex=wp$cex,loff=wp$offs,lupcol=wp$upcolor,ldncol=wp$dncolor,mside=wkside);
							addWfact(vfacts,lbltype=vlbltype,lcex=wp$cex,loff=offsc*wp$offs,lupcol=vcolor,ldncol=vcolor,mside=wkside);
											addWfact(pfacts,lbltype=vlbltype,lcex=wp$cex,loff=1.0*offsc*wp$offs,lupcol=pcolor,ldncol=pcolor,mside=wkside);'
					,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme)
			}
			if(ep && !wf){
				offsc <-1.0
				assign('offsc',offsc,envir=parent.frame(n=npf))
				chartSeries(xdata
						,name=chartName
						,TA='addfVoep();addEp();
								addWfact(vfacts,lbltype=vlbltype,lcex=wp$cex,loff=offsc*wp$offs,lupcol=pcolor,ldncol=pcolor,mside=wkside);
												addWfact(pfacts,lbltype=vlbltype,lcex=wp$cex,loff=2.0*offsc*wp$offs,lupcol=pcolor,ldncol=pcolor,mside=wkside);'
						,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme)
			}
			if(ep && wf){
				offsc <-3.0
				assign('offsc',offsc,envir=parent.frame(n=npf))
				chartSeries(xdata
						,name=chartName
						,TA='addfVoep();addEp();
														 addWfact(wfs,lbltype=wp$shape,lcex=wp$cex,loff=wp$offs,lupcol=wp$upcolor,ldncol=wp$dncolor,mside=wkside);
							 addWfact(vfacts,lbltype=vlbltype,lcex=wp$cex,loff=offsc*wp$offs,lupcol=pcolor,ldncol=pcolor,mside=wkside);
												 addWfact(pfacts,lbltype=vlbltype,lcex=wp$cex,loff=1.0*offsc*wp$offs,lupcol=pcolor,ldncol=pcolor,mside=wkside);'
					 ,type=btype,bar.type=bartyp, up.col=upcol, dn.col=dncol, theme=gtheme)
			}


			rm('offsc',envir=parent.frame(n=npf))
			rm('wfs',envir=parent.frame(n=npf))
			rm('vfacts',envir=parent.frame(n=npf))
			rm('pfacts',envir=parent.frame(n=npf))
			rm('wp',envir=parent.frame(n=npf))
			rm('vlbltype',envir=parent.frame(n=npf))
			rm('vcolor',envir=parent.frame(n=npf))
			rm('pcolor',envir=parent.frame(n=npf))
			
			if(dev){dev.off()}

			return(invisible())
					})

 #' @export
 #' @docType methods
setGeneric(name='crthelp',function(.Object){standardGeneric('crthelp')})
#' @aliases crthelp,Wwatchlist,ANY-method
setMethod('crthelp','Wwatchlist'
					,function(.Object){

							cat("cc(o), ::\t   chart current instrument  current timeframe \n")
							cat("cc(o,tf,nof=F,pl=F,plf=F,vcolf=F,scf=1,nbp=1,dev=F), ::\t   chart current instrument at current timeframe \n"
									,"\tparam nof=T will chart instrument without any artifacts\n"
							,"\tparam pl=T will chart endpoits vertical lines on aggregated periods\n"
							,"\tparam plf=T will chart vertical lines on Wyckof facts bar\n"
							,"\tparam vcolf=T will color volume bars on Wyckof facts bar\n"
							,"\tparam scf=1 will chart enables to chart larger backward periods\n"
											,"\tparam nbp (default=1) enables to chart a number of backward periods \n"
									,"\tparam dev=F  if TRUE prints chart into pdf file \n")
							cat("ccd(o,sig=T), ::\t   wrapper to chart current instrument  in daily timeframe. Show signals messages \n")
							cat("ccw(o), ::\t   wrapper to chart current instrument  in weekly timeframe \n")
							cat("ccv(o,ep=T,wf=F), ::\t  chart abnormal volumes in current timeframe\n"
									,"\tparam ep=T will chart endpoits vertical lines on aggregated periods\n"
									,"\tparam wf=T will add to chart instrument Wyckoff facts\n")
							cat("cci(o,n=22), ::\t   add to chart Donchian indicator on n periods \n")
							cat("ccc(o,nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1) ::\tchart current instrument in daily and weekly timeframe \n")
							cat("ccm(o,vinst=c(),nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1) ::\tchart a list of watched instruments in daily and weekly timeframe \n"
									,"\tif vinst=c() the ticker watch list will be read from o@orderbook@pfpars@watchinst \n")
							cat('ccs(o,tf="d1",ncol=2,vinst=c(),nof=F,pl=F,plf=F,vcolf=F,nbp=1,scf=1,npf=2,dev=F) ::\t chart a list of watched instruments\n '
								    ,'\t in a specific timeframe; if vinst=c() the ticker watch list will be read from o@orderbook@pfpars@watchinst \n')  
								

							cat("flipFctlbl(o), ::\t  flip Defined chart labels from numeric to appreviated \n")
							cat("flipbg(o), ::\t  flip  chart background color between light and dark \n")
							cat("upgpar(o), dngpar(o), inigpar(o) ::\t  increase,decrease,reset size of artifacts on charts \n")
							cat("inigpar(o), ::\t  (re)initialize graphic parameters for charts \n")

})
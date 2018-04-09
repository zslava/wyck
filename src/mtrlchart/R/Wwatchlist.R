# TODO: check if deprecated setInstrument4curstory()<-
# TODO: chck if deprecated getSpecificStory

# Author: zimine
###############################################################################

#' instrument list with user operations interface
#'
#' S4 which holds an Mldata object with market data,
#' a list of instrument stories  as well as user operations methods to
#' manage  artifacts  and to produce instrument charts
#' @rdname Wwatchlist-class
#' @name Wwatchlist
#' @exportClass Wwatchlist
setClass(Class='Wwatchlist'
         ,representation(curwstoryid='numeric'
                        ,curtimeframe='character'
                        ,dsubset='numeric'
                        ,wsubset='numeric'
                        ,hsubset='numeric'

                        ,rdatfile='character'
                        ,wdict='Wdict'
                        ,wstories='list'  #deprecated field
                        ,mdata='Mldata'
                        ,wgconf='Wgconf'
                        ,storybook='Wstorybook'
                        ,orderbook='Orderbook'
                        )
         ,prototype(  curwstoryid=0
                       ,curtimeframe='d1'
                       ,dsubset=250
                       ,wsubset=1200
                       ,hsubset=8

                       ,rdatfile='~/Dropbox/cdat/wwl.rdat'
                       ,wdict=new('Wdict')
                       ,wstories=list()
                       ,mdata=new('Mldata')
                       ,wgconf=new('Wgconf')
	                     ,storybook=new('Wstorybook')
                       ,orderbook=new('Orderbook')
                   )
         )


##user-friend functions for constructors

barewwlist <-function()
  {
  o <- new('Wwatchlist')
  return(o)
 }
#' Initializes watchlist from a vector with instrument ids
#'
#' @export
wwlist <-function(instrvec){

  o <- new('Wwatchlist' )
  wsbook <- initWstorybook(instrvec)
  setStorybook(o)  <- wsbook

  ## set wgconf
  wgc <- wGraphconf()
  setGraphconf(o) <- wgc

  return(o)
}

#' Initializes watchlist from existing Mldata object
#'
#' @export
wwlistfromdata <-function(mldata){

  if(!is.Mldata(mldata)){
          stop(paste("[wwlistfromdata validation]"
         ," input value should be of type of Mldata"))
        }

  ivec <-getSortedinstruments(mldata)
  o <- new('Wwatchlist' )

	wsbook <- initWstorybook(ivec)
	setStorybook(o)  <- wsbook

  ##set market data
  o@mdata <- mldata

  ## set wgconf
  wgc <- wGraphconf()
  setGraphconf(o) <- wgc
  return(o)
}



#' loads saved Wwatchlist object from rdat file
#'
#' @export
lwl <-function(rdatfile,verbose=TRUE){
  load(rdatfile)
  ##new dev 
  #browser()
  if (file.exists(getMdatFilename(.Object))){
    if(verbose){
      cat("loading mdata from separate file ", getMdatFilename(.Object), "\n")
    }
    load(getMdatFilename(.Object))
    setMarketdata(.Object) <- .mldataobj
  }
  if(verbose){
      cat("loading watchlist from file ", rdatfile, "\n")
  }
  return(.Object)
}

#' @export
rwl <-function(object){
  load(object@rdatfile)
  return(.Object)
}


#' reads saved Wwatchlist object from default location of rdat file
#'
#' @export
savedwwlistdefault <-function(){
  rdatfile <- new('Wwatchlist')@rdatfile
  load(rdatfile)
  return(.Object)
}


#' @export
#' @docType methods
setGeneric(name='getCurinstrument'
		,function(object){standardGeneric('getCurinstrument')})
#' @aliases getCurinstrument,Wwatchlist,ANY-method
setMethod('getCurinstrument'
		,'Wwatchlist'
		,function(object){
			mdata <- getMarketdata(object)
			cinst <- getCinstrument(mdata)
			return ( cinst )
		})


#' @export
#' @docType methods
setGeneric(name='getCurstory'
               ,function(object){standardGeneric('getCurstory')})
#' @aliases getCurstory,Wwatchlist,ANY-method
setMethod('getCurstory'
                 ,'Wwatchlist'
                 ,function(object){
					 cinst <- getCurinstrument(object)
					 wsbook <- getStorybook(object)
					 cstory <- getStory(wsbook,cinst)
                     return ( cstory )
                 })



 #' @export
 #' @docType methods
setGeneric(name='updStory<-'
               ,function(object,value){standardGeneric('updStory<-')})
setReplaceMethod('updStory'
                 ,'Wwatchlist'
                 ,function(object,value){
    			   wsbook <- getStorybook(object)
				   updateStory(wsbook) <- value
				   setStorybook(object) <- wsbook
                   return(object)
                 })



 #' @export
 #' @docType methods
setGeneric(name='getCurtimeframe'
               ,function(object){standardGeneric('getCurtimeframe')})
#' @aliases getCurtimeframe,Wwatchlist,ANY-method
setMethod('getCurtimeframe'
                 ,'Wwatchlist'
                 ,function(object){
                     return ( object@curtimeframe )
                 })

 #' @export
 #' @docType methods
setGeneric(name='getCurtimefrequency'
               ,function(object,timeframe){standardGeneric('getCurtimefrequency')})
#' @aliases getCurtimefrequency,Wwatchlist,ANY-method
setMethod('getCurtimefrequency'
                 ,'Wwatchlist'
                 ,function(object,timeframe){
                  if(missing(timeframe)) { 
                    timeframe <-object@curtimeframe 
                  }
                  freq<-as.numeric(NA)
                  if ( grepl('intra_',timeframe)) {
                    freq<-as.numeric(unlist(strsplit(timeframe,'intra_'))[2])
                  }else{
                    freq <- switch(timeframe
                      ,'y1' = 31536000
                      ,'m1' = 2678400
                      ,'w1' =  604800
                      ,'d1' = 86400                  
                    )
                  }
                  return ( freq)
                 })


 #' @export
 #' @docType methods
setGeneric(name='setCurtimeframe<-'
          ,function(object,value){standardGeneric('setCurtimeframe<-')})
setReplaceMethod('setCurtimeframe'
                 ,'Wwatchlist'
                 ,function(object,value){
                     ##validation
                     if ( (  value != 'w1'
                          && value != 'd1'
                          && value != 'h1' #
                           )){
                          stop(paste("[Wwatchlist:setCurtimeframe validation]"
                                    ," bad value of value param"))
                     }
                       object@curtimeframe <- value
                     return (object)
                 })
 #' @export
 #' @docType methods
setGeneric(name='getSubset',function(object,tf){standardGeneric('getSubset')})
#' @aliases getSubset,Wwatchlist,ANY-method
setMethod('getSubset'
                 ,'Wwatchlist'
                 ,function(object,tf){
                   if ((  tf != 'w1'
                       && tf != 'd1'
                       && tf != 'h1' )){
                          stop(paste("[Wstory:getSubset validation]"
                                    ," bad value of tf param"))
                    }
                      csub <- NA
                      csub[ tf %in% 'w1' ] <- object@wsubset
                      csub[ tf %in% 'd1' ] <- object@dsubset
                      csub[ tf %in% 'h1' ] <- object@hsubset
                     return(csub)
                 })
 #' @export
 #' @docType methods
setGeneric(name='setSubset<-',function(object,tf,value){standardGeneric('setSubset<-')})
setReplaceMethod('setSubset'
                 ,'Wwatchlist'
                 ,function(object,tf,value){
                   if ((  tf != 'w1'
                       && tf != 'd1'
                       && tf != 'h1' )){
                          stop(paste("[Wstory:setSubset validation]"
                                    ," bad value of tf param"))
                    }
                      if( tf %in% 'w1'   ) {  object@wsubset <- value }
                      if( tf %in% 'd1' ) {  object@dsubset <- value }
                      if( tf %in% 'h1' ) { object@hsubset <- value }
                   return(object)
                 })

 #' @export
 #' @docType methods
setGeneric(name='getWdict'
               ,function(object){standardGeneric('getWdict')})
#' @aliases getWdict,Wwatchlist,ANY-method
setMethod('getWdict'
                 ,'Wwatchlist'
                 ,function(object){
                   return ( object@wdict )
                 })
 #' @export
 #' @docType methods
setGeneric(name='setWdict<-'
               ,function(object,value){standardGeneric('setWdict<-')})
setReplaceMethod('setWdict'
                 ,'Wwatchlist'
                 ,function(object,value){
                     ##validate input
                     if ( !is(value, 'Wdict'))  {
                          stop(paste("[Wwatchlist:setWdict validation]"
                                    ," input value should be of type of Wdict"))
                     }
                     object@wdict <-value
                     return (object)
                 })
 #' @export
 #' @docType methods
setGeneric(name='getGraphconf'
               ,function(object){standardGeneric('getGraphconf')})
#' @aliases getGraphconf,Wwatchlist,ANY-method
setMethod('getGraphconf'
                 ,'Wwatchlist'
                 ,function(object){
                   return ( object@wgconf )
                 })
 #' @export
 #' @docType methods
setGeneric(name='setGraphconf<-'
               ,function(object,value){standardGeneric('setGraphconf<-')})
setReplaceMethod('setGraphconf'
                 ,'Wwatchlist'
                 ,function(object,value){
                     ##validate input
                     if ( !is(value, 'Wgconf'))  {
                          stop(paste("[Wwatchlist:Wgconf validation]"
                                    ," input value should be of type of Wgconf"))
                     }
                     object@wgconf <-value
                     return (object)
                 })

 #' @export
 #' @docType methods
 setGeneric(name='getStorybook'
		 ,function(object){standardGeneric('getStorybook')})
#' @aliases getStorybook,Wwatchlist,ANY-method
 setMethod('getStorybook'
		 ,'Wwatchlist'
		 ,function(object){
			 return ( object@storybook )
		 })

 #' @export
 #' @docType methods
 setGeneric(name='setStorybook<-'
		 ,function(object,value){standardGeneric('setStorybook<-')})
 setReplaceMethod('setStorybook'
		 ,'Wwatchlist'
		 ,function(object,value){
			 ##validate input
			 if ( !is(value, 'Wstorybook'))  {
				 stop(paste("[Wwatchlist:setStorybook validation]"
								 ," input value should be of type of Wstorybook"))
			 }
			 object@storybook <-value
			 return (object)
		 })

 #' @export
 #' @docType methods
 setGeneric(name='getOrderbook'
     ,function(object){standardGeneric('getOrderbook')})
#' @aliases getOrderbook,Wwatchlist,ANY-method
 setMethod('getOrderbook'
     ,'Wwatchlist'
     ,function(object){
       return ( object@orderbook )
})

 #' @export
 #' @docType methods
 setGeneric(name='setOrderbook<-'
     ,function(object,value){standardGeneric('setOrderbook<-')})
 setReplaceMethod('setOrderbook'
     ,'Wwatchlist'
     ,function(object,value){
       ##validate input
       if ( !is(value, 'Orderbook'))  {
         stop(paste("[Wwatchlist:setOrderbook validation]"
                 ," input value should be of type of Orderbook"))
       }
       object@orderbook <-value
       return (object)
})


 #' @export
 #' @docType methods
 setGeneric(name='getMarketdata'
		 ,function(object){standardGeneric('getMarketdata')})
#' @aliases getMarketdata,Wwatchlist,ANY-method
 setMethod('getMarketdata'
		 ,'Wwatchlist'
		 ,function(object){
			 return ( object@mdata )
		 })


 #' @export
 #' @docType methods
setGeneric(name='setMarketdata<-'
               ,function(object,value){standardGeneric('setMarketdata<-')})
setReplaceMethod('setMarketdata'
                 ,'Wwatchlist'
                 ,function(object,value){
                     ##validate input
                     if ( !is(value, 'Mldata'))  {
                          stop(paste("[Wwatchlist:setMarketdata validation]"
                                    ," input value should be of type of Mldata"))
                     }

                     mdatInstruments <- value@imetadata$IDNAM
					          wsbook <- getStorybook(object)
                     wsitrs <- unlist(lapply(wsbook@wstories,function(x) { x@instr } ))

                     ick <- match(mdatInstruments, wsitrs)
                     hasNas <- ick[is.na(ick)]

                     if ( length(hasNas) > 0 ){
                       cat("wstories instruments:", wsitrs, "\n")
                       cat("market data instruments:",mdatInstruments, "\n")
                       stop(paste("[Wwatchlist:setMarketdata validation]"
                                    ,"instruments vector mismatch"))
                     }
                     object@mdata <- value
                     return (object)
                 })

 #' @export
 #' @docType methods
 setGeneric(name='getIntraDate'
     ,function(object){standardGeneric('getIntraDate')})
#' @aliases getIntraDate,Wwatchlist,ANY-method
 setMethod('getIntraDate'
     ,'Wwatchlist'
     ,function(object){
       return ( object@mdata@mpars$cintrdate$idate )
     })

 
 #' @export
 #' @docType methods
setGeneric(name='setIntraDate',function(.Object)
                                     {standardGeneric('setIntraDate')})
#' @aliases setIntraDate,Wwatchlist,ANY-method
setMethod('setIntraDate'
          ,'Wwatchlist'
          ,function(.Object){
              nameObject <- deparse(substitute(.Object))
              mldat <- getMarketdata(.Object)
              syncIntradayDate(mldat) #logic in Mldata class
              setMarketdata(.Object) <- mldat
              lfmt <- "%Y-%m-%d %H:%M:%S"
              print(paste("intraday date set to: "
                , format( mldat@mpars$cintrdate$idate, format=lfmt)
                , "timezone ", mldat@mpars$cintrdate$itz  ))
              assign(nameObject,.Object,envir=parent.frame())
              return(invisible())
          })



 #' @export
 #' @docType methods
setGeneric(name='addWatchGroup',function(.Object,ngrp, pos)
                                     {standardGeneric('addWatchGroup')})
#' @aliases addWatchGroup,Wwatchlist,ANY-method
setMethod('addWatchGroup'
          ,'Wwatchlist'
          ,function(.Object,ngrp, pos){
              nameObject <- deparse(substitute(.Object))
              mldat <- .Object@mdata
              insertGroup(mldat, ngrp, pos)
              .Object@mdata <- mldat
              assign(nameObject,.Object,envir=parent.frame())
          })

 #' @export
 #' @docType methods
setGeneric(name='addWatchInstrument',function(.Object,nam,tkr,typ,lbl,grp)
                                     {standardGeneric('addWatchInstrument')})
#' @aliases addWatchInstrument,Wwatchlist,ANY-method
setMethod('addWatchInstrument'
          ,'Wwatchlist'
          ,function(.Object,nam,tkr,typ,lbl,grp){
              nameObject <- deparse(substitute(.Object))
              #mdata is  defined (current case)
           if (!is.na( getCinstrument(.Object@mdata))){
                   mldat <- .Object@mdata
                   addMarketdata(mldat,nam,tkr,typ,lbl,grp)
                   .Object@mdata <- mldat
           }
           # add new story object to the list of stories
			wsbook <- getStorybook(.Object)
            ws <- wstory(nam)
            addWstory(wsbook) <- ws
			setStorybook(.Object) <- wsbook
            assign(nameObject,.Object,envir=parent.frame())
            return ( invisible() )
          })


 #' @export
 #' @docType methods
setGeneric(name='rmWatchInstrument',function(.Object,nam)
                                     {standardGeneric('rmWatchInstrument')})
#' @aliases rmWatchInstrument,Wwatchlist,ANY-method
setMethod('rmWatchInstrument'
          ,'Wwatchlist'
          ,function(.Object,nam){
             nameObject <- deparse(substitute(.Object))
             mldat <- .Object@mdata
             mdatInstrs <- mldat@imetadata$IDNAM
             inst2del <- match(nam, mdatInstrs)
             if (!is.na(inst2del)){
                qa<-readline(paste('are you sure you want to delete instrument'
                                    ,mdatInstrs[inst2del],'from watch list? [All stored artifacts will be lost!](y/n)'))
                if ( qa =="y"){
                   #remove instrument from market data
                   rmMarketdata(mldat,nam)
                   .Object@mdata <- mldat
                   #remove new story object to the list of stories
				   wsbook <- getStorybook(.Object)
                   rmWstory(wsbook,nam)
				   setStorybook(.Object) <- wsbook
                }else{
                  cat("Cancelled\n")
                }
             }
             assign(nameObject,.Object,envir=parent.frame())
             return ( invisible() )
           })





#' @export
#' @docType methods
   setGeneric(name='getRdatfile'
		   ,function(object){standardGeneric('getRdatfile')})
#' @aliases getRdatfile,Wwatchlist,ANY-method
   setMethod('getRdatfile'
		   ,'Wwatchlist'
		   ,function(object){
			   return ( object@rdatfile )
		   })

#### storage methods
 #' @export
 #' @docType methods
setGeneric(name='setRdatfile<-'
               ,function(object,value){standardGeneric('setRdatfile<-')})
setReplaceMethod('setRdatfile'
                 ,'Wwatchlist'
                 ,function(object,value){
                     ##validate input
                     if ( !is.character(value))  {
                          stop(paste("[Wwatchlist:setRdatfile validation]"
                                ," input value should be a character full filepath"))
                     }
                     object@rdatfile  <- value
                     return (object)
                 })
 #' @docType methodsa 
setGeneric(name='getMdatFilename'
               ,function(.Object){standardGeneric('getMdatFilename')})
#' @aliases getMdatFilename,Wwatchlist,ANY-method
setMethod('getMdatFilename'
                 ,'Wwatchlist'
                 ,function(.Object){
                   wlfile <- .Object@rdatfile
                   sdir <- dirname(wlfile)
                   sname <- basename(wlfile)
                   fpart <- unlist( strsplit(basename(sname),".",fixed=TRUE))
                   nname <- paste(fpart[1], "_mldata.", fpart[2],sep="")
                   mlfile <- file.path(sdir,nname) 
                   return(mlfile)
                 })



 #' @export
 #' @docType methods
setGeneric(name='persist'
               ,function(.Object,withmarketdata=FALSE,verbose=FALSE){standardGeneric('persist')})
#' @aliases persist,Wwatchlist,ANY-method
setMethod('persist'
                 ,'Wwatchlist'
                 ,function(.Object, withmarketdata=FALSE,verbose=FALSE){
                  mldata <- getMarketdata(.Object)
                  if(!withmarketdata){ 
                    setMarketdata(.Object) <- new('Mldata') ##discard mdata
                    if(verbose){
                      cat("saving watchlist without marketdata to file ", .Object@rdatfile, "\n") 
                    }
                  }
                  nameObject <- deparse(substitute(.Object))
                  save(nameObject, .Object, file=.Object@rdatfile)
                  setMarketdata(.Object) <- mldata  ##reassign mkt data 
                   return(invisible())
                 }
          )


#' @export
#' @docType methods
setGeneric(name='persistMdata'
               ,function(.Object,verbose=TRUE){standardGeneric('persistMdata')})
#' @aliases persistMdata,Wwatchlist,ANY-method
setMethod('persistMdata'
                 ,'Wwatchlist'
                 ,function(.Object,verbose=TRUE){
                   mlfile <- getMdatFilename(.Object)
                   .mldataobj <- getMarketdata(.Object)
                   nameObject <- deparse(substitute(.mldataobj))
                   save(nameObject, .mldataobj, file=mlfile)
                   if(verbose){
                     cat("saved marketdata in separate file: ", mlfile, "\n")
                   }
                   return(invisible())
                 }) 




##bulk replace wyk label  , admin op rarely used ##
 #' @export
 #' @docType methods
setGeneric(name='bulkreplaceWyklbl',function(.Object,oslb,nslb,nllb)
                                   {standardGeneric('bulkreplaceWyklbl')})
#' @aliases bulkreplaceWyklbl,Wwatchlist,ANY-method
setMethod('bulkreplaceWyklbl','Wwatchlist'
          ,function(.Object,oslb,nslb,nllb){
             nameObject <- deparse(substitute(.Object))

			 wsbook <- getStorybook(.Object)
			 globalreplaceWyklbl(wsbook,oslb,nslb,nllb)
			 setStorybook(.Object) <- wsbook
             assign(nameObject,.Object,envir=parent.frame())
            return(invisible())
          })

##bulk 
 #' @export
 #' @docType methods
setGeneric(name='changeDate',function(.Object,ny=0,nd=0,isRand=TRUE,hasIntrd=FALSE)
                                   {standardGeneric('changeDate')})
#' @aliases changeDate,Wwatchlist,ANY-method
setMethod('changeDate','Wwatchlist'
          ,function(.Object,ny=0,nd=0,isRand=TRUE,hasIntrd=FALSE){
      nameObject <- deparse(substitute(.Object))

      if(isRand){
         ny <- -1* floor(runif(1,0,80))
      }
      mdata <- getMarketdata(.Object)
      #Mldata
      addDate(mdata, ny=ny,nd=nd,hasIntrd=hasIntrd)

      #orderbook
      .Object@orderbook@pfpars$iniDate <- getCdate(mdata)

      .Object@mdata <- mdata
      
      assign(nameObject,.Object,envir=parent.frame())
      return(invisible())
})



####print help methods
 #' @export
 #' @docType methods
setGeneric(name='whelp',function(object){standardGeneric('whelp')})
#' @aliases whelp,Wwatchlist,ANY-method
setMethod('whelp','Wwatchlist'
          ,function(object){
             wd <- object@wdict
             show(wd)
              return(invisible())
          })

 #' @export
 #' @docType methods
setGeneric(name='ohelp',function(object, flat=FALSE){standardGeneric('ohelp')})
#' @aliases ohelp,Wwatchlist,ANY-method
setMethod('ohelp','Wwatchlist'
          ,function(object, flat= FALSE){
      cat("#############################################################\n")
      cat("** Wyckof artifacats **\n")
      cat("#############################################################\n")
      if(flat) { whelp(object) }
      else { cat('whelp(o) ::\t  help on Wyckoff registered set of artifacts\n') }
      cat("#############################################################\n")
      cat("*** Market data manipulation functions  ***\n")
      cat("#############################################################\n")
      if(flat) { wldhelp(object) }
      else{ cat('wldhelp(o) ::\t  help on attaching, forwarding market data \n')  }             
      cat("#############################################################\n")
      cat('***Artifact manipulation functions***\n')
      cat("#############################################################\n")
      if(flat) { wfhelp(object) }
      else{ cat('wfhelp(o) ::\t  help on Wyckoff artifacts manipulation\n') }
      cat("#############################################################\n")
      cat('**Charting manipulation functions**\n')
      cat("#############################################################\n")
      if(flat) { crthelp(object) }
      else{cat('crthelp(o) ::\t help on charting wyckoff facts\n')}
      cat("#############################################################\n")
        			cat('**Report generation functions**\n')
      cat("#############################################################\n")
      if(flat) { prephelp(object) }
      else{ cat('prephelp(o) ::\t help on generating print report\n')}
      cat("#############################################################\n")
      cat('**Trade Order management functions**\n')
      cat("#############################################################\n")
      if(flat) { trhelp(object) }
      else {cat('trhelp(o) ::\t  help on trade , order management user functions\n')}
      cat("\n#############################################################\n")
      cat('**Help on help **\n')
      cat("#############################################################\n")
      cat('ohelp(o, flat=TRUE) ::\t  print all help files \n')
      cat("#############################################################\n")
        return(invisible())
            })

###helper methods
#' @export
setMethod ('show' ,'Wwatchlist'
           ,function(object){
              cat("*** Type Wwatchlist,*** \n")
              cat("contains market stories:\n")
			         wsbook <-getStorybook(object)
              for (j in  1:length(wsbook@wstories) ){
                ws <- wsbook@wstories[[j]]
                show(ws)
              }
             cat("\n")
             #cat("Graph conf params\n")
             #show(object@wgconf)
             #cat("\n")
             ##important operational stuff
             cat("* Market data and stories container specs:\n")
             cat(paste("rdat file", object@rdatfile, "\n"))
             cat("contains market data:\n")
             show(object@mdata)
             
             dfmt <- '%Y-%m-%d %Z %A'
             s1fmt <- '%Y-%m-%d %H:%M:%S'
             cat("CURRENT INSTRUMENT: ", getCinstrument(object@mdata), "\n")
             cat("CURRENT TIMEFRAME: ", getCurtimeframe(object), "\n")
             cdt_tz <- base::format(getCdate(object@mdata), format="%Z")
             cat("CURRENT DATE: ", base::format(getCdate(object@mdata),format=dfmt)
                ,if(!is.null(object@mdata@mpars$cintrdate))paste("time:", format(getCintradate(object@mdata) ,format=s1fmt))else paste(" ")                 
                                 , "\n")
             cat("*** Type Wwatchlist End.*** \n")

            })



#' @export
#' @docType methods
setGeneric( name='is.Wwatchlist',function(object){standardGeneric("is.Wwatchlist")})
setMethod('is.Wwatchlist'
          ,'Wwatchlist'
          ,function(object){
            return ( is(object, 'Wwatchlist') )
           }
          )



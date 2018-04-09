# TODO: move to this container  chart theme params
# Author: zimine
###############################################################################

#' Wgconf type
#'
#' Simple container  to hold chart color, size params
#'
#'
#'
#'
#' @keywords internal
#' @rdname Wgconf-class
#' @name Wgconf
setClass(Class='Wgconf'
		,representation(wykshap='character'
				,wykupcolw1='character'
				,wykupcold1='character'
				,wykupcolh1='character'
				,wykdncolw1='character'
				,wykdncold1='character'
				,wykdncolh1='character'

				,wykcexdflt='numeric'
				,wykcexdt='numeric'
				,wykcex='numeric'
				,wykoffdflt='numeric'
				,wykoffdt='numeric'
				,wykoff='numeric'
				
				,lvlcolw1='character'
				,lvlcold1='character'
				,lvlcolh1='character'
				,iclcolw1='character'
				,iclcold1='character'
				,iclcolh1='character'
				,hflcolw1='character'
				,hflcold1='character'
				,hflcolh1='character'
				,lvlcexdflt='numeric'
				,lvlcexdt='numeric'
				,lvlcex='numeric'
				,lvloffdflt='numeric'
				,lvloffdt='numeric'
				,lvloff='numeric'
				
				,trlcolw1='character'
				,trlcold1='character'
				,trlcolh1='character'
				
				,thm.bartype='character'
				,thm.lightupcol='character'
				,thm.lightdncol='character'
				,thm.darkupcol='character'
				,thm.darkdncol='character'
				,thm.col='character'
		)
		,    prototype(wykshap=as.character(NA)
				,wykupcolw1=as.character(NA)
				,wykupcold1=as.character(NA)
				,wykupcolh1=as.character(NA)
				,wykdncolw1=as.character(NA)
				,wykdncold1=as.character(NA)
				,wykdncolh1=as.character(NA)

				,wykcexdflt=as.numeric(NA)
				,wykcexdt=as.numeric(NA)
				,wykcex=as.numeric(NA)
				,wykoffdflt=as.numeric(NA)
				,wykoffdt=as.numeric(NA)
				,wykoff=as.numeric(NA)
				
				,lvlcolw1=as.character(NA)
				,lvlcold1=as.character(NA)
				,lvlcolh1=as.character(NA)
				
				,iclcolw1=as.character(NA)
				,iclcold1=as.character(NA)
				,iclcolh1=as.character(NA)
				,hflcolw1=as.character(NA)
				,hflcold1=as.character(NA)
				,hflcolh1=as.character(NA)
				,lvlcexdflt=as.numeric(NA)
				,lvlcexdt=as.numeric(NA)
				,lvlcex=as.numeric(NA)
				,lvloffdflt=as.numeric(NA)
				,lvloffdt=as.numeric(NA)
				,lvloff=as.numeric(NA)
				
				,trlcolw1=as.character(NA)
				,trlcold1=as.character(NA)
				,trlcolh1=as.character(NA)
				
				,thm.bartype=as.character(NA)
				,thm.lightupcol=as.character(NA)
				,thm.lightdncol=as.character(NA)
				,thm.darkupcol=as.character(NA)
				,thm.darkdncol=as.character(NA)
				,thm.col=as.character(NA)
		
		)
)

##helper function  for construction
#' Initialized Wgconf
#'
#' Constructor function to create new instance of Wgconf
#'
#' @return  Wgconf instance
#' @keywords internal
#' @examples
#' \dontrun{wg <- wGraphconf()}
#' @export
wGraphconf <-function(){
   o <- new('Wgconf')
   resetDefaults(o)
   return(o)
}


###biz methods
#' @docType methods
setGeneric(name='resetDefaults',function(.Object)
                                     {standardGeneric('resetDefaults')})
#' @aliases resetDefaults,Wgconf,ANY-method
setMethod('resetDefaults'
          ,'Wgconf'
          ,function(.Object){
            nameObject <- deparse(substitute(.Object))

            ##wyckoff fact
            .Object@wykshap <- 'ab'  #alternative value  'ab' or 'dt'
			
            .Object@wykdncolw1 <- 'brown'
            .Object@wykdncold1 <- 'red'
            .Object@wykdncolh1 <- 'orangered'

			.Object@wykupcolw1 <- 'green4'
			.Object@wykupcold1 <- 'darkgreen'
			.Object@wykupcolh1 <- 'darkseagreen'
			

            .Object@wykcexdflt <- 0.5
            .Object@wykcexdt   <- 0.05
            .Object@wykcex <- .Object@wykcexdflt

            .Object@wykoffdflt <- 0.30
            .Object@wykoffdt   <- 0.125
            .Object@wykoff <- .Object@wykoffdflt
             ## lvl level params
            .Object@lvlcolw1<- 'blue'
            .Object@lvlcold1<- 'navy'
            .Object@lvlcolh1<- 'blueviolet'

            .Object@iclcolw1<- 'darkred'
            .Object@iclcold1<- 'brown'
            .Object@iclcolh1<- 'brown4'

            .Object@hflcolw1<- 'darkgreen'
            .Object@hflcold1<- 'forestgreen'
            .Object@hflcolh1<- 'green3'

            .Object@lvlcexdflt <- 0.5
            .Object@lvlcexdt   <- 0.05
            .Object@lvlcex <- .Object@lvlcexdflt

            .Object@lvloffdflt <- 0.10
            .Object@lvloffdt   <- 0.05
            .Object@lvloff <- .Object@lvloffdflt

            ## trl trend params
            .Object@trlcolw1<- 'red'
            .Object@trlcold1<- 'hotpink'
            .Object@trlcolh1<- 'pink'

			
	     ## theme color params
	     .Object@thm.bartype="ohlc"
	     .Object@thm.lightupcol="gray30"
		 .Object@thm.lightdncol="palevioletred"
		 .Object@thm.darkupcol="green"
	     .Object@thm.darkdncol="orange"
	     .Object@thm.col="white"
			

            assign(nameObject,.Object,envir=parent.frame())
            return ( invisible() )
          })
		  

#' @docType methods
setGeneric(name='getThemepars'
		,function(.Object){standardGeneric('getThemepars')})
#' @aliases getThemepars,Wgconf,ANY-metho
setMethod('getThemepars'
	,'Wgconf'
	,function(.Object){
		nameObject <- deparse(substitute(.Object))
		 theme.color <- .Object@thm.col
		 bartype <- .Object@thm.bartype
		 if (theme.color == "white" || theme.color == "wheat"  ){
		  up.color <- .Object@thm.lightupcol
		  dn.color <- .Object@thm.lightdncol
		 }else{
		  up.color <- .Object@thm.darkupcol
		  dn.color <- .Object@thm.darkdncol
		 }
		 
		 tpars <- list('bartype'=bartype
		               ,'themecolor'=theme.color
		               ,'up.col'=up.color
			       ,'dn.col'=dn.color
			        )
		  return(tpars)			   
		})
		  
#' @docType methods
setGeneric(name='flipChartbg'
				  ,function(.Object){standardGeneric('flipChartbg')})
#' @aliases flipChartbg,Wgconf,ANY-metho
setMethod('flipChartbg'
				  ,'Wgconf'
				  ,function(.Object){
					  nameObject <- deparse(substitute(.Object))
					  tmp <- .Object@thm.col
					  if ( tmp == 'white') { .Object@thm.col <- "black" }
					  if ( tmp  == 'black') { .Object@thm.col <- 'white' }
					  assign(nameObject,.Object,envir=parent.frame())
					  return(invisible())
				  })		  
		  
#' @docType methods
setGeneric(name='flipWykshape'
           ,function(.Object){standardGeneric('flipWykshape')})
#' @aliases flipWykshape,Wgconf,ANY-metho
setMethod('flipWykshape'
          ,'Wgconf'
          ,function(.Object){
             nameObject <- deparse(substitute(.Object))
             tmp <- .Object@wykshap
             if ( tmp == 'ab') { .Object@wykshap <- 'dt' }
             if ( tmp  == 'dt') { .Object@wykshap <- 'ab' }
             assign(nameObject,.Object,envir=parent.frame())
            return(invisible())
          })

#' @docType methods
setGeneric(name='parsUpsize'
           ,function(.Object){standardGeneric('parsUpsize')})
#' @aliases parsUpsize,Wgconf,ANY-metho
setMethod('parsUpsize'
          ,'Wgconf'
          ,function(.Object){
             nameObject <- deparse(substitute(.Object))
             .Object <- parsModsize(.Object, opup=T)
             assign(nameObject,.Object,envir=parent.frame())
            return( invisible())
          })
#' @docType methods
setGeneric(name='parsDnsize'
           ,function(.Object){standardGeneric('parsDnsize')})
#' @aliases parsDnsize,Wgconf,ANY-metho
setMethod('parsDnsize'
          ,'Wgconf'
          ,function(.Object){
             nameObject <- deparse(substitute(.Object))
             .Object <- parsModsize(.Object, opup=F)
             assign(nameObject,.Object,envir=parent.frame())
            return(invisible())
          })
#' @docType methods
setGeneric(name='parsModsize'
           ,function(.Object,opup=T){standardGeneric('parsModsize')})
#' @aliases parsModsize,Wgconf,ANY-metho
setMethod('parsModsize'
          ,'Wgconf'
          ,function(.Object,opup=T){
             multf <-  1
             if (!opup) { multf <- -1 }
             .Object@wykcex <- .Object@wykcex  + multf * .Object@wykcexdt
             .Object@wykoff <-  .Object@wykoff + multf * .Object@wykoffdt
             ## lvl level params
             .Object@lvlcex <- .Object@lvlcex  + multf * .Object@lvlcexdt
             .Object@lvloff <-  .Object@lvloff + multf * .Object@lvloffdt

             ## trl trend params
            return ( .Object )
          })

#' @docType methods
setGeneric( name='getWykgparams'
           ,function(object,tf){standardGeneric("getWykgparams")})
#' @aliases getWykgparams,Wgconf,ANY-metho
setMethod('getWykgparams', 'Wgconf'
          ,function(object,tf){
          ##validation
            if ( (  tf != 'w1'
               && tf != 'd1'
               && tf != 'h1'
                )){
               stop(paste("[Wgconf:getWykgparams validation]"
                         ," bad value of tf param"))
            }
            par <- list()
            wupcol <- NA
            wupcol[ tf %in% 'w1' ] <- object@wykupcolw1
            wupcol[ tf %in% 'd1' ] <- object@wykupcold1
            wupcol[ tf %in% 'h1' ] <- object@wykupcolh1

			wdncol <- NA
			wdncol[ tf %in% 'w1' ] <- object@wykdncolw1
			wdncol[ tf %in% 'd1' ] <- object@wykdncold1
			wdncol[ tf %in% 'h1' ] <- object@wykdncolh1
			
			par$upcolor <- wupcol
			par$dncolor <- wdncol
			
            par$cex <- object@wykcex
            par$offs <- object@wykoff
            par$shape <- object@wykshap
            return(par)
           })
#' @docType methods
setGeneric( name='getLvlgparams'
           ,function(object,tf){standardGeneric("getLvlgparams")})
#' @aliases getLvlgparams,Wgconf,ANY-metho
setMethod('getLvlgparams', 'Wgconf'
          ,function(object,tf){
          ##validation
            if(    tf != 'w1'
                && tf != 'd1'
                && tf != 'h1'
                ){
               stop(paste("[Wgconf:getLvlgparams validation]"
                         ," bad value of tf param"))
            }
            par <- list()
            lcol <- NA
            lcol[ tf %in% 'w1' ] <- object@lvlcolw1
            lcol[ tf %in% 'd1' ] <- object@lvlcold1
            lcol[ tf %in% 'h1' ] <- object@lvlcolh1

            icol <- NA
            icol[ tf %in% 'w1' ] <- object@iclcolw1
            icol[ tf %in% 'd1' ] <- object@iclcold1
            icol[ tf %in% 'h1' ] <- object@iclcolh1
            hcol <- NA
            hcol[ tf %in% 'w1' ] <- object@hflcolw1
            hcol[ tf %in% 'd1' ] <- object@hflcold1
            hcol[ tf %in% 'h1' ] <- object@hflcolh1

            par$lvcolor <- lcol
            par$iccolor <- icol
            par$hfcolor <- hcol
            par$cex <- object@lvlcex
            par$offs <- object@lvloff
            return(par)
           })
#' @docType methods
setGeneric( name='getTrlgparams'
           ,function(object,tf){standardGeneric("getTrlgparams")})
#' @aliases getTrlgparams,Wgconf,ANY-metho
setMethod('getTrlgparams', 'Wgconf'
          ,function(object,tf){
          ##validation
            if (  tf != 'w1'
               && tf != 'd1'
               && tf != 'h1'
                ){
               stop(paste("[Wgconf:getTrlgparams validation]"
                         ," bad value of tf param"))
            }
            par <- list()
            col <- NA
            col[ tf %in% 'w1' ] <- object@trlcolw1
            col[ tf %in% 'd1' ] <- object@trlcold1
            col[ tf %in% 'h1' ] <- object@trlcolh1

            par$color <- col
            return(par)
           })


###helper methods
setMethod ('show'
           ,'Wgconf'
           , function(object){
              cat("*** Class Wgconf,*** \n")
              cat("* Graph params:\n")
			  
	      cat("Theme colors:\n")
	      cat("theme: ", object@thm.col, "\n")
	      cat("bartype: ", object@thm.bartype, "\n")
			  
              cat("Wyckoff facts:\n")
              cat("Up Colors  w1, d1, h1:")
              cat(object@wykupcolw1, object@wykupcold1,object@wykupcolh1, "\n")
			  cat("Down Colors  w1, d1, h1:")
			  cat(object@wykdncolw1, object@wykdncold1,object@wykdncolh1, "\n")
			  p <- getWykgparams(object, 'd1')
              cat("Wfact shape:", p$shape, "cex:", p$cex, "offset:",p$offs, "\n")

              cat("Level facts:\n")
              cat("Colors LVL w1, d1, h1:")
              cat(object@lvlcolw1, object@lvlcold1,object@lvlcolh1, "\n")
              cat("Colors HFL w1, d1, h1:")
              cat(object@hflcolw1, object@hflcold1,object@hflcolh1, "\n")
              cat("Colors ICL w1, d1, h1:")
              cat(object@iclcolw1, object@iclcold1,object@iclcolh1, "\n")
              p <- getLvlgparams(object, 'd1')
              cat("Level:", "cex:", p$cex, "offset:",p$offs, "\n")

              cat("Trend facts:\n")
              cat("Colors  w1, d1, h1:")
              cat(object@trlcolw1, object@trlcold1,object@trlcolh1, "\n")

            })



setGeneric( name='is.Wgconf',function(object){standardGeneric("is.Wgconf")})
setMethod('is.Wgconf'
          ,'Wgconf'
          ,function(object){
            return ( is(object, 'Wgconf') )
           })

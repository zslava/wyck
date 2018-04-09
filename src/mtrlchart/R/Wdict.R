#' Wdict type
#'
#' Simple container  to hold wyckoff dictionary
#'
#'
#'
#'
#' @keywords internal
#' @rdname Wdict-class
#' @name Wdict
setClass(Class='Wdict'
         , representation(wels='matrix'
                         ,types='matrix'
                         ,monthdict='character'
                         )
        ,prototype(wels=matrix(ncol=3, byrow=TRUE
                               ,data=c('SOS' , 'Sign of Strength'       , 1
                                      ,'SOW' , 'Sign of Weakness'       ,-1
                                      ,'SC'  , 'Selling Climax'         ,1
									  ,'TSC'  ,'Secondary Test of Selling Climax' ,1
	                                  ,'BC'  , 'Buying Climax'          ,-1
									  ,'TBC'  ,'Secondary Test of Bying Climax'   ,-1
	                                  ,'ARN'  , 'Automatic Reaction'    ,1
                                      ,'AR'  , 'Automatic Rally'        ,-1
                                      ,'NR'  , 'Normal Reaction'        ,1
                                      ,'SR'  , 'Sluggish Rally'         ,-1
                                      ,'SA'  , 'Stopping action'        ,1
									  ,'TSA'  , 'Secondary test of Stopping action' ,1
	                                  ,'UT'  , 'Upthrust'                ,-1
									  ,'TUT'  , 'Secondary test of Upthrust' ,-1
	                                  ,'SOT' , 'Shortening of Thrust'    ,-1
                                      ,'SHK' , 'Shake Out'               ,1
                                      ,'PS'  , 'Preliminary Support'     ,1
                                      ,'PSY' , 'Preliminary Supply'      ,-1
                                      ,'LPS' , 'Last point of Support'   ,-1
                                      ,'LPSY', 'Last point of Supply'    ,1
                                      ,'JOC' , 'Jump Over Creek'         ,1
                                      ,'BTC' , 'Back to Creek'           ,1
									  ,'TBTC' , 'Secondary test of Back to Creek' ,1
	                                  ,'BUI' , 'Break Under Ice'         ,-1
                                      ,'BTI' , 'Back to Ice'             ,-1
									  ,'TBTI' , 'Secondary test of Back to Ice'  ,-1
	                                  ,'SPR' , 'Spring'                  ,1
                                      ,'TSPR'  , 'Secondary Test of Spring' ,1
                                      ) 
                               )
                    ,types=matrix(ncol=2,byrow=TRUE
                                 ,data=c('WYF' , 'Wyckoff fact'
                                        ,'TRL' , 'Trend line'
                                        ,'LVL' , 'Creek Level line'
                                        ,'ICL' , 'ICE Level line'
                                        ,'HFL' , 'Half Level line'
										,'BVW' , 'Bar View fact'
										,'BPR' , 'Bar properties fact'
										,'HYP' , 'Hypo fact'
										,'NOB' , 'Note fact'
                                        )
                              )
                    ,monthdict=c('F','G','H', 'J','K','M','N','Q','U','V','X', 'Z')
                    )
         )
#user frendly function for constructor
##helper function  for construction
#' Initialized Wdict
#'
#' Constructor function to create new instance of Wgconf
#'
#' @return  Wdict instance
#' @keywords internal
#' @examples
#' \dontrun{wd <- wDict()}
#' @export
wDict <- function(){
  o <- new('Wdict')
  return(o)
}

##### utility methods

setGeneric( name='contains'
           ,function(object,value){standardGeneric("contains")})
setMethod('contains', 'Wdict'
          ,function(object,value){
            #validation
            if ( !is.character(value) )   {
                      stop(paste("[Wdict:contains validation]"
                                ," value should be character"))
               }
                res <- FALSE
                m <- match(value, object@wels)
                if (!is.na(m)) { res <- TRUE }
               return(res)
           }
         )
setGeneric( name='getWykLongname'
           ,function(object,value){standardGeneric("getWykLongname")})
setMethod('getWykLongname', 'Wdict'
          ,function(object,value){
            #validation
            if ( !is.character(value) )   {
                      stop(paste("[Wdict:getWykLongname validation]"
                                ," value should be character"))
               }
                lnm <- as.character(NA)
                if ( contains(object, value)) {
                   m <- match(value, object@wels[,1])
                   lnm <- object@wels[m,2]
                 }
               return(lnm)
          }
         )
setGeneric( name='getWykside'
				 ,function(object){standardGeneric("getWykside")})
setMethod('getWykside', 'Wdict'
				 ,function(object){
					 return(object@wels[,-2])
				 })
		 
setGeneric( name='getWyksentiment'
           ,function(object,value){standardGeneric("getWyksentiment")})
setMethod('getWyksentiment', 'Wdict'
          ,function(object,value){
            #validation
            if ( !is.character(value) )   {
                      stop(paste("[Wdict:getWyksentiment validation]"
                                ," value should be character"))
               }
                sentiment<-function(cd){ switch(cd, "1" ="bullish","-1" = "bearish","0" ="neutral")}
                sent <- as.character(NA)
                if ( contains(object, value)) {
                   m <- match(value, object@wels[,1])
                   sid <- object@wels[m,3]
                   sent <- sentiment(sid)
                 }
               return(sent)
          }
         )

setMethod ('show'
           ,'Wdict'
           , function(object){
              cat("***Class Wdict. Dictionary of Wyckoff labels:\n")
              cat("**List of Label types:\n")
              print( object@types)
              cat("**List of Wyckoff labels:\n")
              print(object@wels)
              cat("**List of Feautures contract months:\n")
              print(object@monthdict)
           })

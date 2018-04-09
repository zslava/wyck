#' Add vertical separation lines on a chart according to period
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export 
`addEp` <- function(cep=T) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)


   ### set param for endpoints
#   epbrd <- list('years', 1) #default value
#   p <- periodicity(x)
#   pf <-p$frequency
#   pu <-p$units
#   if (pf > 0 && pf < 5  && pu == "mins" ) { epbrd <- list('minutes', 15) }
#   if (pf >= 5 && pf <= 15 && pu == "mins" ) { epbrd <- list('hours', 1) }
#   if (pf > 15  && pf <= 30 && pu=="mins" ) { epbrd <- list('hours', 2) }
#   if (pf >= 3600 && pf < 86400 && pu=="hours") { epbrd <- list('days', 1) }
#   if (pf >= 86400 && pf < 604800 && pu=="days") { epbrd <- list('weeks', 1) }
#
#   if (pf >= 604800 && pf < 2592000 && pu=="days") { epbrd <- list('months', 1) }
#   if (pf >= 2592000 && pf < 31536000 && pu=="days") { epbrd <- list('years', 1) }

  chobTA <- new("chobTA")
  chobTA@new <- FALSE
  chobTA@TA.values <-x[lchob@xsubset,]

  chobTA@name <- "chartEp"
  chobTA@call <- match.call()
  chobTA@on <- 1 #!NB
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale
					   ,cep=cep
                       #,epbrd=epbrd
                        )

  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)

    chartSeries.chob <- quantmod:::chartSeries.chob
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  }
} #}}}
#' @export 
#' @keywords internal
`chartEp` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

	cep <- x@params$cep
	#epbrd <- x@params$epbrd

    y <- x@TA.values

    if(cep && x@on[1] > 0) {
		epbrd <- list('years', 1) #default value
		p <- periodicity(y)
		pf <-p$frequency
		pu <-p$units
		if (pf > 0 && pf < 5  && pu == "mins" ) { epbrd <- list('minutes', 30) }
		if (pf >= 5 && pf <= 15 && pu == "mins" ) { epbrd <- list('hours', 1) }
		if (pf > 15  && pf <= 30 && pu=="mins" ) { epbrd <- list('hours', 2) }
		if (pf >= 3600 && pf < 86400 && pu=="hours") { epbrd <- list('days', 1) }
		if (pf >= 86400 && pf < 604800 && pu=="days") { epbrd <- list('weeks', 1) }
		
		if (pf >= 604800 && pf < 2592000 && pu=="days") { epbrd <- list('months', 1) }
		if (pf >= 2592000 && pf < 31536000 && pu=="days") { epbrd <- list('years', 1) }
		
      epvl <- endpoints(y, on=epbrd[[1]], k=epbrd[[2]])
      vl <- epvl*spacing
      ##get rid of first and last vbord
      vl <-vl[-1]
      vl <-vl[-length(vl)]
      abline(v=vl,col=x@params$colors$grid.col, lty="dotted")

    }

    legend("bottomleft"
          ,legend=c( paste("Epoints:",epbrd[[1]])
                   )
                  ,text.col=x@params$colors$fg.col
                  ,bty="n"
                  ,y.inter=0.95)


} # }}}

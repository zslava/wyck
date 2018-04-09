
#' Add filled & working orders on the chart
#'
#' version for oldstyle quantmod:::charSeries
#'
#' @keywords internal
#' @export 
`addOrderfact` <- function(ords,lcex=0.5, poff=2, theme="white", txtlbl=FALSE) {
  ## lbltype values dt or ab
  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)   #lchob@xdata is xts object

  chobTA <- new("chobTA")
  chobTA@new <- FALSE
  chobTA@TA.values <-x[lchob@xsubset,]

  curdata <- lchob@xdata[lchob@xsubset]
  xsubset <- lchob@xsubset


  chobTA@name <- "chartOrderfact"
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
                       ,xsubset=lchob@xsubset
                       ,curdata=curdata
                       ,ords=ords
			                 ,lcex=lcex
                       ,poff=poff
                       ,theme=theme
                       ,txtlbl=txtlbl
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
}#}}}
#' @export 
#' @keywords internal
`chartOrderfact` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width


    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)
    xsubset <- x@params$xsubset
    curdata <- x@params$curdata
    lcex <-   x@params$lcex
    poff <-   x@params$poff
    ords    <- x@params$ords
    theme   <- x@params$theme
    txtlbl   <- x@params$txtlbl

    y <- x@params$curdata ##xts with current data
    x.pos <- 1 + spacing * (1:length(y) - 1)  
    x.pos <- x.pos  + poff # move points outside a bar by offset

   
   # # plot filled up orders
   # pords_fu <- ords [ ords$status == 'f'
   #                & ords$qty  > 0,    ]
   # plotFilledorders(pords_fu, x.pos, y, lcex=lcex, dir=1,theme=theme)    
   # #plot filled dn orders           
   # pords_fd <- ords [ ords$status == 'f'
   #                & ords$qty  < 0,    ]
   # plotFilledorders(pords_fd, x.pos, y, lcex=lcex, dir=-1, theme=theme)                

   # #plot working dn orders
   # pords_w <- ords [ ords$status == 'w' , ]
   # plotWorkingorders(pords_w, x.pos, y, lcex=lcex, theme=theme ) 

    xtsdt<-index(y)
    if ( periodicity(xtsdt)$frequency %in% c(86400,604800) && 
         base::format( last(index(y)), format='%H:%M:%S' ) == "00:00:00"  ){
          xtsdt <- xtsdt + (86400 - 60) #add 23h:59min  if index(y) is in Date() format
    }
    tz<-indexTZ(y)
    cols <- setcolors(theme)

    for(j in 1:nrow(ords)){
      cord <- ords[j,]
      
      if ( cord$status == 'f') { plotFilledOrder(ordrec=cord, xvec=x.pos,xtsdates=xtsdt, tz=tz
                                ,lcex=lcex, colup=cols$up, coldn=cols$down, coltxt=cols$text, hasTxtlbl=txtlbl) } 

      if ( cord$status == 'w') { plotWorkingOrder(ordrec=cord, xvec=x.pos, xtsdates=xtsdt, tz=tz
                                ,lcex=lcex,colup=cols$up, coldn=cols$down, coltxt=cols$text) } 

      #browser()
    }

 return(invisible())
}##eofunc

##internal functions

plotFilledOrder <- function(ordrec, xvec, xtsdates, tz, lcex, colup, coldn,coltxt, hasTxtlbl){

    ycrd <- ordrec$fprice

    ord_dt <- as.POSIXct(ordrec$chgdate, tz=tz, origin='1970-01-01')
    ix <- match(FALSE, ord_dt > xtsdates)
    xcrd <- xvec[ix]

    dir <- sign(ordrec$qty)
    if(dir == 1)
       points(xcrd, ycrd, pch=24, cex=lcex, col=colup, bg=colup)
    else if(dir == -1)  
       points(xcrd, ycrd, pch=25, cex=lcex, col=coldn, bg=coldn) 

    tlbl <- paste(ordrec$qty,'@',formatC(ordrec$fprice, digits=4, format="f"),sep="")
    if(hasTxtlbl) { text( x=xcrd, y=ycrd, tlbl, pos=4, offset=0.3, col=coltxt, cex=lcex ) }
  
 return(invisible())
}

plotWorkingOrder <- function(ordrec, xvec, xtsdates, tz, lcex, colup, coldn,coltxt){
    ycrd <- rep(ordrec$rprice, 2 )

    ord_dt <- as.POSIXct(ordrec$chgdate, tz=tz, origin='1970-01-01')
    ix <- match(FALSE, ord_dt > xtsdates)
    xcrd_l <- xvec[ix]
    xcrd <- c( xcrd_l, last(xvec) )
     dir <- sign(ordrec$qty)
     if(dir == 1){
        ocol=colup
        cpos="3"
      }else{
        ocol=coldn
        cpos="1"
      }
    lines(x=xcrd, y=ycrd, col=ocol, lty="dashed")
    text( x=xcrd[1], y=ycrd[1], ordrec$qty, pos=cpos, offset=0.1, col=coltxt, cex=lcex )
    return(invisible())
}

setcolors <-function(theme){
    defaultup <- "darkgreen"
    defaultdn <- "darkred"
    defaulttxt <- "black" 
    if (theme =='white'){
      colup <- defaultup
      coldn <- defaultdn
      coltxt <- defaulttxt
    }else if( theme =='black'){
      colup <- "white" #seagreen
      coldn <-"deeppink"
      coltxt <- "white"
    }
    return ( list(up=colup, down=coldn, text=coltxt))
}



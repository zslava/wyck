#require(xts)
#require(quantmod)

##data download methods
#' @export
tbloxListHistDaily <-function(symbols,unadjust=T,verbose=T
                             ,urldfname="http://www.tradingblox.com/Data/DataOnly.zip"
                             ,urldefname="http://www.tradingblox.com/tradingblox/CSIUA/FuturesInfo.txt"
                             ,localdefname="~/Dropbox/cdat/tblox/FuturesInfo_tblox_szi.csv"
                             ,uselocaldef=T)
 {
    osname <- .Platform$OS.type
    if (osname == "unix"){
      localfname <-"/tmp/DataOnly.zip"
    }
    else if (osname == "windows"){
       localfname <- "C:/DataOnly.zip"
    }

    if (uselocaldef){
       urldefname <- localdefname
    }
    
    ddata <- list()
    tmpdir <- tempdir()
    tblox.tmp <- paste(tmpdir, "tblox", sep="/")
    if(!file.exists(tblox.tmp)) { dir.create(tblox.tmp) }
    tmp <- tempfile()
    fmt='%Y-%m-%d'
    lmodday <- as.POSIXct( strftime(file.info(localfname)$mtime ,format=fmt))
    today <- as.POSIXct( strftime(Sys.time(), format=fmt))
    #browser()
    if ( !is.na(lmodday) &&  lmodday==today ){
      print(paste("copying tblox data from downloaded local source.."))
      file.copy(from=localfname, to=tmp )
    }else{
      print(paste("downlading tblox data.."))
      download.file(urldfname, localfname)
      file.copy(from=localfname, to=tmp )      
    }

    unzip(tmp,exdir=tblox.tmp)
    
    def <- read.csv(urldefname,skip=1,header=FALSE)
    allSymb <- as.character(def[,1])
   if ( missing(symbols) || is.na(symbols) || symbols == "all" || symbols == ""){
     symbols <- allSymb
   }
   badsym <- c()
   for (i in match(symbols, allSymb)){
      if (file.exists(paste(tblox.tmp,'Futures',def[i,4],sep='/'))) {
      dat <-read.csv(paste(tblox.tmp,"/Futures/",def[i,4],sep=""),header=FALSE)
      if (verbose)  {cat("done loading ", as.character(def[i,4]), ".....\n")}
       x <- xts(dat[,2:9],order.by=as.POSIXct(paste(dat[,1]),format="%Y%m%d"))
       cn <- c(paste('Adj',c("Open","High","Low","Close"),sep="."),'Volume','OpInt','ExpMth','Unadj.Close')
       colnames(x) <- paste(def[i,1],cn,sep=".")
       if (unadjust ) {
         x <- tbloxUnadjustOhlc(x)
       }
       cinst <- list(name=as.character(def[i,1]), data=x)
       ddata[[length(ddata)+1]] <- cinst
      } else { badsym <- c(badsym, paste(def[i,1])) }
  }
    unlink(tmp)
    unlink(tblox.tmp, recursive=TRUE)
    
    if ( verbose && length(badsym) > 0) { print("symbols with failed download:") ; print(badsym) }
    #browser()
   return(ddata)       
 }

#' @export
tbloxUnadjustOhlc<- function(x){
  ### validation
  if (!is.xts(x)){
             stop(paste("[tbloxUnadjustdata validation]"
                        ,"input param is not xts object"))
  }            
  if (ncol(x)!=8){
             stop(paste("[tbloxUnadjustdata validation]"
                        ,"input xts does not have 8 columns."
                        ,"please check if it comes from tblox"))
  }            
    
  diff <- as.numeric(x[,8] - x[,4])
  ux <- cbind ( x[,1] + diff, x[,2] + diff, x[,3] + diff, x[,4] + diff, x[,5])
  opcolstr <- colnames(x)[1]
  colsplit <- strsplit(opcolstr, "\\.")
  ipref <- first(colsplit[[1]])
  ncolnames <- c('Open', 'High', 'Low','Close','Volume')                      
  ncolnames <- paste(ipref,ncolnames,sep='.')
  colnames(ux) <- ncolnames
  return(ux)
}


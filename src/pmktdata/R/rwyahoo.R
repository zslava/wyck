#require(xts)
#require(quantmod)

##data download methods
#' Download yahoo daily market data

#' Downloads daily markt data from http://finance.yahoo.com. 
#' Adjusts OHLC data for equity data  (param itype = "eq")
#' @param  ticker  character yahoo ticker
#' @param  fromd  character  download date  start in a format "%Y-%m-%d" 
#' @param  tod  character  download date end in a format "%Y%-%m-%d"
#' @param  itype  character  if \code{itype}== "eq"} Data will be adjusted 
#' for dividents, splits with a call to \code{adjustOHLC}        
#' @return  xts object with daily data 
#' @export
#' @examples  dldxts <- yahooHistDaily("GE", fromd='2005-01-01', tod='strftime(Sys.time(), format="%Y-%m-%d)"', itype="eq")
yahooHistDaily <-function( ticker, fromd, tod, itype,tz)
 {
       if(missing(tz)){ tz <- "UTC"}
       fromdstr <- as.POSIXct(fromd, format="%Y-%m-%d")
       todstr <- as.POSIXct(tod, format="%Y-%m-%d")
       mserie <- c()
       mserie <- getSymbols(ticker, auto.assign=F, src='yahoo', from=fromdstr, to=todstr, index.class="POSIXct",tz=tz)
      #mserie <- getSymbols(ticker, auto.assign=F, src='yahoo', from=fromdstr, to=todstr)
       if (nrow(mserie) > 0  && itype=='eq') {
           mserie <- adjustOHLC(mserie, symbol.name=ticker ) # adjusts for splits and dividends
       }
       #fmt <- "%Y-%m-%d"
       #index(mserie) <- as.POSIXct( strftime( index(mserie) ,format=fmt))

     return(mserie)
 }

#' check if last date of xts from yahoo has the same date

#' Chose one quote where volume is higher
#' @export
checkYahooDblquoteDaily <-function( xdata){
  ##get last 2 rows
    tldat <- last(xdata,2)
    txdat <- xdata[1:(nrow(xdata)-2)]
    ##double quote detection
    if ( index(tldat[1]) == index(tldat[2] ) ){
        ##chose quote  with higher volume
        print(paste("Warning. Double quote detected. Purging quote with lower volume"))
        print(tldat)
        maxvol <- as.numeric( max(Vo(tldat)))
        qid <- match(maxvol, Vo(tldat))
        cquot <- tldat[qid]
        xdata <- rbind(txdat, cquot ) ## append quote with higher volume
    }
    return(xdata)
}

#' Failsafe download yahoo daily market data

#' Number of download retries is managed with a param \code{maxfailed}
#' @aliases yahooHistDaily
#' @export
 yahooHistDailyFS <- function( ticker, fromd, tod, itype, maxfailed=5,tz="UTC",verbose=T)
{
     gotInterrupt <- FALSE
     for (kk in 1:maxfailed)
     {
       tryCatch({
        if (gotInterrupt) { break }
        cxts <- yahooHistDaily(ticker, fromd, tod, itype,tz=tz) #call to donwload data
        aValue <- length(cxts)
        if (aValue > 0 ){
          if (verbose) {
            print(paste("downloaded daily bar serie",ticker, nrow(cxts), "elements"
                        ,"from", index(first(cxts)), "to", index(last(cxts)) ))
          }
         break
      }
   }, interrupt = function(ex) {
       cat("An interrupt was detected.\n");
       gotInterrupt <- TRUE
       print(ex);
   }, error = function(ex) {
       cat("An error was detected.\n");
       print(ex);
       cat(paste('\n number of failed  downloads', kk, '\n' ))
       Sys.sleep(0.5) ## sleep 0.5 seconds in case of error before next download attempt
   }, finally = {
      #cat("Releasing resources...");
   }) # tryCatch()
  } ## for maxfailed loop
  return(cxts)
}
        

#' @export
`pmktdatadoc` <-
function ()
{
cat("=== pmktdata package: Availaible documentation guides ===\n")
cat("  mktdataguide() ::\t Load market data guide\n")

cat("=== functions quickhelp listings ===\n")
cat('o <- new("ContractSeq"\n')
cat('cshelp(o) ::\t contract sequence object operations help\n')

}


#' @export
`mktdataguide` <-
function ()
{
	cat("#####################################################################\n")
    cat("Information on market data storage location and  operation workflows\n ")
	cat("#####################################################################\n")

    cat("\n##########Table of contents:############# \n")
    cat("\nPaths and description for market data\n")
    cat("\nReference data for contract expiry\n")
    cat("\nOperations:\n")
    cat("\nUpdating reference expiry data\n")
    cat("\nReading market data for a contracta sequence\n")
    cat("\nAdding new future contracts to a contract sequence\n")
    cat("\nFinding, adding roll dates in  contracts\n")
    cat("\nDaily update download of market data for one or many instruments\n")
    cat("\nRoll date update near an expiry of a contract. Check of liquidity\n")
    cat("\nSetting up a new instrument and start downloading market data for it \n")
    cat("\n###########Table of contents end:######### \n")


    cat("\n***Section Paths and description for market data****\n\n")

	cat("watchlist Rdat files from mtrlchart package are in \n" )
    cat("/Users/Zimine/googledrive/cdat/wls\n")
    cat("A usual daily analysis starts by loading a watchlist from this folder\n")
    cat("and subsequently updating  the market data for instruments present in the list\n")
    cat("\n")
    cat("For futures instruments we first execute an update of market data of all followed instruments.\n")
    cat("Then we load a specific watchlist object from a wathlist folder \n")
    cat("   and attach the updated market data to the list.\n")

    cat("\nDTN provider futures ohlc data are stored in csv files plus rdat files under top folder:\n")
    cat("/Users/Zimine/googledrive/mkdata/dtn/contrseq/\n")

    cat("\nInstrument data is organized under a folder named after a instrument ticker.\n")
    cat("The structure of the folder:\n")
    cat("meta - contains a  tkr_meta.csv file with contract end dates, roll dates. \n")
    cat("\t (we can modify update  this file manually  to set a roll date near contract expiry )\n")
    cat("contracts contains csv market data in subfolders per calendar contract\n")
    cat("log contains a log file on updates  operations\n")
    cat("rdat contains an R rdat file with market data per time frame\n")
    cat("cont  contains a tkr_cont.csv  continues serie market data per time frame \n")

    cat("\n**** Section Reference info on futures contracts expiries ******\n\n")

    cat("Futures contract expiries dates are stored and accessed from google spreadsheets\n")
    cat("/Users/Zimine/googledrive/findata/expiries\n")
    cat("File /Users/Zimine/googledrive/findata/expiries/_expiry_keys with\n")
    cat("uid key=0AuWXa53znQ3YdHU0QWFqNFFZTmhuUzdPdGctMk90aVE  stores keys for contract expriries\n")
    cat("and is used in ContractSeq fillExperies function.\n")

    cat("\n**** Section Operation: Updating reference expiry data ******\n\n")

    cat("Using exhange websites (cmegroup.com theice.com .. update for a next year\n")
    cat("google spreadsheets in folder /Users/Zimine/googledrive/findata/expiries .\n")
    cat("Repeat for each instrument:\n")
    cat("\t cs <- readcs(ticker,topstoredir)\n")
    cat("\t addFutureContracts(cs, ctime, nmonths=6) to add 6 future contracts to a sequence\n")
    cat("\t fillExpiries(cs,persist=T,defaul-tkkeys) this reads from google spreadsheets expiries\n")
    cat("\t\t and stores them in the meta data csv file under  topstoredir file structure.\n")

    cat("\n***** Reading & downloading market data for a contracta sequence ******\n\n")

    cat("cs<-readcs(iname,topstoredir_default) : load existing instrument sequence.\n\n")
    cat("chkMeta(cs) checks a synchronization between contract records in a sequence object\n")
    cat("and an instrument meta csv file in meta subfolder under topstoredir_default.\n\n")

    cat("updateEOD(cs,stime=Sys.time(),ctime=Sys.time(),updMeta=F,persist=T) : download EOD data\n")
    cat("in unexpired contracts up to date. If stime is specified, eod data is downloaded started from \n")
    cat("the specified contract.\n")

    cat("\n******* Section Operation: Adding new future contracts to a  sequence ********\n\n")
    
    cat("A futures countract sequence at any time must have defined at least two \n")
    cat("next (future) contracts.\n")
    cat("addFutureContracts(cs,  nmonths=6) will add to a sequence relevant contract records\n")
    cat("for a time period of next 6 months.\n")
    cat("Use fillExpiries(cs,persist=T,kkeys_default) to fill expiries in a contract sequence\n")
    cat("from google spreadsheets.\n")

    cat("\n******** Finding, adding roll dates in  contracts *******\n\n")

 

    cat("tbdone partialdownload()\n")



}

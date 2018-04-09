#######################################
#
# analysis functions
#
#######################################


 #' @export
 #' @docType methods
setGeneric(name='printDonchian',function(.Object){standardGeneric('printDonchian')})
 #' print donchian veekly, monthly values
 #' 
 #' @aliases printDonchian,Wwatchlist,ANY-method
setMethod('printDonchian','Wwatchlist'
          ,function(.Object){

  ctf <- getCurtimeframe(.Object)
  mldl <- .Object@mdata
  iname<-getCinstrument(mldl)
  cdata <- getCdatedata(mldl, iname, ctf)

  ##compute subset string
  fmt="%Y-%m-%d"
  cdate <- getCdate(mldl)
  subrdate <- cdate 
  subldate <- cdate  -  62 * 86400 # two last months
  csubs=paste( strftime(subldate, format=fmt),'::', strftime(subrdate, format=fmt),sep="")

  if(ctf == 'd1'){ #calc params for daily data
    dchw <- DonchianG(cdata[csubs], gperiod="weeks",offset=0.25 , dlag=F) 
    dchmo <- DonchianG(cdata[csubs], gperiod="months",offset=0.25, dlag=F)
    #browser()
    #current bar close, hi, lo
    cCl <- as.numeric( Cl(last(cdata[csubs])) )
    cLo <- as.numeric( Lo(last(cdata[csubs])) )
    cHi <- as.numeric( Hi(last(cdata[csubs])) )
    
    wtop <- last(dchw)$dtop
    wbot <- last(dchw)$dbot
    wtbrd <- last(dchw)$dtbord
    wbbrd <- last(dchw)$dbbord

    motop <- last(dchw)$dtop
    mobot <- last(dchw)$dbot
    motbrd <- last(dchw)$dtbord
    mobbrd <- last(dchw)$dbbord

    cClpz <- (cCl - cLo) / (cHi - cLo) ## position of the close in the bar range
    #previous bar close
    #pCl <-  as.numeric(Cl(last(cdata[csubs],2)[1]))
    
    clposw <- ( cCl - last(dchw)$dbot ) / (last(dchw)$dtop - last(dchw)$dbot)
    clposmo <- ( cCl - last(dchmo)$dbot ) / (last(dchmo)$dtop - last(dchmo)$dbot)
    
   


    pwtop <- last(dchw,2)[1]$dtop
    pwbot <- last(dchw,2)[1]$dbot
    pmotop <- last(dchmo,2)[1]$dtop
    pmobot <- last(dchmo,2)[1]$dbot     
    pclposw <- (cCl - pwbot) / (pwtop - pwbot)
    pclposmo <- (cCl - pmobot) / (pmotop - pmobot)
    ##weekly pos for low high
    ploposw <- (cLo - pwbot) / (pwtop - pwbot)
    phiposw <- (cHi - pwbot) / (pwtop - pwbot)
    #monthly pos for low high
    ploposmo <- (cLo- pmobot) / (pmotop - pmobot)
    phiposmo <- (cHi- pmobot) / (pmotop - pmobot)

    #weekly , monthly absolute price ranges
    wrng     <- last(dchw)$dtop - last(dchw)$dbot 
    wrngpct  <- wrng / cCl * 100
    morng    <- last(dchmo)$dtop - last(dchmo)$dbot 
    morngpct <- morng / cCl * 100

    
    wmsg <-  paste("weekly:   ", "| b=",last(dchw)$dbot, " bb=",last(dchw)$dbbord, "  | m=",last(dchw)$mid
                           , " | bt=",last(dchw)$dtbord, " t=",last(dchw)$dtop
                           , " | wrng=", wrng, " [",formatC(wrngpct,digits=2),"% ] "   
                           , " | cl=",Cl(last(cdata[csubs])), " w clpos@" ,formatC(clposw,digits=2) , sep="")

    momsg <- paste("monthly:  ", "| b=",last(dchmo)$dbot, " bb=",last(dchmo)$dbbord, " | m=",last(dchmo)$mid
                           , " | bt=",last(dchmo)$dtbord, " t=",last(dchmo)$dtop
                           , " | morng=", morng, " [",formatC(morngpct,digits=2),"% ]"    
                           , " | cl=",Cl(last(cdata[csubs])), " mo clpos@" ,formatC(clposmo,digits=2) , sep="")

    print(wmsg)
    print(momsg)
   
    #### check the same for closeclose, highhigh, lowlow => alerts on range formation
    #range info
    xd1 <- cdata[csubs] ## two months of daily data
    xd1rng <- Hi(xd1) - Lo(xd1)
    crng <- cHi - cLo
    qxd1rng <-  quantile(xd1rng, c(0.30, 0.5, 0.8, 0.9))

    bins <- 0.05*(1:19) # every 5 percent
    binsd1rng <- quantile(xd1rng, bins)
    idx <- which( (binsd1rng < crng) == FALSE )
    #3 cases
    if(length(idx) == 0){ # higher than highest bin
       hhitbin <- last(bins)
       crmsg <- paste(crng,">",hhitbin)
    }else{
       if( first(idx) == 1){  #lower than lowest bin
         lhitbin <- first(bins)
         crmsg <-paste(crng,"<",lhitbin)
       }else{              #somewhere between
         hhitbin <- bins[ first(idx) ]
         lhitbin <- bins[ first(idx) -1]
         crmsg <- paste(lhitbin,"<",crng,"<",hhitbin)
       }
    }
    
    rngmsg <- paste("range: ", "quantile (0.30, 0.5, 0.8, 0.9): [", paste(qxd1rng,collapse=" "),"]"
                   ," last= ", crng , " ", crmsg, sep="")
    print(rngmsg)

    #alert warnings
    walrts <- "weekly alerts:"
    cntw <- 0
    moalrts <- "monthly alerts:"
    cntmo <- 0
     
    rrside <- 0  ## to potentiall print  rr

    bthresh <- 0.29
    ##bottom zone
    if (clposw <= bthresh) { 
       cntw<-cntw +1 
       walrts <- paste(walrts, "closed in bottom weekly zone", " w pclpos@",formatC(clposw,digits=2),";",sep="")
       rrside <- 1
    }
    if (clposmo <= bthresh) {
       cntmo <- cntmo+1
       moalrts<- paste(moalrts, "closed in bottom monthly zone"," mo pclpos @", formatC(clposmo,digits=2),";",sep="")
       rrside <-1
    }


    ##breaking of donchian
    if (pclposw > 1){ 
      cntw <- cntw+1
      walrts <-paste(walrts, " broke up weekly channel ",pwtop, " w pclpos@", formatC(pclposw,digits=2),";",sep="")
      rrside <- 1
    } 
    if (pclposw < 0) {
      cntw <- cntw+1
      walrts <-paste(walrts, " broke down weekly channel ",pwbot," w pclpos@", formatC(pclposw,digits=2),";",sep="") 
      rrside <- -1
    } 
    if (pclposmo > 1) { 
      cntmo<- cntmo+1
      moalrts <-paste(moalrts, " broke up monthly channel ",pmotop," mo pclpos @", formatC(pclposmo,digits=2),";",sep="") 
    } 
    if (pclposmo < 0) {
     cntmo <- cntmo+1
    moalrts <-paste(moalrts, " broke down monthly channel ",pmobot," mo pclpos @", formatC(pclposmo,digits=2),";",sep="") 
    } 

    ##spring alerts
    sprltrsh <- -0.02
    sprctrsh <- -0.01


    if( ploposw < sprltrsh &&  ploposw < pclposw && pclposw > sprctrsh){
     cntw <- cntw +1 
      walrts <-paste(walrts, "potential weekly Spring ",pwbot, " w plopos@", formatC(ploposw,digits=2),";",sep="")
      rrside <- 1
    }
    if( ploposmo < sprltrsh &&  ploposmo < pclposmo && pclposmo > sprctrsh){
     cntmo <- cntmo +1 
     moalrts <-paste(moalrts, " potential monthly Spring ",pmobot," mo plopos @", formatC(ploposmo,digits=2),";",sep="") 
     rrside <- 1
    }   

    ##put monthly uptrhust alert 
    uthitrsh <- 1.01
    utctrsh <- 0.999
    clposthrsh <- 0.55
    if(phiposmo > uthitrsh &&  pclposmo < utctrsh && cClpz < clposthrsh) {
     cntmo <- cntmo + 1
     moalrts <-paste(moalrts, " potential monthly  Upthrust ",pmotop," mo phipos @", formatC(phiposmo,digits=2),";",sep="") 
     rrside <- -1

    }
    #print if alerts are raised
    hasAlerts <- F
    if(cntw > 0 || cntmo > 0) { hasAlerts <- T}
    if(hasAlerts){ print("#####################################################")}
    if( cntw >0  ) { print(walrts)}
    if( cntmo >0 ) { print(moalrts)}
    if(hasAlerts){ print("####################################################")}  

    if(rrside != 0) { printRR(.Object, side=rrside)}

  } #if on ctf='d1'
})
 #' print risk reward
 #' @export
 #' @docType methods
setGeneric(name='printRR',function(.Object,side=1){standardGeneric('printRR')})
 #' print risk reward for veekly, monthly donchian values
 #' 
 #' @aliases printRR,Wwatchlist,ANY-method
setMethod('printRR','Wwatchlist'
          ,function(.Object,side=1){

  ctf <- getCurtimeframe(.Object)
  mldl <- .Object@mdata
  iname<-getCinstrument(mldl)
  cdata <- getCdatedata(mldl, iname, ctf)
  d1data <- getCdatedata(mldl, iname, 'd1') 
  ##compute subset string
  fmt="%Y-%m-%d"
  cdate <- getCdate(mldl)
  subrdate <- cdate 
  subldate <- cdate  -  62 * 86400 # two last months
  csubs=paste( strftime(subldate, format=fmt),'::', strftime(subrdate, format=fmt),sep="")
  sd1dat <- d1data[csubs] 
    dchw <- DonchianG(sd1dat, gperiod="weeks",offset=0.25 , dlag=F) 
    dchmo <- DonchianG(sd1dat, gperiod="months",offset=0.25, dlag=F)
    #browser()
    #current bar close, hi, lo
    cCl <- as.numeric( Cl(last(cdata)) )
    cLo <- as.numeric( Lo(last(cdata)) )
    cHi <- as.numeric( Hi(last(cdata)) )
    
    wtop <- last(dchw)$dtop
    wbot <- last(dchw)$dbot
    wtbrd <- last(dchw)$dtbord
    wbbrd <- last(dchw)$dbbord

    motop <- last(dchmo)$dtop
    mobot <- last(dchmo)$dbot
    motbrd <- last(dchmo)$dtbord
    mobbrd <- last(dchmo)$dbbord
    if( side == 1 ) {  ## longs
      #long weekly rr for 0.75 and weekly top 
      wrr1 <- (wtbrd - cHi)  / (cHi - cLo)
      wrr2 <- (wtop - cHi)  / (cHi - cLo)
      west1 <- c(cHi, cLo, wtbrd)
      west2 <- c(cHi, cLo, wtop)
      #long monthly rr for 0.75 and weekly top ## stops are near weekly lows or highs
      morr1 <- (motbrd - cHi)  / (cHi - cLo)
      morr2 <- (motop - cHi)  / (cHi - cLo)
      moest1 <- c(cHi, cLo,motbrd)
      moest2 <-  c(cHi, cLo, motop)
   }else{  # shorts
      #short weekly rr for 0.75 and weekly bot
      wrr1 <- (cLo - wbbrd)  / (cHi - cLo)
      wrr2 <- (cLo- wbot)  / (cHi - cLo)
      west1 <- c(cLo, cHi, wbbrd)
      west2 <- c(cLo, cHi, wbot)
      #short monthly rr for 0.75 and weekly top
      morr1 <- (cLo - mobbrd)  / (cHi - cLo)
      morr2 <- (cLo- mobot)  / (cHi - cLo)
      moest1 <- c(cLo, cHi,mobbrd)
      moest2 <- c(cLo, cHi,mobot) 
  }
  trade2str <- function(trv){
    str <- paste("in@", trv[1], "; stop@", trv[2], "; target@", trv[3], "; abs risk@",-abs(trv[1]-trv[2]), "; abs profit@", abs(trv[1]-trv[3]))
  }
  if(side == 1){
    print("-----------------risk rewards 4 longs-------------------------------")
    print(paste("weekly tzone", "|",formatC(wrr1,digits=2), "|"   , trade2str(west1) ))
    print(paste("weekly top ", "|",formatC(wrr2,digits=2),"|"     , trade2str(west2) ))
    print(paste("monthly tzone", "|",formatC(morr1,digits=2), "|" ,trade2str(moest1) ))
    print(paste("monthly top ", "|",formatC(morr2,digits=2),"|"   , trade2str(moest2) ))
  }else{ #shorts
    print("-----------------risk rewards 4 shorts-------------------------------")
    print(paste("weekly bzone",  "|",formatC(wrr1,digits=2), "|"    ,trade2str(west1) ))
    print(paste("weekly bottom ",  "|",formatC(wrr2,digits=2), "|"  ,trade2str(west2) ))
    print(paste("monthly bzone", "|", formatC(morr1,digits=2), "|"  , trade2str(moest1) ))
    print(paste("monthly bottom ", "|",formatC(morr2,digits=2), "|" , trade2str(moest2) ))
  }
})
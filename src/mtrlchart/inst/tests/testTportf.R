context("xts ops")


test_that("xts create remove" , {
 ##xts container 
 xtso <-xts()

 dtfmt<-"%Y-%m-%d %H:%M:%S" 

 #dummy order
 ord <- list(size=5, price=100.00)

 incdatetime1 <- Sys.time()
 recdata <- matrix(unlist(ord),nrow=1)
 xtsrec <-xts(recdata,order.by=incdatetime1 ) 
 colnames(xtsrec) <-c("size", "price")


 xtso <- addxtsrec(xtso, xtsrec, dtfmt) 
 

Sys.sleep(1)
incdatetime2 <- Sys.time()
##dummy order 2
ord <- list(size=-5, price=110.00)
xtsrec<-xts(matrix(unlist(ord),nrow=1),order.by=incdatetime2)

xtso <- addxtsrec(xtso, xtsrec, dtfmt) 
 
expect_equal(nrow(xtso), 2)

## now remove 2 elmements from the xts container

xtso <- rmxtsrec(xtso, incdatetime1, dtfmt)
expect_equal(nrow(xtso), 1)
xtso <- rmxtsrec(xtso, incdatetime2, dtfmt)
expect_equal( length(xtso),0)

#browser()


})


context("instr trade conf")



test_that("instr trade book create" , {

 ##Itradeconf
 tcfn <- crItradeconf("ES","USD",50,0.25,1,0.025)
 tcfnf <- crFullItradeconf("ES","USD",50,0.25,1,0.025,9,30,16,0)

 expect_equal( tcfn@instr, "ES" )
 expect_equal( tcfnf@openh1, 9 )
 expect_equal( tcfnf@closeh1, 16 )


 trb <- crItrbook(tcfn)

 expect_equal( trb@instr, "ES" )


} )
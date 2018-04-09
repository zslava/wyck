#source('Contract.R')
#source('ContractSeq.R')
#source('~/Dropbox/rlab/rcw/rwdata/rwdtn.R')


context("Contract creation")

test_that("Contract Object creation with no data", {

    fc <- new("Contract")
    expect_that( (length(getIname(fc)) > 0), is_true() )
    expect_that( is.numeric(getContractMonth(fc)), is_false() )
    expect_true( is.na(getContractMonth(fc)) )
    expect_that( getExpiry(fc), equals(NA) )
    expect_that(is.na( getFixDayRoll(fc)), is_true() )
    expect_that(is.na( getOnVolumeRoll(fc)), is_true() )

    expect_match( getContractTicker(fc), "undefined" )

} )

test_that("Contract Object creation with month specification", {

     bc <- bareContract(name='ES', ticker='@ES', month=as.Date('2012-03-01'))

    expect_that( (length(getIname(bc)) > 0), is_true() )
    expect_that( str(getContractMonth(bc)), prints_text("Date") )
    expect_that( str(getContractMonth(bc,charfmt=T)), prints_text("chr") )
    expect_that( getExpiry(bc), equals(NA) )
    expect_that(is.na( getFixDayRoll(bc)), is_true() )
    expect_that(is.na( getOnVolumeRoll(bc)), is_true() )

} )

test_that("Contract Object creation with basic specification", {

    bbc <- basicContract(name='ES', ticker='@ES'
                   ,month=as.Date('2012-03-01'),expiry=as.Date("2012-03-19"))

    expect_true(  length(getIname(bbc)) > 0  )
    expect_output( str(getContractMonth(bbc)), "Date" )
    expect_false( is.na(getExpiry(bbc)) )
    expect_output( str(getExpiry(bbc)), "Date" )
    expect_true(is.na( getFixDayRoll(bbc)) )
    expect_true(is.na( getOnVolumeRoll(bbc)) )
} )

test_that("Contract Object setting roll dates", {

    bbc <- basicContract(name='ES', ticker='@ES'
                   ,month=as.Date('2012-03-01'),expiry=as.Date("2012-03-19"))

    expect_true(is.na( getFixDayRoll(bbc)) )
    expect_true(is.na( getOnVolumeRoll(bbc)) )
    ###set rolls
    setFixDayRoll(bbc) <- as.POSIXct('2012-03-13 23:59:59')
    setOnVolumeRoll(bbc) <- as.POSIXct('2012-03-1523:59:59')

    expect_output( str(getFixDayRoll(bbc)), "POSIXct" )
    expect_output( str(getOnVolumeRoll(bbc)), "POSIXct" )
} )

test_that("Contract Object creation with full specification", {

      fbc <-fullContract(name='ES', ticker='@ES'
                     ,month='2011-12-01',expiry="2011-12-21"
                     ,rollFixed="2011-12-17"
                     ,rollVol="2011-12-15"
                      )
    expect_output( str(getExpiry(fbc)), "Date" )
    expect_output( str(getContractMonth(fbc)), "Date" )
    expect_output( str(getFixDayRoll(fbc)), "POSIXct" )
    expect_output( str(getOnVolumeRoll(fbc)), "POSIXct" )

    expect_output( str(getContractTicker(fbc)), "chr" )

  } )






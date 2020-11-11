context("test-util")

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
test_that("zero", {
  # file path
  dir.create( "./inst",showWarnings = FALSE )
  dir.create( "./inst/extdata",showWarnings = FALSE )
  d <- file.path( "./inst/extdata" )

  # spec想定PDF
  specCFD <- data.frame( matrix( NA ,0 ,4 ) )
  colnames( specCFD ) <- c("h1","h2","x1","x4")
    sapply( specCFD,mode )
  specCFD[1,1:2] <- c("証拠金入出金明細書","受渡日")
  specCFD[1,3:4] <- c(25,488)
  save( specCFD ,file=file.path( d,"specCFD.Rda" ) )

  # NO files
  r <- CFD( d )
})

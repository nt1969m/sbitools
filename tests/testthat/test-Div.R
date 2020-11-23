context("test-Div")

# 株式等配当金
test_that("Div.init", {
  Div.init()
})
test_that("Div.pdf", {
  #Div.pdf()
})
test_that("Div", {
  # file path
  dir.create( "./inst",showWarnings = FALSE )
  dir.create( "./inst/extdata",showWarnings = FALSE )
  dir.create( "./inst/extdata/Div",showWarnings = FALSE )
  d <- file.path( "./inst/extdata/Div" )

  #Div( d )
})

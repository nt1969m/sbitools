context("test-alloc")

# 割当株式
test_that("alloc.init", {
  alloc.init()
})
test_that("alloc.pdf", {
  #alloc.pdf()
})
test_that("alloc", {
  # file path
  dir.create( "./inst",showWarnings = FALSE )
  dir.create( "./inst/extdata",showWarnings = FALSE )
  dir.create( "./inst/extdata/As",showWarnings = FALSE )
  d <- file.path( "./inst/extdata/As" )

  As( d )
})

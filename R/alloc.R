# convert many pdf to csv
#'
#' 複数の(.pdf)を(.csv)に変換する。
#'
#' @export
#' @param d file path not raw vector with pdf data
# #' @param s row of specCFD 1:default(current version)
#' @name sbitools
#' @family sbitools
# #' @examples # Just a random pdf file
# #' d <- file.path( "./inst/extdata/As" )
# #' As(d)
#'
As <- function( d ) {
  print( d )
  files <- dir(	d ,pattern = ".pdf$" ,ignore.case=T )
  if( length(files) == 0 ) return(NULL)

  stack <- alloc.init()
  for( f in files ) {
    p <- pdf_info( file.path( d ,f ) )$pages
    df <-	pdf_data( file.path( d ,f ) )
    print( c( f , p ) )

    mei		<-	alloc.pdf( df ,p )
    stack		<-	rbind( stack ,mei )
  }
  csv <- alloc.han(stack)
  #  f <- file.path( d ,"割当株式等.csv" )
#  f <- file.path( d ,paste0( specCFD[s,1] ,".csv" ) )
  #cat(stri_escape_unicode("割当株式等")) \u5272\u5f53\u682a\u5f0f\u7b49
  f <- file.path( d ,"\u5272\u5f53\u682a\u5f0f\u7b49.csv" )
  write.csv( csv ,file=f )
  print( f )

  return( csv )
}

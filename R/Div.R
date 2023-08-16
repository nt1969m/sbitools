# convert many pdf to csv
#' Div
#'
# #' 株式等配当金(.pdf)を(.csv)に変換する。
#'
#' @export
#' @param d file path not raw vector with pdf data
# #' @param s row of specCFD 1:default(current version)
# #' @name sbitools
# #' @family sbitools
# #' @examples # Just a random pdf file
# #' d <- file.path( "./inst/extdata/As" )
# #' As(d)
#'
Div <- function( d ) {
  print( d )
  files <- dir(	d ,pattern = ".pdf$" ,ignore.case=T )
  if( length(files) == 0 ) return(NULL)

  stack <- alloc.init()
  for( f in files ) {
    p <- pdf_info( file.path( d ,f ) )$pages
    df <-	pdf_data( file.path( d ,f ) )
    print( c( f , p ) )

    mei		<-	Div.pdf( df ,p )
    stack		<-	rbind( stack ,mei )
  }
  csv <- Div.fin(stack)
  #  f <- file.path( d ,"株式等配当金.csv" )
#  f <- file.path( d ,paste0( specCFD[s,1] ,".csv" ) )
  #cat(stringi::stri_escape_unicode("株式等配当金")) \u682a\u5f0f\u7b49\u914d\u5f53\u91d1
  f <- file.path( d ,"\u682a\u5f0f\u7b49\u914d\u5f53\u91d1.csv" )
#  write.csv( csv ,file=f )
  write.csv( csv ,file=f ,row.names = FALSE) # 2023-08-16 changed
  print( f )
  message( paste0( "# sum( col.06 " ,colnames( csv[6] ),    "  ) = "
                  ,format( sum( csv[,6] ) # 配当金額
                          ,big.mark=","
                          ,width=15
                          ,scientific=F )
                 )
  )
  message( paste0( "# sum( col.07 " ,colnames( csv[7] ),  "    ) = "
                   ,format( sum( csv[,7] ) # 所得税
                            ,big.mark=","
                            ,width=15
                            ,scientific=F )
                 )
  )
  message( paste0( "# sum( col.08 " ,colnames( csv[8] ),  "    ) = "
                   ,format( sum( csv[,8] ) # 地法税
                            ,big.mark=","
                            ,width=15
                            ,scientific=F )
                   )
  )
  message( paste0( "# sum( col.10 " ,colnames( csv[10] ),  " ) = "
                   ,format( sum( csv[,10] ) # お受取金額
                            ,big.mark=","
                            ,width=15
                            ,scientific=F )
                   )
  )

  return( csv )
}

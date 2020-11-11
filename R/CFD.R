# convert many pdf to csv
#' CFD
#'
#' 複数の(.pdf)を(.csv)に変換する。
#'
#' @export
#' @param d file path not raw vector with pdf data
#' @param s row of specCFD
#' @name sbitools

# #' @examples # Just a random pdf file
# #'  csv <- CFD( pdf_file_path )
CFD <- function( d ,s=1 ) {

  print( d )

    #  specCFD <- read.csv("./data/specCFD.txt") # Warning: found non-ASCII string
    #  data(specCFD,package="sbitools")
    #  specCFD <- data(specCFD,package="sbitools")
    #  specCFD <-
    #  load("./inst/extdata/specCFD.Rda")
  load( file.path( d,"specCFD.Rda" ) )
  print( specCFD )
    #  print.data.frame( specCFD )

  csv <- CFD.init()
    # 要改良
  files	<- dir( d ,pattern=".pdf$" ,ignore.case=T )
  if( length(files) == 0 ) return(NULL)

  for( f in files) {
    p <- pdf_info( file.path( d ,f ) )$pages
    df <-	pdf_data( file.path( d ,f ) )
    print( c( f , p ) )

    n <- CFD.pdf( specCFD ,df ,p )
    if( nrow(n) == 0 ) next

    # print(colnames(n)[1])
#    if(colnames(n)[1] != "X1") {
        colnames(csv) <- colnames(n)
#    }
    csv <- rbind( csv ,n )
  }
  csv <- CFD.fin( csv )

#  f <- file.path( d ,"証拠金入出金明細書.txt" )
  f <- file.path( d ,paste0( specCFD[s,1] ,".txt" ) )
  write.csv( csv ,file=f )
  print( f )

return( csv )
}

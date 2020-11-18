# convert many pdf to csv
#' CFD
#'
#' 証拠金入出金明細書(.pdf)を(.csv)に変換する。
#'
#' @export
#' @param d file path not raw vector with pdf data
#' @param s row of specCFD 1:default(current version)
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
  # default(current version)
  #   # de facto standard :事実上の標準
  specCFD <- specCFD.init()
  # Custom order  :特注
  datapath <- file.path( d,"specCFD.Rda" )
  if(file.exists(datapath))    load( datapath )
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
  f <- file.path( d ,paste0( specCFD[s,1] ,".csv" ) )
  write.csv( csv ,file=f )
  print( f )

return( csv )
}

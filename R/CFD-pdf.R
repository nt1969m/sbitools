# pdf to csv
#' CFD.pdf
#'
#' 説明
#'
#' @export
#' @param specCFD settings
#' @param df pdf_data
#' @param p pages
#' @param s row of specCFD
#' @name sbitools
CFD.pdf <- function( specCFD ,df ,p ,s=1) {
  #  data(specCFD,package="sbitools")
  #  specCFD <- read.csv("./data/specCFD.txt")
  # specCFD <- data(specCFD,package="sbitools")
  #load("./inst/extdata/specCFD.Rda")
    #print( specCFD )
  c_h1 <- specCFD[s,1]
  c_h2 <- specCFD[s,2]
  c_x1 <- specCFD[s,3]
  c_x4 <- specCFD[s,4]

  csv <- CFD.init()	# PDF別
  for( i in 1:p ) {
    print( i )

    df_p <- sapply( df[[ i ]] ,"[" )  [ ,c("x","y","text") ]

    x <- as.integer( df_p[ ,"x" ] )
    y <- as.integer( df_p[ ,"y" ] )

#Warning    h1 <- subset( df_p ,df_p[,"text"] == "証拠金入出金明細書" )
#NG    h1 <- subset( df_p ,df_p[,"x"] == 341 & df_p[,"y"] == 283 )
#NG    h1 <- subset( df_p ,df_p[,"x"] == 341 )
#    h1 <- subset( df_p ,df_p[,"text"] == specCFD[s,1] )
    h1 <- subset( df_p ,df_p[,"text"] == c_h1 )
    if ( nrow( h1 ) == 0 ) next
    print( h1 )

#Warning    h2 <- subset( df_p ,df_p[,"text"] == "受渡日" )
#NG    h2 <- subset( df_p ,df_p[,"y"] == 384 )
#    h2 <- subset( df_p ,df_p[,"text"] == specCFD[s,2] )
    h2 <- subset( df_p ,df_p[,"text"] == c_h2 )
    if ( nrow( h2 ) == 0 ) next

    c <- subset( df_p ,df_p[,"y"] == h2[,"y"] )
    if( nrow( c ) == 7 ) {
      colnames( csv ) <- c[,"text"]
    }
    m <- df_p[ y > h2[,"y"] , ]
#    r <- subset( m, m[ ,"x"] == specCFD[s,3] )
    r <- subset( m, m[ ,"x"] == c_x1 )
    # 		print( r )
#    l <- CFD.line( m ,r ,specCFD[s,4])
    l <- CFD.line( m ,r ,c_x4)
    colnames( l ) <- colnames( csv )
    csv <- rbind( csv ,l )
    # print( csv )
    # print(colnames(csv)[1])
  }
  return( csv )
}

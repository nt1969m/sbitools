# line of pdf to csv
#' CFDpdf
#'
#'
#' @param m DataFrame
#' @param r nrow
#' @param x4 減少の最大の横位置
CFD.line <- function( m ,r ,x4=488 ) {
  csv <- CFD.init()
  for( j in 1:nrow( r ) ) {
    csv[ j ,1 ] <- r[ j,"text"]
      #
    l <- subset( m ,m[ ,"y" ]==r[ j , "y" ] )
    # v0.3.2 add sta
      x <- as.integer( l[,"x"] )
      o <- order( c( x ) )
      l <- l[ o , ]
    # v0.3.2 add end
    csv[ j ,2 ] <- l[ 2,"text"]
      #
    if( as.integer( l[ 3,"x"] ) <= x4 ) {
      csv[ j ,4 ] <- l[ 3,"text"]
    } else {
      csv[ j ,5 ] <- l[ 3,"text"]
    }
      #
    csv[ j ,6 ] <- l[ 4,"text"]
      # 想定外かも？
    switch( ( nrow( l ) - 4 )
            ,"1" = {
              print( l )
            }
    )
  }
  return( csv )
}

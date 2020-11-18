# pdf to csv
#' CFD.pdf
#'
#' 説明
#'
# #' @export
# #' @param specCFD settings
#' @param df pdf_data
#' @param p pages
# #' @param p pages
# #' @param s row of specCFD
# #' @name sbitools
alloc.pdf <- function( df ,p ) {
  # 入力
#  Pages	<- pdf_info( f )$pages	# 総ページ数
#  df	<-	pdf_data( f )
  # 出力
  mei	<- alloc.init()
  for( i in 1:p ) {
    df_p <- df[[i]][c(3,4,6)]
    #df_p[,"x"] <- as.integer( df_p[,"x"] )

    mei[ i , ] <-	c( upper( df_p ) ,lower( df_p ) )
  }
  return( mei )
}

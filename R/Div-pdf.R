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
#配当.pdf <- function( df ,Pages ) {
Div.pdf <- function( df ,p ) {
    # 配当金
#  mei	<- 配当.init()
  mei	<- Div.init()
  for( i in 1:p ) {
#    mei[ p * 2 -1 ,] <-	配当.upper( df ,p )
#    mei[ p * 2     ,] <-	配当.lower(  df ,p )
    df_p <- df[[i]][c(3,4,6)]
    mei[ i * 2 -1 ,]  <-	Div.upper(df_p)
    # 下段
    y6  <- yn( df_p ,166 )
    if(length(y6) == 0 ) next
    mei[ i * 2     ,] <-	Div.lower(df_p)
    #print( paste( " :" ,p ,substr( mei[ p * 2 -1,c(1:2) ] ,1 ,10 ) ,mei[ p * 2 ,c(1:2) ] ) )
  }
  return( mei )
}

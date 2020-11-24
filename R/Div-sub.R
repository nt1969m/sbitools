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
# 上段
#配当.upper	<- function( df ,p )	{
Div.upper	<- function(df)	{
    # p : ページ
  y1 <- yn( df ,91 )
  y2 <- yn( df ,100 )
  y3 <- yn( df ,94 )
  y4 <- yn( df ,126 )
  y5 <- yn( df ,139 )

  y1 <- paste( y1 ,collapse = " " )		# "X" と "YY" を結合
  y2 <- paste( y2 ,collapse = "" )		# "(nnnn" と ")" を結合
  #y3 	<- c(	ymd( y3 )	,配当.unit(	y3 )	)
  y3 <- c( ymd( y3 ) ,Div.unit(	y3 ) )
  y5 	<-		ymd(y5)
  return( c( y1 ,y2 ,y3 ,y4 ,y5 ) )
}
# 下段
Div.lower	<- function( df )	{
  y6  <- yn( df ,166 )
  y7  <- yn( df ,175 )
  y8  <- yn( df ,171 )
  y9  <- yn( df ,202 )
  y10 <- yn( df ,216 )

  y6 <- paste( y6 ,collapse = " " )		# "X" と "YY" を結合
  y7 <- paste( y7 ,collapse = "" )		# "(nnnn" と ")" を結合
  #y8 	<- c(	ymd( y8 )	,配当.unit( y8 )	)
  y8 <- c(	ymd( y8 )	,Div.unit( y8 )	)
  y10	<-		ymd(y10)
  # 返り値
  r <- c( y6 ,y7 ,y8 ,y9 ,y10 )
#  n <- length( r )
#  if	(	n !=	11 	)	{
#    return( c( r ,rep( NA ,11-n ) ) )
#  } else { 			# 明細行であれば
#    return( r )
#  }
return( r )
}
# 単価・数量の対処
Div.unit <- function( ve ) {
  #u	<-	grep( "．" ,ve ) #cat(stringi::stri_escape_unicode("．")) \uff0e
  u	<- grep( "\uff0e"	,ve )
  if( length( u ) != 1 )	return()						# ベクトルの要素が「１」である事
  return( c( ve[u]	,ve[u+1] ) )				# "配当単価" 、"数量"
}


# 上段
#配当.upper	<- function( df ,p )	{
Div.upper	<- function(df)	{
    # p : ページ
  y1 <- yn( df ,91 )
  y2 <- yn( df ,100 )
  y3 <- yn( df ,94 )
  y4 <- yn( df ,126 )
  y5 <- yn( df ,139 )
  #y1		<-	   配当.name( y1 )		# "X" と "YY" を結合
  #y2		<-	   配当.code( y2 )		# "(nnnn" と ")" を結合
  #y3 	<- c(	ymd( y3 )	,配当.unit(	y3 )	)

  #y1 <- Div.name( y1 )		# "X" と "YY" を結合
  y1 <- paste( y1 ,collapse = " " )		# "X" と "YY" を結合
  #y2 <- Div.code( y2 )		# "(nnnn" と ")" を結合
  y2 <- paste( y2 ,collapse = "" )		# "(nnnn" と ")" を結合
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
  #y6		<-	   配当.name( y6 )		# "X" と "YY" を結合
  #y7		<-	    配当.code( y7 )
  #y8 	<- c(	ymd( y8 )	,配当.unit( y8 )	)

  #y6 <- Div.name( y6 )		# "X" と "YY" を結合
  y6 <- paste( y6 ,collapse = " " )		# "X" と "YY" を結合
  #y7 <- Div.code( y7 )		# "(nnnn" と ")" を結合
  y7 <- paste( y7 ,collapse = "" )		# "(nnnn" と ")" を結合
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
#不要 # 銘柄コードの対処
Div.code <- function( ve ) {
  #n	<-	grep( "）" ,ve ) #cat(stringi::stri_escape_unicode("）")) \uff09
  n	<- grep( "\uff09" ,ve )
  if( length( n ) != 1 )	return()						# ベクトルの要素が「１」である事
  switch(	n
          ,	"1"	= paste0( ve[1]	)							# "(NNNN)"
          ,	"2"	= paste0( ve[1]	,ve[2] )				# "(NNNN" ")" or "(" "NNNN)"
          ,	"3"	= paste0( ve[1]	,ve[2] ,ve[3] )	# "(" "NNNN" ")"
  )
}
#不要   # 銘柄名の対処
Div.name <- function( ve ) {
  switch(	length( ve )
          ,	"1"	= paste( ve[ 1 ] )				# "X"
          ,	"2"	= paste( ve[ 1 ] ,ve[ 2 ] )		# "X" and"YY"
  )
}


# y抽出
yn <- function( df ,n )	{
    # n:明細行の"y"
  x <- paste0( df
               [ df$"y" == n	,	]$"x"	)
  x <- as.integer( x )
    #	print( x )	#Debug
  o <- order( c( x ) )
    #	print( o )	#Debug
  t <- paste0( df
               [ df$"y" == n	,	]$"text"	)
    #	print( t )	#Debug
  return( t[ o ] )
}
# 日付の対処
ymd <- function( ve ) {
  #cat(stringi::stri_escape_unicode("日")) \u65e5
  #d	<-	grep(		"日"	,ve		)
  d	<-	grep(		"\u65e5"	,ve		)
  if( length( d ) != 1 )	return( ve )						# ベクトルの要素が「１」である事
# # 以下余白ではない場合
    #cat(stringi::stri_escape_unicode("年")) \u5e74
  # y	<-	grep(		"年"	,ve		)
  y	<-	grep(		"\u5e74"	,ve		)
  switch(	d
          ,	"1"	= {
            if(	y == 1 ) {
              paste0( ve[ d ]	)							# 年月日
            } else
              paste0( ve[ y ] ,ve[ d ]	)				# (月日) (年)	※欧米かよ
          }
          ,	"2"	= paste0( ve[1]	,ve[ d ]		)				# (年)月日 or (年月)日
          ,	"3"	= paste0( ve[1]	,ve[2]		,ve[ d ]		)	# (年)(月)(日)
  )
  #paste(ve[1:d] ,collapse = "" )
}
h.ymd <- function( ve ) {
  #cat(stringi::stri_escape_unicode("年月日"))　\u5e74\u6708\u65e5
  # as.Date( ve ,format="%Y年%m月%d日")
  as.Date( ve ,format="%Y\u5e74%m\u6708%d\u65e5" )
}
# 数字の対処（カンマ削除）
comma <- function( ve ) {
  as.integer( gsub( "," ,"" ,ve ) )
}

# 配当.init	<- function()	{
Div.init <- function() {
# 最終型
 # 列数を初期化
 #  配当金 <- data.frame( matrix( NA ,0 ,11 ) )　
 Div <- data.frame( matrix( NA ,0 ,11 ) )
 # 列名を初期化
 #  colnames( 配当金 )		<-	c(
 colnames(Div) <- c(
   "\u9298\u67c4\u540d"                   # "銘柄名"       # c1
  ,"\u9298\u67c4\u30b3\u30fc\u30c9"       #,"銘柄コード"   # c2
  ,"\u304a\u652f\u6255\u65e5"             #,"お支払日"     # c3
  ,"\u914d\u5f53\u5358\u4fa1"             #,"配当単価"     # c4
  ,"\u6570\u91cf"                         #,"数量"         # c5
  ,"\u914d\u5f53\u91d1\u984d"             #,"配当金額"     # c6
  ,"\u6240\u5f97\u7a0e"                   #,"所得税"       # c7
  ,"\u5730\u65b9\u7a0e"                   #,"地方税"       # c8
  ,"\u7aef\u6570\u51e6\u7406\u4ee3\u91d1" #,"端数処理代金" # c9
  ,"\u304a\u53d7\u53d6\u91d1\u984d"       #,"お受取金額"   # c10
  ,"\u914d\u5f53\u57fa\u6e96\u65e5"       #,"配当基準日"   # c11
 )
 #cat(stringi::stri_escape_unicode("配当基準日"))
 #return( 配当金 )
 return(Div)
}
Div.han <- function( mei ) {
  # 半角
  han	<- Div.init()
  # stri_trans_nfkc( mei ) #NG
  for( i in 1:nrow( mei ) ) {
    han[ i ,1 ]	<-	mei [ i ,1 ]
    han[ i ,-1 ]	<-	stri_trans_nfkc( mei [ i ,-1 ] )
  }
  return( han )
}

Div.fin <- function( stack ) {
#  o <-	! is.na( stack[  ,"銘柄コード" ] )  # 以下余白を削除
#  h_mei	<- Div.han( stack[ o, ] )
  h_mei	<- Div.han( stack )
  #h_mei[ ,"銘柄名"] 		<- substr( h_mei[ ,"銘柄名" ] ,1 ,10 )
  #h_mei[ ,"銘柄コード"] 		<- gsub( "[()]" ,"" ,h_mei[ ,"銘柄コード"] )
  #h_mei[ ,"お支払日"]			<-	as.Date( h_mei[ ,"お支払日"	]		,format="%Y年%m月%d日")
  #h_mei[ ,"配当単価"] 		<- as.integer( gsub( "," ,"" ,h_mei[ ,"配当単価"] ) )
  #h_mei[ ,"数量"] 		<- as.integer( gsub( "," ,"" ,h_mei[ ,"数量"] ) )
  #h_mei[ ,"配当金額"] 		<- as.integer( gsub( "," ,"" ,h_mei[ ,"配当金額"] ) )
  #h_mei[ ,"所得税"]			<-	as.integer( gsub( "," ,"" ,h_mei[ ,"所得税"] ) )
  #h_mei[ ,"地方税"]			<-	as.integer( gsub( "," ,"" ,h_mei[ ,"地方税"] ) )
  #h_mei[ ,"端数処理代金"]	<-comma( h_mei[ ,"端数処理代金"] )
  #h_mei[ ,"お受取金額"]	<-	as.integer( gsub( "," ,"" ,h_mei[ ,"お受取金額"] ) )
  #h_mei[ ,"配当基準日"]	<-	as.Date( h_mei[ ,"配当基準日"	]	,format="%Y年%m月%d日")
  h_mei[ ,1] 	<- substr( h_mei[ ,1 ] ,1 ,10 ) #c1 "銘柄名"
  h_mei[ ,2] 	<- gsub( "[()]" ,"" ,h_mei[ ,2] ) #c2 "銘柄コード"
  h_mei[ ,3]	<- h.ymd( h_mei[ ,3 ] ) #c3 "お支払日"
  h_mei[ ,4]  <- comma( h_mei[ ,4] )  #c4 "配当単価"
  h_mei[ ,5]  <- comma( h_mei[ ,5] )  #c5 "数量"
  h_mei[ ,6]  <- comma( h_mei[ ,6] )  #c6 "配当金額"
  h_mei[ ,7]  <- comma( h_mei[ ,7] )  #c7 "所得税"
  h_mei[ ,8]  <- comma( h_mei[ ,8] )  #c8 "地方税"
  h_mei[ ,9]  <- comma( h_mei[ ,9] )  #c9 "端数処理代金"
  h_mei[ ,10]	<- comma( h_mei[ ,10] ) #c10 "お受取金額"
  h_mei[ ,11]	<- h.ymd( h_mei[ ,11 ] ) #c11 "配当基準日"
  return( h_mei )
}


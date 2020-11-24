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
# 上段
upper <- function( df ) {
  y1 <- yn( df ,126 )
    #print(y1)
  y2 <- yn( df ,135 )
    #cat(stringi::stri_escape_unicode("株式分割"))　\u682a\u5f0f\u5206\u5272
    # k <- grep( "株式分割" ,y1 )
  k <- grep( "\u682a\u5f0f\u5206\u5272" ,y1 )
    # 銘柄 <- y1[ 1:k-1 ]
  c1 <- y1[ 1:k-1 ]
    # 権利区分 <- y1[ k ]
  c3 <- y1[ k ]
    #cat(stringi::stri_escape_unicode("年月日"))　\u5e74\u6708\u65e5
    #n <- grep( "[年月日]" ,y1 )
  n <- grep( "[\u5e74\u6708\u65e5]" ,y1 )
  y <- n[ n > k ]
  ve <- paste( y1[ y ] )
    # 権利割当日 <- ymd( ve )
  c4 <- ymd( ve )
    #cat(stringi::stri_escape_unicode("："))　\uff1a
    # n <- grep( "：" ,y1 )
  n <- grep( "\uff1a" ,y1 )
  # 分割比率 <- paste0( y1[n] ,y1[n+1] )
  c5  <- paste0( y1[n] ,y1[n+1]  )
    # return( c( 銘柄 ,y2 ,権利区分 ,権利割当日 ,分割比率  )  )
  return( c(  c1 ,y2 ,c3 ,c4 ,c5 )  )
}
lower <-  function(  df ) {
  # 下段
  # p : ページ
  y3  <- yn( df  ,180 )
}
alloc.han <-  function(  mei ) {
  # 半角
  han	<- alloc.init()
    # stri_trans_nfkc( mei ) NG
  for( i in 1:nrow( mei ) ) {
    han[  i  ,  ]  <-  stri_trans_nfkc(  mei  [  i  ,  ]  )
  }
    #han[ ,"権利割当日"]			<-	as.Date( han[ ,"権利割当日"	]		,format="%Y年%m月%d日")
    #cat(stringi::stri_escape_unicode("年月日"))　\u5e74\u6708\u65e5
    # han[ ,4]			<-	as.Date( han[ ,4	]		,format="%Y年%m月%d日")
  #han[  ,4]  <-  as.Date(  han[ ,4  ] ,format="%Y\u5e74%m\u6708%d\u65e5")
  han[ ,4] <- h.ymd( han[ ,4 ] )
  return( han )
}


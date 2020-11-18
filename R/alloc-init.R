# initialize DataFrame
#' alloc.init
#'
#'
alloc.init <- function(  ) {
  # 列数を初期化
  alloc <- data.frame( matrix( NA ,0 ,9 ) )

  # 列名を初期化
#  colnames( alloc )		<-	c(
#    "\u9298\u67c4"	         # 1 #cat(stringi::stri_escape_unicode("銘柄"))
#    ,"\u9298\u67c4\u30b3\u30fc\u30c9" # 2 #cat(stri_escape_unicode("銘柄コード"))
#    ,"\u6a29\u5229\u533a\u5206"	      # 3 #cat(stri_escape_unicode("権利区分"))
#    ,"\u6a29\u5229\u5272\u5f53\u65e5" # 4 #cat(stri_escape_unicode("権利割当日"))
#    ,"\u5206\u5272\u6bd4\u7387"       # 5 #cat(stri_escape_unicode("分割比率"))
#    ,"\u52a0\u5165\u8005\u540d"	      # 6 #cat(stri_escape_unicode("加入者名"))
#    ,"\u304a\u9810\u308a\u6570\u91cf"	# 7 #cat(stri_escape_unicode("お預り数量"))
#    ,"\u5272\u5f53\u6570\u91cf"       # 8 #cat(stri_escape_unicode("割当数量"))
#    ,"\u5408\u8a08\u6570\u91cf"       # 9 #cat(stri_escape_unicode("合計数量"))
#  )
  colnames( alloc )		<-	c(
     "\u9298\u67c4"	                  # 1
    ,"\u9298\u67c4\u30b3\u30fc\u30c9" # 2
    ,"\u6a29\u5229\u533a\u5206"	      # 3
    ,"\u6a29\u5229\u5272\u5f53\u65e5" # 4
    ,"\u5206\u5272\u6bd4\u7387"       # 5
    ,"\u52a0\u5165\u8005\u540d"	      # 6
    ,"\u304a\u9810\u308a\u6570\u91cf"	# 7
    ,"\u5272\u5f53\u6570\u91cf"       # 8
    ,"\u5408\u8a08\u6570\u91cf"       # 9
    )
  return( alloc )
}

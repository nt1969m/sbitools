# initialize DataFrame
# #' CFDinit
# #'
# #'
# #' @param n \1:証拠金入出金明細書
CFD.init <- function( n=1 ) {
  # 列数
  csv <- data.frame( matrix( NA ,0 ,7 ) )
  # 列名
  #m <-	sapply( df[[2]] ,"[" ) [ ,c(3,4,6) ]
  #	subset( m, m[,"x"]==25 )	# 32 "受渡日"
  #	subset( m, m[,"x"]==92 )	# 131 "区分"
  #	subset( m, m[,"x"]==213 )	# 275 "摘要"
  #	subset( m, ( m[,"x"]>=478 & m[,"x"]<= 483 ) )	# 382 "証拠金の減少（円）"
  #	subset( m, ( m[,"x"]>=576 & m[,"x"]<= 600 ) )	# 509 "証拠金の増加（円）"
  #	subset( m, ( m[,"x"]>=700 & m[,"x"]<= 743 ) )	# 640 "証拠金の残高（円）"
  #	# subset( m, ( m[,"x"]>=700 & m[,"x"]<= 743 ) )	# 781 "備考"
#  colnames( df ) <- c(
#    "受渡日="			# 列1
#    ,"区分="		# 列2
#    ,"摘要="			# 列3
#    ,"証拠金の減少="	 #,"証拠金の減少（円）="		# 列4
#    ,"証拠金の増加=" #,"証拠金の残高（円）="		# 列5
#    ,"証拠金の残高="	 #,"証拠金の残高（円）="		# 列6
#    ,"備考="		# 列7
#  )
  switch( n
          , "1" =  {		# （２頁〜）証拠金入出金明細書
            #	print( sapply( df ,mode ) )
          }
  )
  return( csv )
}
CFD.fin <- function( csv ) {
  csv[ ,1] <- gsub( "/" ,"-" ,csv[ ,1] )
  csv[ ,4] <- comma( csv[ ,4] )
  csv[ ,5] <- comma( csv[ ,5] )
  csv[ ,6] <- comma( csv[ ,6] )
  #  print( paste0("証拠金の増減=" ,sum( csv[ ,5] ,na.rm=T ) - sum( csv[ ,4] ,na.rm=T ) ) )
  print( paste0(
    colnames(csv)[5]
    ," - "
    ,colnames(csv)[4]
    ," = "
    ,sum( csv[ ,5] ,na.rm=T ) - sum( csv[ ,4] ,na.rm=T ) ) )

  return( csv )
}


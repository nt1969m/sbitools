

sbi_init	<- function(inf_i)	{
  sbi_init <- matrix( NA ,0 ,inf_i$cols ) |>data.frame()
  cols <- inf_i$colnames |> strsplit(",") |> unlist()
  colnames(sbi_init) <- cols
  return( sbi_init )
}

# y抽出
#' @export
#' @param df_p pdf_data[[n]]
#' @param n    pdf_data[[n]]$"y"
#' @param s    pdf_data[[n]]$"x" Starting position
#' @param e    pdf_data[[n]]$"x" End position
#' @param ye   pdf_data[[n]]$"y" End position
#' @name sbitools
ynx <- function( df_p ,n ,s=0 ,e=Inf ,ye=0)	{
#  if(s |> is.na() ) s <- 0
#  if(e |> is.na() ) e <- Inf
  if( ye == 0 ) ye <- n
#  x_text <- subset(df_p,y==n & x>=s & x<e,select = c(x,text))
   x_text <- subset( df_p ,y>=n & y<=ye & x>=s & x<e
                    ,select = c(y,x,text))
  if(x_text |> length() <= 1 ) return(x_text$text)
#  o <- x_text$x |> order()
  o <- order( x_text$y ,x_text$x )
  t <- x_text$"text"[o] |>  paste0(collapse = "")
  return( t )
}

sbi_pdf <- function( df ,p ,inf_i) {
  mei	<- sbi_init(inf_i)
  sta <- mei
  for( i in 1:p ) {
    #    df_p <- df[[i]][c(3,4,6)] # x y text
    df_p <- subset(df[[i]],select=c(x,y,text))
    #    df_p |> print()

    # 上段
    mei[1,]  <-	sbi_row(df_p,inf_i)
    sta <- sta |> rbind(mei)

    # 下段
    if(inf_i$rows == 1) next
    # "以下余白"
    if(grep("\u4ee5\u4e0b\u4f59\u767d",df_p$text) |> length() > 0 ) next
    mei[1,]  <-	sbi_row(df_p,inf_i,rows=2)
    sta <- sta |> rbind(mei)
  }
  return( sta )
}

sbi_row	<- function(df_p,inf_i,rows=1)	{
  #  inf_i |> print()
  mei <- sbi_init(inf_i)
  #  inf_i$cols |> print()
  if(rows == 1)
    y <- inf_i$y |> strsplit(",")|> unlist()
  if(rows == 2)
    y <- inf_i$y2 |> strsplit(",")|> unlist()
  ye <- y
  y  <- gsub( ":.*"  ,"" ,y  ) |> as.integer()
  #     y |> print()
  ye <- gsub( "^.*:" ,"" ,ye ) |> as.integer()
  #     ye |> print()

  s_col <- inf_i$s |> strsplit(",")|> unlist()
  #     s_col |> print()
  s <- gsub( ":.*"  ,"" ,s_col ) |> as.integer()
  #     s |> print()
  e <- inf_i$e |> strsplit(",")|> unlist() |> as.integer()

  if( grep( ":" ,inf_i$s ) |> length() != 0 ) {
    e <- gsub( "^.*:" ,"" ,s_col ) |> as.integer()
  }
  # e |> print()

  for(j in 1:inf_i$cols){
    #    j |> print()
    mei[1,j] <- ynx( df_p ,y[j] ,s[j] ,e[j] ,ye[j] )
  }
  return( mei )
}

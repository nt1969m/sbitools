

sbi_init	<- function(inf_i)	{
  sbi_init <- matrix( NA ,0 ,inf_i$cols ) |>data.frame()
  cols <- inf_i$colnames |> strsplit(",") |> unlist()
  colnames(sbi_init) <- cols
  return( sbi_init )
}

# y抽出
ynx <- function( df_p ,n ,s=0 ,e=Inf)	{
  if(s |> is.na() ) s <- 0
  if(e |> is.na() ) e <- Inf
  x_text <- subset(df_p,y==n & x>=s & x<e,select = c(x,text))
  if(x_text |> length() <= 1 ) return(x_text$text)
  o <- x_text$x |> order()
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
    y <- inf_i$y |> strsplit(",")|> unlist() |> as.integer()
  if(rows == 2)
    y <- inf_i$y2 |> strsplit(",")|> unlist() |> as.integer()
  #    y |> print()

  s <- inf_i$s |> strsplit(",")|> unlist() |> as.integer()
  #    s |> print()
  e <- inf_i$e |> strsplit(",")|> unlist() |> as.integer()
  #    e |> print()
  for(j in 1:inf_i$cols){
    #    j |> print()
    mei[1,j] <- ynx( df_p ,y[j] ,s[j] ,e[j] )
  }
  return( mei )
}

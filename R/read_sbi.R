# 2025-08-14(木) export(read_sbi)
# 2025-08-13(水)
# 2025-08-10(日)

# setwd() ※PDFのあるディレクトリーを設定
# R本体の場合、  「メニュー」 → 「その他」 → 「作業ディレクトリの変更」
# RStudioの場合、^+⇧+h
#「メニュー」→「Session」→「Set Working Directory」→「Choose Directory」

# convert many pdf to csv
# #'
# #' 株式等配当金(.pdf)を(.csv)に変換する。
# #'
#' read_sbi
#'
#' @export
#' @param d file path
#' @param f file name
#' @param p pattern file ex."^inf"
#' @param t title
# #' @param s row of specCFD 1:default(current version)
# #' @name sbitools
# #' @family sbitools
# #' @examples # Just a random pdf file
# #' d <- file.path( "./inst/extdata/As" )
#'
read_sbi <- function( d=getwd(),f=".pdf$",p="",t="" ) {
  "p1: d=" |> message( "\"" ,d ,"\"" )

  "p2: f=" |> message( "\"" ,f ,"\"" )
  files <- dir(	d ,pattern = f ,ignore.case=T )
  if( files |> length() == 0 ) return(NULL)
  data <- files[1] |> pdftools::pdf_data()
  df <- data[[1]]

  if( p == "" ) {
    p <- "read_sbi_p3.csv"
    "p3: p=" |> message( "\"" ,p ,"\" # system.file(package = \"sbitools\")" )
    inf <- p |> system.file(package = "sbitools") |> read.table(header=T)
  }
  else{
    "p3: p=" |> message( "\"" ,p ,"\"" )
    infs <- dir(	d ,pattern = p ,ignore.case=T )
    if( infs |> length() == 0 ) return(NULL)
    message( infs[1] )
    inf <- infs[1] |> read.table(header=T)
  }

  if( t == "" ) {
    t <- "read_sbi_p4" |> system.file(package = "sbitools") |> read.table()
  }
  "p4: t=" |> message( "\"" ,t ,"\"" )
  #  first <- df[df$"y" == 15,]$"text" # title
  ti <- grep(t,df$"text")
  first <- df$"text"[ti]
  first |> message() # title
  i <- which(inf[,3] == first)
  if(i|>length()==0){
    "Not supported" |> message()
    return(NULL)
  }
  inf[i,2] |>message("\u5217") # "列"
  inf_i <- inf[i,]
  # inf_i |> print()

  sbi_init <- inf_i |> sbi_init()
  sbi_init |>colnames() |>cat() |>message()
  stack <-    inf_i |> sbi_init()
  #  colnames(stack) <- sbi_init

  for( f in files ) {
    p <- pdf_info( file.path( d ,f ) )$pages
    data <-	 file.path( d ,f ) |> pdf_data()
    df <- data[[1]]
    message( f ,"\t", p )
    ti <- grep(t,df$"text")    # "お知らせ$|募集$"
    if ( df$"text"[ti] != first )  # title
    {  paste("stop!" ,df$"text"[ti]) |> warning()
      break }

    #    df[[1]][[5,6]] |> stop();
    mei		<-	sbi_pdf( data ,p ,inf_i)
    stack		<- stack |> rbind( mei )
  }
  #  inf_i$han |> print()
  han <- inf_i$han |> strsplit(",")|> unlist() # |> as.integer()
  #  han |> print()
  r <- matrix(NA,inf_i$cols[1] ,2 ) |>data.frame()
  r[,2] <- stack |> colnames()
  r[,1] <- NA |> as.integer()
  for( i in 1:length(han) ){
    if( han[i] == "" ) next
    stack[,i]		<- stack[,i] |> stri_trans_nfkc() # 半角
    if(han[i] == "ymd") {
      stack[,i]		<- stack[,i] |>
        as.Date(format="%Y\u5e74%m\u6708%d\u65e5") # 年月日
    }
    if(han[i] == "s") {
      stack[,i]		<-gsub( "," ,"" ,stack[,i] )
      stack[,i]		<-gsub( "\u2212" ,"-" ,stack[,i] ) # "−" 「Minus Sign」です。
      stack[,i]		<-stack[,i]  |>as.numeric()
      r[i,1] <- stack[,i]  |> sum(na.rm = T)
      c(i,"\t",r[i,1],"\t",r[i,2]) |> message()
    }
  }
  nrow(stack) |>message("\u884C") # "行"
  csv <- first |> paste0(".csv")
  write.table(stack,file=csv,quote=F,row.names=F)
  if( which(han=="s") |> length() != 0 ) {
    csv <- first |> paste0("_sum.csv")
    write.table(r,file=csv,quote=F)
  }
  return(stack)
} # 仮

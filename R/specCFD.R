#' Datasets of specCFD
#'
#' A dataset of spec of pdf files in CFD.
#'
#' @format A data frame with 1or2 rows and 4 variables:
# #' \describe{
# #'   \item{h1}{証拠金入出金明細書}
# #'   \item{h2}{受渡日}
# #'   \item{x1}{position of 受渡日}
# #'   \item{x4}{end position of 証拠金の減少（円）}
# #' }
#' @docType data
#'
# #' @usage data(specCFD)
#'
#' @keywords datasets
#'
# #' @source \url{https://www.com/}
#'
# #' @export
# #' @name sbitools
# #"specCFD"
specCFD.init <- function(){
  # 想定PDF
    # 列数
  spec <- data.frame( matrix( NA ,0 ,4 ) )
    # 列名
  colnames( spec ) <- c("h1","h2","x1","x4")
  # first raw　１行：設定値（現行）

    # specCFD[1,1] <- "証拠金入出金明細書"
    #   cat(stringi::stri_escape_unicode("証拠金入出金明細書"))
  spec[1,1] <- "\u8a3c\u62e0\u91d1\u5165\u51fa\u91d1\u660e\u7d30\u66f8"

      # specCFD[1,2] <- "受渡日"
    #   cat(stringi::stri_escape_unicode("受渡日"))
  spec[1,2] <- "\u53d7\u6e21\u65e5"

  spec[1,3:4] <- c(25,488)

    sapply( spec,mode )
  return(spec)
}

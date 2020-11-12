library(testthat)
library(sbitools)

# file path
d <- file.path( "./inst/extdata" )

# テストデータに含まれる個人情報を
# マスキングした為、
# PDF error: Invalid Font Weight
# が出力されます。

csv <- CFD( d )

test_check("sbitools")

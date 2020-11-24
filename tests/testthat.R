library(testthat)
library(sbitools)

# file path
d <- file.path( "./inst/extdata/CFD" )

# テストデータに含まれる個人情報を
# マスキングした為、
# PDF error: Invalid Font Weight
# が出力されます。

csv <- CFD( d )

#FY <- "/Users/nt1969m/OneDrive/office/確定申告/24期-2021-08-31"
#FY <- "/Users/nt1969m/OneDrive/office/確定申告/23期-2020.08.31"
#d <- file.path( FY ,"B_0988_SBI証券/割当株式" ) # PDF群のディレクトリー
#
d <- file.path( "./inst/extdata/As" )
As( d )

d <- file.path( "./inst/extdata/Div" )
Div( d )

test_check("sbitools")

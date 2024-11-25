# 載入必要套件
library(quantmod)
library(TTR)

# 調整local端工作路徑
#base_dir = getwd()
base_dir = "~/Documents/R_language/stock_data_fetch_app"

print(base_dir)


# 設定工作目錄或提供絕對路徑
setwd(base_dir) 

# 載入外部函式: security_data.R計算均線, KD, RSI數值, 布林格通道
source("stock_data_fetch.R")


## 輸入股碼與時間區間
symbol<- '2330.TW'
end_date <- Sys.Date()
start_date <- end_date-365
data <- stock_data_fetch(symbol, start_date, end_date)

head(data)


## RSI參數設定
rsiPeriod6 <- 6
rsiPeriod12 <- 12
## KD參數設定
kPeriod <- 9

kBBandsPeriod <-20
kBBandsSD <-2

##MA預設 MA5, MA10, MA20, MA50, MA60, MA90


calculate_kdj <- function(stock=stock, n = 9) {
  library(TTR)
  
  tryCatch({
    
    stock_length <- nrow(stock)
    KDJ <- matrix(NA, stock_length, 3) # 構建存放數據的矩陣
    KDJ <- as.data.frame(KDJ)          # 轉換為data.frame
    
    KDJ[1:kPeriod-1, ] <- 50                   # 前kPeriod-1天的K,D,J均設為50
    high_max <- runMax(Hi(stock), n=kPeriod) # 計算kPeriod=9日內最高價
    low_min <- runMin(Lo(stock), n=kPeriod)  # 計算kPeriod=9日內最低價
    rsv <- (Cl(stock) - low_min)/(high_max - low_min) * 100 #計算rvs
    
    for(i in kPeriod:stock_length) {
      KDJ[i, 1] <- 2/3 * KDJ[(i-1), 1] + 1/3 * rsv[i, ]  #K
      KDJ[i, 2] <- 2/3 * KDJ[(i-1), 2] + 1/3 * KDJ[i, 1] #D
      KDJ[i, 3] <- 3 * KDJ[i, 1] - 2 * KDJ[i, 2]         #J
    }
    colnames(KDJ) <- c("K", "D", "J")
    KDJ <- as.xts(KDJ, order.by=index(rsv)) #將KDJ轉化為xts格式

    return(KDJ)
    
  }, error = function(e) {
    message("計算過程中發生錯誤：", e$message)
    return(NULL)
  })
}

stock_data_fetch <- function(symbol, start_date, end_date) {
  
  required_packages <- c("quantmod", "TTR", "PerformanceAnalytics")
  
  
  #取得股票資料
  #stock <- get(getSymbols(symbol,src="yahoo",from=start_date,to=end_date)) #from為起始時間
  stock_og <- getSymbols(symbol, src = "yahoo", from = start_date,to=end_date, auto.assign = FALSE)
  
  # 篩選 Volume 為非 0 的資料 (避掉颱風假2024.10.31未開盤之問題)
  stock <- stock_og[Vo(stock_og) > 0, ]
  close <- Cl(stock)
  
  #print(close)
  # 計算移動平均線 並存入stock data frame
  stock$ma5 <- SMA(close, n = 5)
  stock$ma10 <- SMA(close, n = 10)
  stock$ma20 <- SMA(close, n = 20)
  stock$ma50 <- SMA(close, n = 50)
  stock$ma60 <- SMA(close, n = 60)
  stock$ma90 <- SMA(close, n = 90)
  
  # KDJ
  KDJ <- calculate_kdj(stock=stock, n=9)
  
  head(KDJ)
  
  # stock data frame 新增K, D
  stock$K <- KDJ$K
  stock$D <- KDJ$D
  stock$J <- KDJ$J
  
  
  # 計算RSI
  stock$RSI6 <- RSI(close, n = rsiPeriod6)
  stock$RSI12 <- RSI(close, n = rsiPeriod12)
  
  
  # 計算布林 (20, 2)
  bb <- BBands(close, n = kBBandsPeriod, sd = kBBandsSD)
  stock$BB.up <- bb$up
  stock$BB.mid <- bb$mavg
  stock$BB.down <- bb$dn
  
  stock <- na.omit(stock)
  return(stock)
  
}



#R example
RUN_DEMO <- FALSE
if(RUN_DEMO) {
  
  symbol <- "2330.TW"  #設定YAHOO Finance台灣股票代碼 查詢: https://tw.stock.yahoo.com/rank/turnover
  endDate <-Sys.Date()
  startDate <- endDate-365
  
  stock <- stock_data_fetch(symbol, startDate, endDate)
  head(stock)
  
}

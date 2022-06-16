From Financial analytics with R read Chapter 3.7, 3.8, 7.1

#Download data for five stocks of your choice + SPY
#####Problem 1#####
# Read Chapters 3.7 and 3.8 and calculate the alpha and beta for each
# of the five stocks 
library(tidyquant)
library(tidyverse) 
library("PerformanceAnalytics")
library(fPortfolio)
library(timeSeries)
library(ggplot2)

spy <- tidyquant::tq_get("SPY") 

stock_data <- c("FB","AMZN","NFLX","AAPL", "NKE", "SPY")
prices <- getSymbols(stock_data, src = "yahoo",
                     from = 2015-01-01,
                     auto.assign = TRUE,
                     warnings  = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge)%>% 
  'colnames<-'(stock_data) 

prices_month <-to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
asset_returns <-na.omit(Return.calculate(prices_month, method = "log")) 

spy_market_benchmark <- getSymbols("SPY", src ="yahoo",
                                   from = 2015-01-01,
                                   auto.assign = TRUE,
                                   warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  'colnames<-'("SPY") %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) 

market_return <- na.omit(Return.calculate(spy_market_benchmark, method = "log")) 

asset_FB <- getSymbols("FB", src = "yahoo",
                       from = 2015-01-01,
                       auto.assign = TRUE,
                       warnings = FALSE) %>%
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  'colnames<-'("FB") %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) 

FB_return <- na.omit(Return.calculate(asset_FB, method = "log")) 


FB_beta=CAPM.beta(FB_return, market_return) 
FB_alpha = CAPM.alpha(FB_return,market_return) 


asset_AMZN <- getSymbols("AMZN", src = "yahoo",
                       from = 2015-01-01,
                       auto.assign = TRUE,
                       warnings = FALSE) %>%
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  'colnames<-'("AMZN") %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) 

AMZN_return <- na.omit(Return.calculate(asset_AMZN, method = "log")) 

AMZN_beta <- CAPM.beta(AMZN_return, market_return) 
AMZN_alpha <- CAPM.alpha(AMZN_return, market_return)


asset_NFLX <- getSymbols("NFLX", src = "yahoo",
                         from = 2015-01-01,
                         auto.assign = TRUE,
                         warnings = FALSE) %>%
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  'colnames<-'("NFLX") %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) 

NFLX_return <- na.omit(Return.calculate(asset_NFLX, method = "log")) 

NFLX_beta <- CAPM.beta(NFLX_return, market_return) 
NFLX_alpha <- CAPM.alpha(NFLX_return, market_return)


asset_AAPL <- getSymbols("AAPL", src = "yahoo",
                         from = 2015-01-01,
                         auto.assign = TRUE,
                         warnings = FALSE) %>%
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  'colnames<-'("AAPL") %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) 

AAPL_return <- na.omit(Return.calculate(asset_AAPL, method = "log")) 

AAPL_beta <- CAPM.beta(AAPL_return, market_return) 
AAPL_alpha <- CAPM.alpha(AAPL_return, market_return) 



asset_NKE <- getSymbols("NKE", src = "yahoo",
                         from = 2015-01-01,
                         auto.assign = TRUE,
                         warnings = FALSE) %>%
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  'colnames<-'("NKE") %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) 

NKE_return <- na.omit(Return.calculate(asset_NKE, method = "log")) 

NKE_beta <- CAPM.beta(NKE_return, market_return) 
NKE_alpha <- CAPM.alpha(NKE_return, market_return)

#####Problem 1#####

#####Problem 2#####
# Simulate the efficient frontier of a portfolio created by the 5 stocks
# you have chosen. Here is a solution written in Python.
# https://www.interviewqs.com/blog/efficient-frontier
# Pay attention that you should use portfolio returns: The percentage
# change in the stock price: price-lag(price)/lag(price) 

stock_data <- c("FB","AMZN","NFLX","AAPL", "NKE")
prices <- getSymbols(stock_data, src = "yahoo",
                     from = 2015-01-01,
                     auto.assign = TRUE,
                     warnings  = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge)%>% 
  'colnames<-'(stock_data) 

prices_month <-to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
asset_returns <-na.omit(Return.calculate(prices_month, method = "log")) 

#or 

asset_returns <- log(prices_month) - log(lag(prices_month)) 

summary(asset_returns)
asset_returns <- as.timeSeries(asset_returns)
efficient.frontier <- portfolioFrontier(asset_returns, constraints = "LongOnly") 
plot(efficient.frontier, c(1,2,3,4,5,6,7))

#1 - efficient frontier 
#2 - global minimum variance portfolio 
#3 - optimal portfolio 
#4 - risk/return for each asset 
#5 - equal weights portfolio 
#6 - two assets frontier 
#7 - monte carlo portfolio 
#8 - sharpe ratio 


# Create a table with randomized weights, which add up to one.
# I'd suggest to start with 5 columns - one for each of 
# the stocks, which contain the corresponding weights.
# Then you can add twenty-three additional columns:
# Five columns with the expected return for each of the stocks.
# Five columns with the standard deviation for each of the stocks.
# Ten columns with the covariances between each two stocks(5*4/2 = 10)
# Two column for Expected return and the standard deviation of the portfolio
# One column for Sharpe ratio, which you will calculate and add later.
# This is the formula for the sum of expected values - E[aX+bY] = aE[X]+ bE[Y].
# The formula for variances is Var(aX+aY)=a^2*Var(X)+b^2*Var(Y)+2Cov(X,Y). 
# And then take the square root to get the standard deviation.
# Calculate the Sharpe ratio, using risk free rate of 1% - look at chapter 7.1.
# Choose the portfolio with the highest Sharpe ratio

#####Problem 2#####    


 
#alpha - excess average return, indicates great performance on the market (when the market return is zero)



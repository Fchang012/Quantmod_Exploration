# R Script to get financial data from yahoo and google APIs


# Libraries ---------------------------------------------------------------
require(quantmod)


# Get Stock Info ----------------------------------------------------------
getSymbols(c("SPY",
             "BHF",
             "COP"), from=as.Date("16-01-01", format="%y-%m-%d"))


# Chart Series ------------------------------------------------------------
x11()
chartSeries(COP)
addBBands()
addMACD()
addRSI()
addExpiry()


# Options Info ------------------------------------------------------------
# Turning it into a function

OptionChain <- function(stockTickerDF){
  # get ticker name
  stockTicker <- deparse(substitute(stockTickerDF))
  
  # Getting historical options prices
  HistoricalOptionPriceDF <- stockTickerDF[options.expiry(stockTickerDF),]
  
  # Get All Option Chains
  theOptionChain <- getOptionChain(stockTicker, "2018")
  
  # Options Chain - Calls In the Money (NextOptions Expiry Date)
  CallInTheMoney <- theOptionChain[[1]]$calls[theOptionChain[[1]]$calls$Strike <= getQuote(stockTicker)$Last,]
  
  # Options Chain - Puts In The Money (NextOptions Expiry Date)
  PutInTheMoney <- theOptionChain[[1]]$puts[theOptionChain[[1]]$puts$Strike >= getQuote(stockTicker)$Last,]
  
  # Quick General Market Sentiment Based on Volumne of Options
  # Total vol sum of Call Option strike price out of money / Total vol sum of Put Option strike price out of money
  # 1 = Neutral
  # > 1 = Bullish
  # < 1 = Bearish
  VolBull <- sum(theOptionChain[[1]]$calls[theOptionChain[[1]]$calls$Strike >= getQuote(stockTicker)$Last,]$Vol)
  VolPut <- sum(theOptionChain[[1]]$puts[theOptionChain[[1]]$puts$Strike <= getQuote(stockTicker)$Last,]$Vol)
  BullToPutRatio <- VolBull/VolPut
  
  # Options Chain - Calls Out of the Money (NextOptions Expiry Date)
  CallOutOfTheMoney <- theOptionChain[[1]]$calls[theOptionChain[[1]]$calls$Strike >= getQuote(stockTicker)$Last,]
  
  # Options Chain - Puts Out of The Money (NextOptions Expiry Date)
  PutOutOfTheMoney <- theOptionChain[[1]]$puts[theOptionChain[[1]]$puts$Strike <= getQuote(stockTicker)$Last,]

  # Return the List
  theList <- list("HistoricalOptionPriceDF" = HistoricalOptionPriceDF,
                  "theOptionChain" = theOptionChain,
                  "CallInTheMoney" = CallInTheMoney,
                  "PutInTheMoney" = PutInTheMoney,
                  "VolBull" = VolBull,
                  "VolPut" = VolPut,
                  "BullToPutRatio" = BullToPutRatio,
                  "CallOutOfTheMoney" = CallOutOfTheMoney,
                  "PutOutOfTheMoney" = PutOutOfTheMoney)
  
  return(theList)
}
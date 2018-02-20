# R Script to get financial data from yahoo and google APIs


# Libraries ---------------------------------------------------------------
require(quantmod)


# Get Stock Info ----------------------------------------------------------
getSymbols(c("SPY",
             "BHF",
             "COP"), from=as.Date("16-01-01", format="%y-%m-%d"))


# Options Info ------------------------------------------------------------
# Turning it into a function

OptionChain <- function(stockTickerDF, thePeriod=1){
  # get ticker name
  stockTicker <- deparse(substitute(stockTickerDF))
  
  # Getting historical options prices
  HistoricalOptionPriceDF <- stockTickerDF[options.expiry(stockTickerDF),]
  
  # Get All Option Chains
  theOptionChain <- getOptionChain(stockTicker, "2018")
  
  # Options Chain - Calls In the Money (NextOptions Expiry Date)
  CallInTheMoney <- theOptionChain[[thePeriod]]$calls[theOptionChain[[thePeriod]]$calls$Strike <= getQuote(stockTicker)$Last,]
  
  # Options Chain - Puts In The Money (NextOptions Expiry Date)
  PutInTheMoney <- theOptionChain[[thePeriod]]$puts[theOptionChain[[thePeriod]]$puts$Strike >= getQuote(stockTicker)$Last,]
  
  # Quick General Market Sentiment Based on Volumne of Options
  # Total vol sum of Call Option strike price out of money / Total vol sum of Put Option strike price out of money
  # 1 = Neutral
  # > 1 = Bullish
  # < 1 = Bearish
  VolCall <- sum(theOptionChain[[thePeriod]]$calls[theOptionChain[[thePeriod]]$calls$Strike >= getQuote(stockTicker)$Last,]$Vol)
  VolPut <- sum(theOptionChain[[thePeriod]]$puts[theOptionChain[[thePeriod]]$puts$Strike <= getQuote(stockTicker)$Last,]$Vol)
  CallToPutRatioVol <- VolCall/VolPut
  
  # Quick General Market Sentiment Based on Open Interest of Options
  # Total sum OI Call Option strike price out of money / Total sum OI Put Option strike price out of money
  # 1 = Neutral
  # > 1 = Bullish
  # < 1 = Bearish
  OICall <- sum(theOptionChain[[thePeriod]]$calls[theOptionChain[[thePeriod]]$calls$Strike >= getQuote(stockTicker)$Last,]$OI)
  OIPut <- sum(theOptionChain[[thePeriod]]$puts[theOptionChain[[thePeriod]]$puts$Strike <= getQuote(stockTicker)$Last,]$OI)
  CallToPutRatioOI <- OICall/OIPut
  
  # Options Chain - Calls Out of the Money (NextOptions Expiry Date)
  CallOutOfTheMoney <- theOptionChain[[thePeriod]]$calls[theOptionChain[[thePeriod]]$calls$Strike >= getQuote(stockTicker)$Last,]
  
  # Options Chain - Puts Out of The Money (NextOptions Expiry Date)
  PutOutOfTheMoney <- theOptionChain[[thePeriod]]$puts[theOptionChain[[thePeriod]]$puts$Strike <= getQuote(stockTicker)$Last,]

  # Return the List
  theList <- list("HistoricalOptionPriceDF" = HistoricalOptionPriceDF,
                  "theOptionChain" = theOptionChain,
                  "CallInTheMoney" = CallInTheMoney,
                  "PutInTheMoney" = PutInTheMoney,
                  "VolCall" = VolCall,
                  "VolPut" = VolPut,
                  "CallToPutRatioVol" = CallToPutRatioVol,
                  "OICall" = OICall,
                  "OIPut" = OIPut,
                  "CallToPutRatioOI" = CallToPutRatioOI,
                  "CallOutOfTheMoney" = CallOutOfTheMoney,
                  "PutOutOfTheMoney" = PutOutOfTheMoney)
  
  return(theList)
}

# Chart Series ------------------------------------------------------------
x11()
chartSeries(SPY)
addBBands()
addMACD()
addRSI()
addExpiry()
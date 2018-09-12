# R Script to get financial data from yahoo and google APIs


# Libraries ---------------------------------------------------------------
require(quantmod)


# Get Stock Info ----------------------------------------------------------
getSymbols(c("VTI",
             "T"), from=as.Date("18-01-01", format="%y-%m-%d"))


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
  
  # Implementation of the Put/Call Ratio (Volume)
  # Total vol sum of Call Option / Total vol sum of Put Option 
  # 1 = Neutral
  # > 1 = Bearish
  # < 1 = Bullish
  VolPut <- sum(theOptionChain[[thePeriod]]$puts$Vol)
  VolCall <- sum(theOptionChain[[thePeriod]]$calls$Vol)
  PutToCallRatioVol <- VolPut/VolCall
  
  # Implementation of the Put/Call Ratio (Open Interest)
  # Total sum OI Call Option / Total sum OI Put Option
  # 1 = Neutral
  # > 1 = Bearish
  # < 1 = Bullish
  OIPut <- sum(theOptionChain[[thePeriod]]$puts$OI)
  OICall <- sum(theOptionChain[[thePeriod]]$calls$OI)
  PutToCallRatioOI <- OIPut/OICall
  
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
                  "PutToCallRatioVol" = PutToCallRatioVol,
                  "OICall" = OICall,
                  "OIPut" = OIPut,
                  "PutToCallRatioOI" = PutToCallRatioOI,
                  "CallOutOfTheMoney" = CallOutOfTheMoney,
                  "PutOutOfTheMoney" = PutOutOfTheMoney)
  
  return(theList)
}

# After running OptionChain function run this to get call spread with break @ the money
callSpread <- function(theOptionList){
  print(theOptionList$CallInTheMoney)
  print("------------------------------------------------------------")
  print(theOptionList$CallOutOfTheMoney)
}

# After running OptionChain function run this to get put spread with break @ the money
putSpread <- function(theOptionList){
  print(theOptionList$PutOutOfTheMoney)
  print("------------------------------------------------------------")
  print(theOptionList$PutInTheMoney)
}

# Chart Series ------------------------------------------------------------
x11()
chartSeries(VTI)
addBBands()
addMACD()
addRSI()
addExpiry()

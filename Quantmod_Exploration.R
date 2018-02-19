# R Script to get financial data from yahoo and google APIs


# Libraries ---------------------------------------------------------------
require(quantmod)


# Get Stock Info ----------------------------------------------------------
getSymbols(c("SPY"), from=as.Date("16-01-01", format="%y-%m-%d"))


# Chart Series ------------------------------------------------------------
x11()
chartSeries(SPY)
addBBands()
addMACD()
addRSI()
addExpiry()


# Options Info ------------------------------------------------------------
# Getting historical options prices
SPY[options.expiry(SPY),]

# Get All Option Chains
SPYOptionChain <- getOptionChain("SPY", "2018")

# Options Chain - Calls In the Money (NextOptions Expiry Date)
SPYOptionChain[[1]]$calls[SPYOptionChain[[1]]$calls$Strike <= getQuote("SPY")$Last,]

# Options Chain - Puts In The Money (NextOptions Expiry Date)
SPYOptionChain[[1]]$puts[SPYOptionChain[[1]]$puts$Strike >= getQuote("SPY")$Last,]

# Quick General Market Sentiment Based on Volumne of Options
# Closest Call Option strike price out of money / Closet Put Option strike price out of money
# 1 = Neutral
# > 1 = Bullish
# < 1 = Bearish
VolBull <- SPYOptionChain[[1]]$calls[SPYOptionChain[[1]]$calls$Strike >= getQuote("SPY")$Last,][1,]$Vol
VolPut <- tail(SPYOptionChain[[1]]$puts[SPYOptionChain[[1]]$puts$Strike <= getQuote("SPY")$Last,], n=1)$Vol
BullToPutRatio <- VolBull/VolPut

# Options Chain - Calls Out of the Money (NextOptions Expiry Date)
SPYOptionChain[[1]]$calls[SPYOptionChain[[1]]$calls$Strike >= getQuote("SPY")$Last,]

# Options Chain - Puts Out of The Money (NextOptions Expiry Date)
SPYOptionChain[[1]]$puts[SPYOptionChain[[1]]$puts$Strike <= getQuote("SPY")$Last,]

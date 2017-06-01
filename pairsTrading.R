######################################################################
# The code below is structured in the following manner:
# Part A -
# Helper Functions viz. Signal, ParameterEstimates,
# ParameterEstimatesHistorical, Stationarity, Weights, Returns, and
# .return
# Part B -
# Load the data and perform analysis based on the defined functions
######################################################################
# Part A
######################################################################

# Decision Rule enabling trade execution
Signal <- function(spread, threshold)
{
  signal <- ifelse(spread >=  threshold, -1, NA)
  signal <- ifelse(spread <= -threshold, 1, signal)
  return(na.locf(signal))
}

# Test that log(price) follows I(1) process and
# Calculate the spread between two log stock prices.
ParameterEstimates <- function(price.pair, method = lm)
{
  x <- log(price.pair)
  
  reg <- method(x[, 2] ~ x[, 1])
  hedgeRatio <- as.numeric(reg$coef[2])
  premium     <- as.numeric(reg$coef[1])
  spread      <- x[, 2] - (hedgeRatio * x[, 1] + premium)
  list(spread = spread, hedgeRatio = hedgeRatio, premium = premium)
}
ParameterEstimatesHistorical <- function(price.pair, period, method = lm)
{
  Apply <- function(price.pair){
    reg <- ParameterEstimates(price.pair, method)
    c(spread = as.numeric(last(reg$spread)), hedgeRatio = reg$hedgeRatio, premium = reg$premium)
  }
  as.xts(rollapplyr(price.pair, period, Apply, by.column = FALSE))
}

#Returns weather spread is stationary or not
Stationarity <- function(spread, threshold)
{
  Is.passed.PP.test  <- PP.test(as.numeric(spread))$p.value <= threshold
  Is.passed.adf.test <- adf.test(as.numeric(spread))$p.value <= threshold
  c(PP.test = Is.passed.PP.test, adf.test = Is.passed.adf.test)
}

Weights <- function(hedgeRatio)
{
  hedgeRatio <- abs(hedgeRatio) * (-1)
  normalization.factor <- 1 / (1 + abs(hedgeRatio))
  return(cbind(1 * normalization.factor, hedgeRatio * normalization.factor))
}

Returns <- function(price.pair, signal.lagged, hedgeRatio.lagged)
{
  signal      <- as.xts(na.omit(cbind(signal.lagged, -1*(signal.lagged))))
  return.pair <- as.xts(na.omit(.return(price.pair, type = "discrete")))
  weight.pair <- as.xts(na.omit(Weights(hedgeRatio.lagged)))
  x <-          as.xts(apply(merge(signal[, 1], weight.pair[, 1], return.pair[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.pair[, 2], return.pair[, 2], all = FALSE), 1, prod)))
  
  if(!length(dim(x))){
    xts(rep(NA, nrow(price.pair)), order.by = index(price.pair))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}
.return <- function(x, type = c("continuous", "discrete"), na.pad = TRUE) 
{
  type <- match.arg(type)
  if (type == "discrete") {
    result <- x/lag(x, na.pad = na.pad) - 1
  }else if (type == "continuous") {
    result <- diff(log(x), na.pad = na.pad)
  }
  return(result)
}
######################################################################
# Part B
######################################################################
library(tseries)
library(xts)
library(zoo)
library(readr)

infy <- read_csv("~/Backup Anirudh/M.Sc. Semester IV/Time Series II/Term Paper/infy.csv")
tcs <- read_csv("~/Backup Anirudh/M.Sc. Semester IV/Time Series II/Term Paper/tcs.csv")
hcl <- read_csv("~/Backup Anirudh/M.Sc. Semester IV/Time Series II/Term Paper/hcl.csv")

infy_prices <- as.numeric(infy$`Adj Close`)
tcs_prices <- as.numeric(tcs$`Adj Close`)
hcl_prices <- as.numeric(hcl$`Adj Close`)
dates <- as.Date(infy$Date,format = '%d-%m-%Y')

# Pair-wise correlations between the 3 stocks
cor(tcs$`Adj Close`,infy$`Adj Close`)
cor(hcl$`Adj Close`,infy$`Adj Close`)
cor(tcs$`Adj Close`,hcl$`Adj Close`)

# Store data in data frames for prices, log prices and log returns

prices <- data.frame(Dates = dates,TCS = tcs_prices,INFY = infy_prices, HCL = hcl_prices)


# Store the data in zoo objects
TCSPrices <- with(prices, zoo(TCS, order.by = Dates))
INFYPrices <- with(prices, zoo(INFY, order.by = Dates))
HCLPrices <- with(prices, zoo(HCL, order.by = Dates))

# Plot Prices
plot(TCSPrices, col = 'blue', main = 'TCS Prices (???)')
plot(INFYPrices, col = 'brown', main = 'Infosys Prices (???)')
plot(HCLPrices, col = 'purple', main = 'HCL Prices (???)')

# TCS-INFY pair ######################################################

pair <- cbind(TCSPrices, INFYPrices)
regress <- ParameterEstimates(pair, method = lm)
str(regress)

plot(regress$spread, main = 'TCS-INFY Spread', col = 'brown')

#check stationarity
Stationarity(regress$spread, 0.10)

#estimate parameters for back test
params <- ParameterEstimatesHistorical(pair, period = 180)

#create & plot trading signals
signal <- Signal(params$spread, 0.15)
mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
barplot(signal,col= mycol,space = 0, border = mycol,xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread, main = 'TCS-INFY Spread and Signal')

#Performance of pair trading
plot(params$hedgeRatio, main = 'TCS-INFY 180-day Moving-Window Hedge Ratio')
returns <- Returns(pair, lag(signal), lag(params$hedgeRatio))
plot(100 * cumprod(1 + returns), main = 'cumulative returns (%)')
as.numeric(cumprod(1 + returns)[length(returns)])

# HCL-INFY pair ######################################################

pair <- cbind(HCLPrices, INFYPrices)
regress <- ParameterEstimates(pair, method = lm)
str(regress)

plot(regress$spread, main = 'HCL-INFY Spread', col = 'brown')

#check stationarity
Stationarity(regress$spread, 0.10)

#estimate parameters for back test
params <- ParameterEstimatesHistorical(pair, period = 180)

#create & plot trading signals
signal <- Signal(params$spread, 0.10)
mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
barplot(signal,col= mycol,space = 0, border = mycol,xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread, main = 'HCL-INFY Spread and Signal')

#Performance of pair trading
plot(params$hedgeRatio, main = 'HCL-INFY 180-day Moving-Window Hedge Ratio')
returns <- Returns(pair, lag(signal), lag(params$hedgeRatio))
plot(100 * cumprod(1 + returns), main = 'HCL-INFY cumulative returns (%)')
as.numeric(cumprod(1 + returns)[length(returns)])

# HCL-TCS pair #######################################################

pair <- cbind(HCLPrices, TCSPrices)
regress <- ParameterEstimates(pair, method = lm)
str(regress)

plot(regress$spread, main = 'HCL-TCS Spread', col = 'brown')

#check stationarity
Stationarity(regress$spread, 0.10)

#estimate parameters for back test
params <- ParameterEstimatesHistorical(pair, period = 180)

#create & plot trading signals
signal <- Signal(params$spread, 0.10)
mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
barplot(signal,col= mycol,space = 0, border = mycol,xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread, main = 'Spread and Signal')

#Performance of pair trading
plot(params$hedgeRatio, main = 'HCL-TCS 180-day Moving-Window Hedge Ratio')
returns <- Returns(pair, lag(signal), lag(params$hedgeRatio))
plot(100 * cumprod(1 + returns), main = 'cumulative returns (%)')
as.numeric(cumprod(1 + returns)[length(returns)])

cumulative_returns <- 100 * cumprod(1 + returns)

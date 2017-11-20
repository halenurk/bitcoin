
# An Exploration on cryptocurrencies
# Clean R environment, Remove everything from environment
rm(list=ls(all=TRUE))
setwd("C:/Users/Enespc/Desktop/HASAN/Introduction to Data Analysis/project")

# Install required packages
# rvest: easy web scraping with R; https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/
# install.packages("rvest")  
# plotrix: Various Plotting Functions;Lots of plots, various labeling, axis and color scaling functions.
# install.packages('plotrix')
# Treemaps display hierarchical data as a set of nested rectangles.
# install.packages("treemap")
#install.packages('plotly)
#install.packages('gclus')

# Define URL where retrieve cryptocurrencies datas, the URL is coinmarketcap.com
url_cryptocurrencies <- "https://coinmarketcap.com/currencies/views/all/"
# Load required libraries
# library() command loads <rvest>, a package to read web data
library(rvest)
# plotrix is plotting utility
library(plotrix)
# dplyr package is for data manipulation
library(dplyr)
# Treemaps display hierarchical data as a set of nested rectangles.
library(treemap)
library(ggplot2)
library(corrplot)
library(plotly)
library(quantmod)

# Read the price table
# The data, CryptoCurrency Market Capitalizations (CMC) price chart, is "read" from their website to df_cryptocurrencies dataframe
url_cryptocurrencies %>% 
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table() %>%
  as.data.frame() -> "df_cryptocurrencies" # save table to variable df_cryptocurrencies
# Let's see what we have as df_cryptocurrencies
head(df_cryptocurrencies)
# View(df_cryptocurrencies)

# Alternate (second) way to retreive datas (price infos of cryto currencies) by using JSON API from api.coinmarketcap.com
# The JSON API provide clean data
library(jsonlite)
allCurrencies_cmc <- as.data.frame(fromJSON("https://api.coinmarketcap.com/v1/ticker/"))
allCurrencies_cmc$rank <- NULL
allCurrencies_cmc[,c(-1,-2,-3)] <- lapply(allCurrencies_cmc[,c(-1,-2,-3)], as.numeric)
# Let's view dataframe, allCurrencies_cmc
head(allCurrencies_cmc, 10)
# The JSON API provides more cleaner data than RVEST, 
# but we apply cleaning steps to dirty dataframe which was retreived by RVEST 

# Clean data
# Before cleaning, Name strings should be modified as "\n" is replaced with "-" in df_cryptocurrencies$Name column
df_cryptocurrencies$Name <- lapply(df_cryptocurrencies$Name, gsub, pattern = "\n", replacement = "-")
# The table, df_cryptocurrencies contains a few unwanted characters such as 
# new line, \n, $,*, spaces, %. In order to conduct analysis, we need to remove these characters.

df_cryptocurrencies[] <- lapply(df_cryptocurrencies, gsub, pattern = "\\\n|\\s|[%*$,?]", replacement = "")
# remove the first column X. (coins' ranking of coinmarketcap web site) 
df_cryptocurrencies$X. <- NULL
# Update the column names small, lowercase and meaningful
names(df_cryptocurrencies) <- c("name", "symbol", "marketcap", "price", "supply", "volume", "change_1h", "change_24h", "change_7d")
# Let's see how our data frame, df_cryptocurrencies looks after cleaning process
head(df_cryptocurrencies)

# Prepare data for mathematical analysis
# We make some matematical analysis but type of variables should be numeric, not character
# Check type of marketcap
typeof(df_cryptocurrencies$marketcap)
# The type of marketcap is character 
# So we have to transform type of variables to numeric except name and symbol to new data frame num_df_cryptocurrencies
num_df_cryptocurrencies <- lapply(df_cryptocurrencies[-c(1:2)], as.numeric) %>% as.data.frame()
# create new dataframe named as df_allcryptocurrencies including 
df_allcryptocurrencies <- cbind(df_cryptocurrencies$name, df_cryptocurrencies$symbol, num_df_cryptocurrencies)
names(df_allcryptocurrencies) <- c("name", "symbol", "marketcap", "price", "supply", "volume", "change_1h", "change_24h", "change_7d")
# Check type of marketcap, be sure that it is numeric
typeof(df_allcryptocurrencies$marketcap)
# Double is a floating point data type in R on which mathematical operations are valid.
# Let's see how our data frame, df_allcryptocurrencies looks
head(df_allcryptocurrencies, 15)
dim(df_allcryptocurrencies)
# How many coins are there in the market?
nrow(df_allcryptocurrencies)

# Market share of all cryto currencies as treemap
df_allcryptocurrencies$formatted_market_cap <-  paste0(df_allcryptocurrencies$name,'\n','$',format(df_allcryptocurrencies$marketcap,big.mark = ',',scientific = F, trim = T))
treemap(df_allcryptocurrencies, index = 'formatted_market_cap', vSize = 'marketcap', title = 'Cryptocurrency Market Capitalizations', fontsize.labels=c(12, 8), palette='RdYlGn')

# Explore the evolution of the market in 1 hour, 24 hours and 7 days by using all currencies, df_allcryptocurrencies df
# Calculate SD and summary of statistics of market change in 1 hour, 24 hours and 7 days

summary(df_allcryptocurrencies$change_1h)
sd(df_allcryptocurrencies$change_1h, na.rm=TRUE)
summary(df_allcryptocurrencies$change_24h)
sd(df_allcryptocurrencies$change_24h, na.rm=TRUE)
boxplot(df_allcryptocurrencies$change_7d, horizontal=TRUE, col= c("red"), main="Box plot of An Evolution of Currencies in 7 days")
hist(df_allcryptocurrencies$change_7d, col="purple",main="Histogram of An Evolution of Currencies in 7 days",xlab="An Evolution of Currencies in 7 days")
summary(df_allcryptocurrencies$change_7d)
sd(df_allcryptocurrencies$change_7d, na.rm=TRUE)


# Find cryptocurrencies whose value has more than doubled in 7d
df_allcryptocurrencies_double_in7d <- df_allcryptocurrencies[df_allcryptocurrencies$change_7d > 100,]
#Let's remove all lines containing NAs
df_allcryptocurrencies_double_in7d <- df_allcryptocurrencies_double_in7d[complete.cases(df_allcryptocurrencies_double_in7d),]
df_allcryptocurrencies_double_in7d

# Find cryptocurrencies whose value lost is larger than 25% half in 7d
df_allcryptocurrencies_reduced_in7d <- df_allcryptocurrencies[df_allcryptocurrencies$change_7d < -25,]
#Let's remove all lines containing NAs
df_allcryptocurrencies_reduced_in7d <- df_allcryptocurrencies_reduced_in7d[complete.cases(df_allcryptocurrencies_reduced_in7d),]
df_allcryptocurrencies_reduced_in7d

# Dominant Anlysis, Is there any dominant one? Find dominant currencies and How much?
total_coin_market_cap <-sum(df_allcryptocurrencies$marketcap, na.rm=TRUE)
Bitcoin_Dominance <- df_allcryptocurrencies$marketcap[1]/total_coin_market_cap
# Format total_coin_market_cap and BitcoinDominance for easy understanding and presenting very well
total_coin_market_cap_formatted <-  paste0('$',format(total_coin_market_cap,big.mark = ',',scientific = F, trim = T))
Bitcoin_Dominance_formatted <-  paste0(format(Bitcoin_Dominance, big.mark = ',',scientific = F, trim = T),'%')
total_coin_market_cap_formatted
# The total market capitalization is currently more than 190 billion USD !
Bitcoin_Dominance_formatted
# Even though Bitcoin is sharing the market with more and more cryptos, it remains the biggest player.

# Let's focus on top 10 currrencies by market cap
# Total coin market cap, aggregate
total_coin_market_cap <-sum(df_allcryptocurrencies$marketcap, na.rm=TRUE)
top_10_market_cap <- arrange(.data = df_allcryptocurrencies, desc(marketcap))[1:10, ]
# Add a column named <marketcap_percent> which percentage of a cryptocurrency market share
top_10_market_cap$marketcap_percent <- top_10_market_cap$marketcap/total_coin_market_cap
# Display first to 10 crypto currencies by market cap 
head(top_10_market_cap, 10)

# Market share of top 10 coins
market_share_top_10 <- (sum(top_10_market_cap$marketcap)/total_coin_market_cap)*100
market_share_top_10_formatted <-  paste0(format(market_share_top_10, big.mark = ',',scientific = F, trim = T),'%')
market_share_top_10_formatted

# Market Cap of top 10 currencies as 3D pie chart
# Create labels for pie chart
lbls <- paste0(top_10_market_cap$name, " : ", sprintf("%.2f", top_10_market_cap$marketcap_percent*100), "%")
pie3D(top_10_market_cap$marketcap_percent, labels = lbls,
      explode=0.5, main="Top 10 Cryptocurrencies Market Share")


# Historical Data Collection from CoinMarketCap web site
# HTML data is converted into a dataframe because there is no API for the historic of a cryptocurrencies
# We will study the daily evolution of top 5 cryptocurrencies: 
# Let's find top 5 currrencies by market cap
top_5_market_cap <- arrange(.data = df_allcryptocurrencies, desc(marketcap))[1:5, ]
# Add a column named <marketcap_percent> which percentage of a cryptocurrency market share
top_5_market_cap$marketcap_percent <- top_5_market_cap$marketcap/total_coin_market_cap
# Display first to 10 crypto currencies by market cap 
head(top_5_market_cap)
# Top 5: BTC-Bitcoin, ETH-Ethereum, BCH-BitcoinCash, XRP-Ripple, LTC-Litecoin

Months <- data.frame('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', stringsAsFactors = F)
colnames(Months) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

convertTime <- function(string) {
  return(as.Date(paste(substr(string, 6, 9), "-", as.character(Months[substr(string, 1, 3)]), "-", substr(string, 4, 5), sep = "")))
}

getHistoric <- function(coin = 'bitcoin', start = '2013-04-28', end = Sys.Date()) {
  start = strsplit(as.character(start), split='-')[[1]]
  start = paste(start[1], start[2], start[3], sep = "")
  end = strsplit(as.character(end), split='-')[[1]]
  end = paste(end[1], end[2], end[3], sep = "")
  url <- paste("https://coinmarketcap.com/currencies/", coin, 
               "/historical-data/?start=", start, "&end=", end, sep="")
  data <- as.data.frame(html_table(html_nodes(css = "table", x = read_html(url))))
  data[] <- lapply(data, gsub, pattern = "\\\n|\\s|[%*$,?]", replacement = "")
  data$X. <- NULL
  data[-1] <- sapply(data[-1], as.numeric)
  data$Date <- convertTime(data$Date)
  data <- addPercentage(data)
  return(data)
}

addPercentage <- function(data) {
  data$Percentage <- c(0,(data$Close[2:nrow(data)] - data$Close[1:(nrow(data)-1)]) / data$Close[2:nrow(data)])
  return(data)
}
# Import Bitcoin historical price datas
df_bitcoin <- getHistoric(coin = 'bitcoin', start = '2013-04-28', end = Sys.Date())
# Let's check everything is OK
head(df_bitcoin)
# Save Bitcoin historical price to file named bitcoin_historical_price.csv (in the working directory)
write.csv(df_bitcoin, file = "bitcoin_historical_price.csv")

# Import Ethereum historical price datas
df_ethereum <- getHistoric(coin = 'ethereum', start = '2013-04-28', end = Sys.Date())
# Let's check everything is OK
head(df_ethereum)
# Save Ethereum historical price to file named ethereum_historical_price.csv (in the working directory)
write.csv(df_ethereum, file = "ethereum_historical_price.csv")

# Import BitcoinCash historical price datas
df_bitcoincash <- getHistoric(coin = 'bitcoin-cash', start = '2013-04-28', end = Sys.Date())
# Let's check everything is OK
head(df_bitcoincash)
# Save BitcoinCash historical price to file named bitcoincash_historical_price.csv (in the working directory)
write.csv(df_bitcoincash, file = "bitcoincash_historical_price.csv")

# Import Ripple historical price datas
df_ripple <- getHistoric(coin = 'ripple', start = '2013-04-28', end = Sys.Date())
# Let's check everything is OK
head(df_ripple)
# Save Ripple historical price to file named ripple_historical_price.csv (in the working directory)
write.csv(df_ripple, file = "ripple_historical_price.csv")

# Import Litecoin historical price datas
df_litecoin <- getHistoric(coin = 'litecoin', start = '2013-04-28', end = Sys.Date())
# Let's check everything is OK
head(df_litecoin)
# Save Litecoin historical price to file named litecoin_historical_price.csv (in the working directory)
write.csv(df_litecoin, file = "litecoin_historical_price.csv")

# Prepare date format of top 5 crypto currencies
df_bitcoin$Date <- as.Date(df_bitcoin$Date)
df_ethereum$Date <- as.Date(df_ethereum$Date)
df_bitcoincash$Date <- as.Date(df_bitcoincash$Date)
df_ripple$Date <- as.Date(df_ripple$Date)
df_litecoin$Date <- as.Date(df_litecoin$Date)

# Explore historical evolution of Bitcoin price
# Plot bitcoin price versus time
p_01 <- ggplot(data = df_bitcoin)
p_01 + labs(title = "History of Bitcoin Price") + labs(x = "Date") + labs(y = "Price in USD") + geom_line(aes(x = Date, y = Close)) 


summary(df_bitcoin$Percentage)
sd(df_bitcoin$Percentage)
# Bitcoin is very volatile: it has gained up to 23% or lost up to 43% in only one day.
# candle plot eklenebilir..finansc�lar sever


# Explore historical evolution of Ethereum price
# Plot Ethereum price versus time
p <- ggplot(data = df_ethereum)
p + geom_line(aes(x = Date, y = Close)) + labs(x = "Date") + labs(y = "Price in USD") + labs(title = "History of Ethereum Price")
summary(df_ethereum$Percentage)
sd(df_ethereum$Percentage)
# Ethereum is also very volatile: it has gained up to 72% or lost up to 51% in only one day.

# Correlation between cryptocurrencies
# Correlation of Market Cap
# If Bitcoin's market cap rises, how do its competitors behave ?
correlation <- cor(cbind(Bitcoin = df_bitcoin[df_bitcoin$Date > as.Date('2017-07-23'),]$Close, Ethereum = df_ethereum[df_ethereum$Date > as.Date('2017-07-23'),]$Close, Ripple = df_ripple[df_ripple$Date > as.Date('2017-07-23'),]$Close, Litecoin = df_litecoin[df_litecoin$Date > as.Date('2017-07-23'),]$Close, Bitcoincash = df_bitcoincash[df_bitcoincash$Date > as.Date('2017-07-23'),]$Close))
corrplot(correlation)
cor(correlation)
# The market capitalizations of these five cryptocurrencies are highly correlated. 

# Correlation of volatility between cryptocurrencies
correlation <- cor(cbind(Bitcoin = df_bitcoin[df_bitcoin$Date > as.Date('2017-07-23'),]$Percentage, Ethereum = df_ethereum[df_ethereum$Date > as.Date('2017-07-23'),]$Percentage, Ripple = df_ripple[df_ripple$Date > as.Date('2017-07-23'),]$Percentage, Litecoin = df_litecoin[df_litecoin$Date > as.Date('2017-07-23'),]$Percentage, Bitcoincash = df_bitcoincash[df_bitcoincash$Date > as.Date('2017-07-23'),]$Percentage))
corrplot(correlation)

# The correlations are much weaker except between Bitcoin and Bitcoincash
# It is interesting to trade cryptocurrencies on exchanges. 
# The market is highly volatile so it can be dangerous.

# Correlation with well known Asset, Gold
# Bitcoin may be accepted as the gold of the digital world. Are gold and Bitcoin correlated ?
correlation <- function(series1, series2, k = 0) {
  len = length(series1)
  return(cor(cbind(series1[1:(len-k)], series2[(1+k):len]), use = 'na.or.complete')[1,2])
}
# The daily prices of gold on Quandl are downloaded as CSV format, gold_daily.csv
Gold_Data <- read.csv('gold_daily.csv', header = T, stringsAsFactors = F)
df_Gold <- data.frame(Date = as.Date(Gold_Data$Date), Close = Gold_Data$Value)
correlation(df_bitcoin$Close, df_Gold[is.element(df_Gold$Date,df_bitcoin$Date),]$Close)

# It can be seen that there is no correlation. 
# Bitcoin is only a tiny fraction of gold in terms of market capitalization. Bitcoin and gold are two separate markets


## Added by Emine
#In here I want to plot top 5 currency open and close prices in the same graph
plot(df_bitcoin$Date,(df_bitcoin$Close-df_bitcoin$Open), type = 'line', col="blue", main = 'Daily Differences between Opening and Closing Price', xlab ='Date', ylab = 'Differences')
lines(df_ethereum$Date,(df_ethereum$Close-df_ethereum$Open), col="red")
lines(df_bitcoincash$Date,(df_bitcoincash$Close-df_bitcoincash$Open), col='green')
lines(df_ripple$Date,(df_ripple$Close-df_ripple$Open), col="orange")
lines(df_litecoin$Date,(df_litecoin$Close-df_litecoin$Open), col='black')
legend("topleft", legend=c("bitcoin", "Ethereum", "bitcoincash", "ripple", "litecoin"), 
       col=c("blue", "red", "green", "orange","black"), lty=1:5, bty ="n" , cex=0.6)
## Plot Daily Percentage Change of the top 5 curriencies:
plot(df_bitcoin$Date,df_bitcoin$Percentage, type = 'line', col="blue", main = 'Percentage of Closing Price', xlab ='Date', ylab = 'Differences')
lines(df_ethereum$Date,df_ethereum$Percentage, col="red")
lines(df_bitcoincash$Date,df_bitcoincash$Percentage, col='green')
lines(df_ripple$Date,df_ripple$Percentage, col="orange")
lines(df_litecoin$Date,df_litecoin$Percentage, col='black')
legend("topleft", legend=c("bitcoin", "Ethereum", "bitcoincash", "ripple", "litecoin"), 
       col=c("blue", "red", "green", "orange","black"), lty=1:2, bty ="n" , cex=0.5)

#How did the historical prices / market capitalizations of top 5 currencies (in terms of
#market capitalization) change over time?
#a. plot price vs time, each and all
#b. market caps vs time
##Plot Currencis Change over time
plot(df_bitcoin$Date,df_bitcoin$High, type = 'line', col="blue", main = 'Curriencies High Price', xlab ='Date', ylab = 'High Price')
lines(df_ethereum$Date,df_ethereum$High, col="red")
lines(df_bitcoincash$Date,df_bitcoincash$High, col='green')
lines(df_ripple$Date,df_ripple$High, col="orange")
lines(df_litecoin$Date,df_litecoin$High, col='black')
legend("topleft", legend=c("bitcoin", "Ethereum", "bitcoincash", "ripple", "litecoin"), 
       col=c("blue", "red", "green", "orange","black"), lty=1:2, bty ="n" , cex=0.5)

# Correlation: Bitcoin vs Google
# In here, we try to define the correlation between bitcoin and google search:
# Import Google search  historical price datas. Data yi google dan otomatik cekemedim malesef, library(gtrendsR) paketini
#yuklememe ragmen calismadi, bende maual cektim datayi. Bundan dolayi bu kod sizde calismaya bilir, once google trend datasini download etmeniz gerekiyor.
df_GoogleSearch <- read.csv('multiTimeline.csv', header = F, stringsAsFactors = F)[c(-1,-2),]
colnames(df_GoogleSearch) <- c('Date', 'Volume')
df_GoogleSearch$Date <- as.Date(df_GoogleSearch$Date)
df_GoogleSearch$Volume <- as.numeric(df_GoogleSearch$Volume)
row.names(df_GoogleSearch) <- NULL
df_GoogleSearch <- df_GoogleSearch[nrow(df_GoogleSearch):1,]
correlation(df_GoogleSearch$Volume, df_bitcoin[is.element(df_bitcoin$Date, df_GoogleSearch$Date),]$Close)
# Let's check everything is OK
# Save Bitcoin historical price to file named bitcoin_historical_price.csv (in the working directory)

## Add Candle Plot for bit coin And Ethereum
# custom colors
## Candle Plot for Bitcoin Last 30 days
##Barlarin uzerine gelindiginde gosterilen degelerin mean, max, median gibi degerleri gosterdigini ekleyemedim..
i <- list(line = list(color = '#FFD700'))
d <- list(line = list(color = '#0000ff'))
df_bitcoin_Last30 <- head(df_bitcoin, n = 30)
p <- df_bitcoin_Last30 %>%
  plot_ly(x = df_bitcoin_Last30$Date, type="candlestick",
          open = df_bitcoin_Last30$Open, close = df_bitcoin_Last30$Close,
          high = df_bitcoin_Last30$High, low = df_bitcoin_Last30$Low,
          increasing = i, decreasing = d) %>%
  layout(title = "Candlestick Chart For Bitcoin: Last 30 Days")
print(p)
## Candle Plot for Bitcoin Whole Data sets
p <- df_bitcoin %>%
  plot_ly(x = df_bitcoin$Date, type="candlestick",
          open = df_bitcoin$Open, close = df_bitcoin$Close,
          high = df_bitcoin$High, low = df_bitcoin$Low) %>%
  add_lines(y = df_bitcoin$Open, line = list(color = 'black', width = 0.75)) %>%
  layout(title= "Bitcoin Candlestick Chart", showlegend = TRUE)
print(p)

## Linear Regression Added by Emine: 11/19/2017 Sunday
# Linear regression analysis between Volume and Market.Cap, Volume and HighProce+Percentage+Market.Cap
library(dplyr)
df_bitcoin_RegModel <- lm(formula = Close ~ High, data=df_bitcoin) # regression formula
summary(df_bitcoin_RegModel) # show regression coefficients table
plot(df_bitcoin$Close, df_bitcoin$High)
abline(df_bitcoin_RegModel,col ='red')

# Summarize and print the results
df_bitcoin_RegModel2 <- lm(Volume ~ Market.Cap + High + Percentage, data = df_bitcoin)
summary(df_bitcoin_RegModel2) # show regression coefficients table
plot(df_bitcoin_RegModel2, which= 1)

## Logistic Regression Analysis..
##predict the probability of bitcoin price with high-low, open-close, volume and percentage changing data
# collapse all missing values to NA
# run our regression model
## Add increasing or decreasing information to bitcoin data sets
df_bitcoin_Prediction <- df_bitcoin %>% mutate(Changing = ifelse(Percentage > 0, 1, ifelse(Percentage < 0, -1, 0)))
# check levels of bitcoin whether or not it is increasing or decreasing
df_bitcoin_Prediction$Changing <- factor(df_bitcoin_Prediction$Changing, levels=c(-1, 1, 0))
PricePrediction <- glm(Changing~High+Low+Open+Close+Volume+Percentage,
               data=df_bitcoin_Prediction, family="binomial")
anova(PricePrediction)
summary(PricePrediction)
coef(summary(PricePrediction))
predict(PricePrediction, type="response") # predicted values
residuals(PricePrediction, type="deviance") # residuals
cdplot(Changing~Low,  data=df_bitcoin_Prediction)
##Define relationships between the cryptocurriencies:


################
library(lattice)
pairs(~Open+High+Close+Low +Percentage, data=df_bitcoin_Prediction,
                   main="Bitcoin Price Changing")
super.sym <- trellis.par.get("superpose.symbol")
splom(df_bitcoin_Prediction[c(2,3,4,5,8)], groups = df_bitcoin_Prediction$Changing, data=df_bitcoin_Prediction,
      panel=panel.superpose, 
      key=list(title="Bitcoin Price Changing",
               columns=3,
               points=list(pch=super.sym$pch[1:3],
                           col=super.sym$col[1:3]),
               text=list(c("1 Increasing","-1 Decreasing","0 Stable"))))

# Scatterplot Matrices from the car Package
## 
library(car)
scatterplotMatrix(~ ~Open+High+Close+Low +Percentage | Changing, smoother=FALSE, 
                  by.group=TRUE, transform=TRUE, data=df_bitcoin_Prediction, main="Bitcoin Price Changing")

library(gclus)# The lattice package provides options to condition the scatterplot matrix on a factor.
dta <- df_bitcoin_Prediction[c(2,3,4,5,8)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )
###################
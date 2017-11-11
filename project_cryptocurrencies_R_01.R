# An Exploration on cryptocurrencies
# Clean R environment, Remove everything from environment
rm(list=ls(all=TRUE)) 

# Install required packages
# rvest: easy web scraping with R; https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/
# install.packages("rvest")  
# plotrix: Various Plotting Functions;Lots of plots, various labeling, axis and color scaling functions.
# install.packages('plotrix')
# Treemaps display hierarchical data as a set of nested rectangles.
# install.packages("treemap")

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
# but we apply cleaning steps to dataframe which was retreived by RVEST 

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

# Market share of all cryto currencies as treemap
df_allcryptocurrencies$formatted_market_cap <-  paste0(df_allcryptocurrencies$name,'\n','$',format(df_allcryptocurrencies$marketcap,big.mark = ',',scientific = F, trim = T))
treemap(df_allcryptocurrencies, index = 'formatted_market_cap', vSize = 'marketcap', title = 'Cryptocurrency Market Capitalizations', fontsize.labels=c(12, 8), palette='RdYlGn')

# Dominant Anlysis, Is there any dominant one? Find dominant currencies and How much?
total_coin_market_cap <-sum(df_allcryptocurrencies$marketcap, na.rm=TRUE)
Bitcoin_Dominance <- df_allcryptocurrencies$marketcap[1]/total_coin_market_cap
# Format total_coin_market_cap and BitcoinDominance for easy understanding and presenting very well
total_coin_market_cap_formatted <-  paste0('$',format(total_coin_market_cap,big.mark = ',',scientific = F, trim = T))
Bitcoin_Dominance_formatted <-  paste0(format(Bitcoin_Dominance, big.mark = ',',scientific = F, trim = T),'%')
total_coin_market_cap_formatted
Bitcoin_Dominance_formatted


# Let's focus on top 10 currrencies by market cap
top_10_market_cap <- arrange(.data = df_allcryptocurrencies, desc(marketcap))[1:10, ]
# Add a column named <marketcap_percent> which percentage of a cryptocurrency market share
top_10_market_cap$marketcap_percent <- top_10_market_cap$marketcap/sum(top_10_market_cap$marketcap)
# Display first to 10 crypto currencies by market cap 
head(top_10_market_cap, 10)
# Total coin market cap, aggregate
total_coin_market_cap <-sum(df_allcryptocurrencies$marketcap, na.rm=TRUE)
# Market share of top 10 coins
market_share_top_10 <- (sum(top_10_market_cap$marketcap)/total_coin_market_cap)*100
market_share_top_10_formatted <-  paste0(format(market_share_top_10, big.mark = ',',scientific = F, trim = T),'%')
market_share_top_10_formatted

# Market Cap of top 10 currencies as 3D pie chart
# Create labels for pie chart
lbls <- paste0(top_10_market_cap$name, " : ", sprintf("%.2f", top_10_market_cap$marketcap_percent*100), "%")
pie3D(top_10_market_cap$marketcap_percent, labels = lbls,
      explode=0.5, main="Top 10 Cryptocurrencies Market Share")




library(tidyverse)
library(tidyquant)
library(lubridate)
library(recipes)

# Ticker and Sector Tibble ------------------------------------------------

ticks <- tibble(tickers = c("BAC","JPM","WFC", "AAPL", "MSFT","GOOG","NEE", "DUK",
                            "AMZN", "DIS", "PG", "KO", "XOM", "CVX", "JNJ", "UNH",
                            "BA", "MMM", "VZ", "T", "DWDP", "MON"), 
                sector = c("Financials", "Financials", "Financials", 
                           "Tech", "Tech", "Tech", "Utilities", "Utilities",
                           "Consumer Discretionary", "Consumer Discretionary", "Consumer Staples",
                           "Consumer Staples", "Energy", "Energy", "Healthcare", "Healthcare",
                           "Industrials", "Industrials", "Telecom", "Telecom" , "Materials", "Materials"))


# Stock Data --------------------------------------------------------------

prices  <- tq_get(ticks[1], get = "stock.prices", from = "1976-01-01")%>%
    select(tickers, date, close, volume)

prices <- prices%>%
    mutate(quarter = quarter(date, with_year = TRUE))%>%
    group_by(tickers, quarter)%>%
    summarize(avg_price = mean(close, na.rm = TRUE), avg_volume = mean(volume, na.rm = TRUE))

# Fed Data ----------------------------------------------------------------

actual <- read_csv("actual.csv")
baseline <- read_csv("baseline.csv")%>%
    filter(`Scenario Name` == 'Supervisory Baseline')
adverse <- read_csv("adverse.csv")%>%
    filter(`Scenario Name` == 'Supervisory Adverse')
severely_adverse <- read_csv("severely_adverse.csv")%>%
    filter(`Scenario Name` == 'Supervisory Severely Adverse')

fed_data <- bind_rows(actual, baseline, adverse, severely_adverse)%>%
    mutate(quarter = quarter(date, with_year = TRUE))
fed_data$`Scenario Name` <- as.factor(fed_data$`Scenario Name`)

# Join Data ----------------------------------------------------------------

data <- left_join(fed_data, prices, by = c("quarter"))
    
    


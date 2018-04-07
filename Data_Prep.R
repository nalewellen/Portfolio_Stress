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


write_csv(ticks, "Tickers.csv")

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
    mutate(quarter = quarter(Date,with_year = TRUE))%>%
    select(-Date)
fed_data$`Scenario Name` <- as.factor(fed_data$`Scenario Name`)

nber <- read_csv("USREC.csv", col_names = c("Date", "Stress"), skip = 1)%>%
    mutate(quarter = quarter(Date, with_year = TRUE))%>%
    group_by(quarter)%>%
    summarize(Stress = round(mean(Stress, na.rm = TRUE)))

# Join Data ----------------------------------------------------------------

data <- left_join(fed_data, prices, by = c("quarter"))

data <- left_join(data, nber, by = c("quarter"))%>%
    mutate(Stress = if_else(is.na(Stress) == TRUE & `Scenario Name` == 'Supervisory Adverse', 1, 
                            if_else(is.na(Stress) == TRUE & `Scenario Name` == 'Supervisory Baseline', 0, 
                            if_else(is.na(Stress) == TRUE & `Scenario Name` == 'Supervisory Adverse' ,0, Stress))))

write_csv(data, "Model Data Set.csv")
    
# Preprocess Data ----------------------------------------------------------------

#This creates variable transformations and preprocesses data. 

final_data <- data%>%
    group_by(tickers)%>%
    arrange(tickers, quarter)%>%
    mutate_if(is.numeric, funs(lag = lag(.)))%>%
    mutate_if(is.numeric, funs(log = log(.)))%>%
    mutate_if(is.numeric, funs(diff = . - lag(.)))%>%
    select(-Stress_lag,-Stress_log, -Stress_lag_diff, -Stress_lag_log, -Stress_diff, -Stress_log_diff, - Stress_lag_log_diff)

write_csv(final_data, "final data set.csv")


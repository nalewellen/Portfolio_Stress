library(tidyverse)
library(tidyquant)
library(lubridate)
library(fmsb)
library(reshape2)

# Load Data ------------------------------------------------

model_data <- read_csv("final data set.csv")

# APPL Model -----------------------------------------------

test_data <- filter(model_data, tickers == "AAPL" | is.na(tickers) == TRUE)%>%
    filter(`Scenario Name` == 'Actual')%>%
    na.omit()

# Correlation test_data

corr_test <- test_data%>%
    dplyr::select(22:34, 36:37)

cormat <- round(cor(corr_test),2)

melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()

# Make top model

min <- lm(avg_price_log ~ 1, data = test_data)

names <- names(test_data)

vars <- cat(paste0("`",names,"`") , sep = " + ")

max <- lm(avg_price_log ~`10-year Treasury yield_log` + `Commercial Real Estate Price Index (Level)_log` + `Market Volatility Index (Level)_log`, data = test_data)

summary(max)

#final <- step(max, scope = c(min, max), direction = "both")

final <- max

summary(final)
VIF(final)

predict_data <- filter(model_data, tickers == "AAPL" | is.na(tickers) == TRUE)

final_pred <- cbind(predict_data, yhat = predict(final, predict_data))%>%
    dplyr::select(`Scenario Name`, tickers, quarter, yhat, avg_price_log)%>%
    mutate(avg_price_level = exp(avg_price_log), hat_sd = StdDev(yhat),
           avg_price_hat_level = exp(yhat))

final_pred%>%
    filter(`Scenario Name` == 'Actual')%>%
    ggplot()+
        geom_line(aes(x = quarter, y = avg_price_level), color = "Black", group = 1)+
        geom_line(aes(x = quarter, y = avg_price_hat_level), color = "Red", group = 1)

final_pred <- final_pred%>%
    filter(quarter >= 2018.1)%>%
    mutate(yhat = if_else(quarter == 2018.1, avg_price_log, yhat))

final_pred <- final_pred%>%
    mutate(avg_price_level = exp(avg_price_log), hat_sd = StdDev(yhat),
           avg_price_hat_level = exp(yhat + (.5)*(hat_sd**2)))

final_pred%>%
    filter(`Scenario Name` == 'Actual')%>%
    ggplot()+
        geom_line(aes(x = quarter, y = avg_price_level), group = 1)+
        geom_line(aes(x = quarter, y = avg_price_hat_level), group = 1)

ggplot()+
    geom_line(data = final_pred, aes(x = quarter, y = avg_price_level, color = `Scenario Name`, group = `Scenario Name`))+
    geom_line(data = final_pred, aes(x = quarter, y = avg_price_hat_level, color = `Scenario Name`, group = `Scenario Name`))+
    ggtitle("APPL Forecast by Scenario")+
    theme_minimal()+
    ylab("Average Quarterly Price")+
    xlab("Quarter")





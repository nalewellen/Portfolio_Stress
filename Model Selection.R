### Model Selection Algorithm

<<<<<<< HEAD
library(tidyverse)
library(fmsb)
library(MASS)

# Load Data

model_data <- read_csv("final data set.csv")

test_data <- filter(model_data, tickers == "AAPL" | is.na(tickers) == TRUE)%>%
    filter(`Scenario Name` == 'Actual')%>%
    na.omit()

data <- model_data

# Algo to build model

ticker <- "AAPL"

build_model <- function(ticker, data, direction = c("both", "backward", "forward")){

    model_data <- filter(data, tickers == ticker | is.na(tickers) == TRUE)%>%
        filter(`Scenario Name` == 'Actual')%>%
        na.omit()
    
    first_model<- lm(avg_price_log ~ 1, data = model_data)
    
    final_model <- lm(avg_price_log ~ `Nominal disposable income growth_log` 
                      + `Unemployment rate_log` + `CPI inflation rate_log`  
                      + `10-year Treasury yield_log` + `BBB corporate yield_log` 
                      + `Mortgage rate_log` + `Prime rate_log` 
                      + `Dow Jones Total Stock Market Index (Level)_log` 
                      + `House Price Index (Level)_log` 
                      + `Commercial Real Estate Price Index (Level)_log` 
                      + `Market Volatility Index (Level)_log` 
                      + `avg_volume_log`, data = model_data)
    
    invisible(capture.output(
        last_model <- stepAIC(final_model,
                          scope = c(lower = first_model, upper = final_model), 
                          direction = direction)
    ))
    return(last_model)
    
}

# Loop Algo

list_tickers <- unique(as.list(na.omit(model_data$tickers)))

models <- list()

for (i in list_tickers){
    
    models[[i]] <- build_model(i, model_data, "backward")
}

# Print simple fit statistics

for(i in list_tickers){
    print(paste0(i," r-sq: " ,summary(models[[i]])$r.squared)) 
    print(paste0(i," adjR-sq: " ,summary(models[[i]])$adj.r.squared))
    cat("\n")
    }
=======
>>>>>>> 4165bb67c83272f303df35300ad25a1308ae93da

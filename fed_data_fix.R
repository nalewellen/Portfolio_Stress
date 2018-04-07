library(tidyverse)
library(zoo)
library(tidyquant)

# stack data

filenames <- paste0(getwd(),"/Data/",list.files(paste0(getwd(),"/Data")))

df <- lapply(filenames, function(i){
    read_csv(i)})
    
df <- do.call("rbind", df)

# write.csv(df, "fed_data.csv", row.names = FALSE)

# Fix dates

df$Date <- as.Date(as.yearqtr(df$Date, format = "%Y Q%q"), frac = 1)

# Subset data

df$`Scenario Name` <- as.factor(df$`Scenario Name`)


actual <- df%>%
    filter(`Scenario Name` == "Actual")%>%
    arrange(Date)

write.csv(actual, "actual.csv", row.names = FALSE)

baseline <- df%>%
            filter(`Scenario Name` == "Actual" | `Scenario Name` == "Supervisory Baseline")%>%
            arrange(Date)

write.csv(baseline, "baseline.csv", row.names = FALSE)

adverse <- df%>%
            filter(`Scenario Name` == "Actual" | `Scenario Name` == "Supervisory Adverse")%>%
            arrange(Date)

write.csv(adverse, "adverse.csv", row.names = FALSE)

severely_adverse <- df%>%
            filter(`Scenario Name` == "Actual" | `Scenario Name` == "Supervisory Severely Adverse")%>%
            arrange(Date)

write.csv(severely_adverse, "severely_adverse.csv", row.names = FALSE)

# Daily values 

daily = seq(actual$Date[1], tail(actual$Date,1), by="day")

# Spline Interpolation

actual_daily <- data.frame(Date = daily)

actual_daily <- for (i in df){

    col <- names(df[i])
    x <- data.frame(Date = daily, 
                    col = spline(df[i], method = "fmm", xout = daily))
    
    actual_daily <- inner_join(actual_daily, x, by = Date)
    
}

# Example Data 

stocks <- tq_get("FB", get="stock.prices", from="2017-01-01", to="2017-05-01")

stocks <- tq_get("GOOG", get = "stock.prices", from = "1976-01-1", to = as.character(tail(actual$Date,1)))%>%
            mutate(qtr = as.yearqtr(date, format = "%Y Q%q"))%>%
            group_by(qtr)%>%
            summarise(avg_price = mean(close))%>%
            mutate(Date = as.Date(as.yearqtr(qtr, format = "%Y Q%q"), frac = 1))%>%
            select(-qtr)




df$Date <- as.Date(as.yearqtr(df$Date, format = "%Y Q%q"), frac = 1)


test <- inner_join(actual, stocks, by = "Date")%>%
            mutate(dif_price = avg_price - lag(avg_price))

out <- lm(dif_price ~ `Market Volatility Index (Level)` + `Real GDP growth`, test)


library(shiny)
runExample("01_hello")

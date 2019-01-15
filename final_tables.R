
### libraries and options:
### Options should be ran first
options(warn=-1)
options(java.parameters = "-Xmx8g" )
options(tibble.width = Inf)
options(tibble.print_max = 30)

library(lubridate)
library(dplyr)
library(tidyr)
library(ggrepel)






tables.path <- rstudioapi::getActiveProject()
tables.path <- paste0(tables.path, "/tables")





### function: Total number of Defected products for each day in each station
PinStationDaily <- function(data, start.date=NULL, end.date=NULL){
  if(is.null(start.date)){
    start.date<- ceiling_date(as.Date(min(data$date)), "day")
  }
  if(is.null(end.date)){
    end.date<- floor_date(as.Date(max(data$date)), "day")
  }
  data <- data %>% 
    filter(date >= start.date, date <= end.date) %>% 
    mutate(day = trunc((as.Date(date) - start.date)/ddays(1)) + 1)
  data.station.daily <- data %>% 
    group_by(station, day) %>% 
    summarise(total.produced= sum(produced),
              total.defected = sum(defected), 
              defected.ratio = total.defected/total.produced,
              datedate = floor_date(as.Date(date[1]), "day")) %>%
    arrange(datedate)
  return(data.station.daily)
}





### function: Total number of Defected products for each week in each station
PinStationWeekly <- function(data, start.date=NULL, end.date=NULL){
  if(is.null(start.date)){
    start.date<- ceiling_date(as.Date(min(data$date)), "week")
  }
  if(is.null(end.date)){
    end.date<- floor_date(as.Date(max(data$date)), "week") 
  }
  data <- data %>% 
    filter(date >= start.date, date < end.date) %>% 
    mutate(week = trunc((as.Date(date) - start.date)/dweeks(1))+1)
  data.station.weekly <- data %>% 
    group_by(station,week) %>% 
    summarise(total.produced= sum(produced),
              total.defected = sum(defected), 
              defected.ratio = total.defected/total.produced,
              datedate = floor_date(as.Date(date[1]), "week")) %>% 
    arrange(datedate)
  return(data.station.weekly)
}





#### function: Total number of Defected products for each month in each station
PinStationMonthly <- function(data, start.date=NULL, end.date=NULL){
  monnb <- function(d){ 
    lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon
  }
  mondf <- function(d1, d2){
    monnb(d2) - monnb(d1)
  }
  if(is.null(start.date)){
    start.date <- ceiling_date(as.Date(min(data$date)), "month")
  }
  if(is.null(end.date)){
    end.date <- floor_date(as.Date(max(data$date)), "month")
  }
  data <- data %>% 
    filter(date >= start.date, date<end.date) %>%
    mutate(month = mondf(as.Date(date),start.date))
  data.station.monthly <- data %>% group_by(station, month) %>% 
    summarise(total.produced= sum(produced),
              total.defected = sum(defected), 
              defected.ratio = total.defected/total.produced,
              datedate = floor_date(as.Date(date[1]), "month")) %>%
    arrange(datedate)
  return(data.station.monthly)
}





### Loading whole data brief and filtering them to 
### keep stations with more than 100 instances.
product.data.path<- paste0(tables.path, "/Data.Rds")
product.data<- readRDS(product.data.path)




### calculating Daily, Weekly, and Monthly pin saving data to 
### be used in control chart calculations.
daily.data <- PinStationDaily(product.data)
saveRDS(daily.data, file = paste0(tables.path,"/daily.data.Rds"))

weekly.data <- PinStationWeekly(product.data)
saveRDS(weekly.data, file = paste0(tables.path,"/weekly.data.Rds"))

monthly.data <- PinStationMonthly(product.data)
saveRDS(monthly.data, file = paste0(tables.path,"/monthly.data.Rds"))


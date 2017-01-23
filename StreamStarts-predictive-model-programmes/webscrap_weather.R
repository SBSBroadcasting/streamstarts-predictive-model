library(rvest)
library(data.table)
library('lubridate')
library(dplyr)

colnames(weather_data) <- c('Date','Temp','Rain')
months <- list('january','february','march','april','may','june','july','august','september','october','novemver','december')
nmonth <- c(1:12)
wresults <- list()
weather_data_hist <- wresults
weather_data_hist15 <- wresults

#get past weather
#2015
getHistoricalWeather15 <- function(x){
  date <- html_nodes(x,'tbody tr.pre th')
  temp <- html_nodes(x,'tbody tr.pre td:nth-child(2)')
  rain <- html_nodes(x,'tbody tr.pre td:nth-child(4)')
  html_date <- html_text(date)
  html_temp <- html_text(temp)
  html_rain <- html_text(rain)
  weather_data_hist15 <- data.table(Date= html_date,
                                  Temp= html_temp,
                                  Rain= html_rain)
  colnames(weather_data_hist15) <- c('Date','Temp','Rain') 
  return(weather_data_hist15) 
}

for(i in 1:12){
  htmlpage <- read_html(paste0("http://www.accuweather.com/en/nl/amsterdam/249758/",months[i],"-weather/249758?monyr=",nmonth[i],"/1/2015&view=table"))
  wresults[[i]] <- getHistoricalWeather(htmlpage)
  wresults[[i]]$Date <- gsub("[a-z]|[A-Z]","",wresults[[i]]$Date)
  wresults[[i]]$Date <- gsub(" UTC","",wresults[[i]]$Date)
  wresults[[i]]$Date <- as.list(wresults[[i]]$Date)
  wresults[[i]]$Temp <- as.list(wresults[[i]]$Temp)
  wresults[[i]]$Rain <- as.list(wresults[[i]]$Rain)
  wresults[[i]]$Date <- mdy(wresults[[i]]$Date)
  print(wresults[[i]])
  weather_data_hist15 <- rbind(weather_data_hist15,wresults[[i]])
}
weather_data_hist15 <- weather_data_hist15[!(is.na(weather_data_hist15$Date)),]

#2016  --- it needs to be edited every month to be consistent
getHistoricalWeather <- function(x){
  date <- html_nodes(x,'tbody tr.pre th')
  temp <- html_nodes(x,'tbody tr.pre td:nth-child(2)')
  rain <- html_nodes(x,'tbody tr.pre td:nth-child(4)')
  html_date <- html_text(date)
  html_temp <- html_text(temp)
  html_rain <- html_text(rain)
  weather_data_hist <- data.table(Date= html_date,
                                 Temp= html_temp,
                                 Rain= html_rain)
  colnames(weather_data_hist) <- c('Date','Temp','Rain') 
  return(weather_data_hist) 
}

for(i in 1:8){
  htmlpage <- read_html(paste0("http://www.accuweather.com/en/nl/amsterdam/249758/",months[i],"-weather/249758?monyr=",nmonth[i],"/1/2016&view=table"))
  wresults[[i]] <- getHistoricalWeather(htmlpage)
  wresults[[i]]$Date <- gsub("[a-z]|[A-Z]","",wresults[[i]]$Date)
  wresults[[i]]$Date <- gsub(" UTC","",wresults[[i]]$Date)
  wresults[[i]]$Date <- as.list(wresults[[i]]$Date)
  wresults[[i]]$Temp <- as.list(wresults[[i]]$Temp)
  wresults[[i]]$Rain <- as.list(wresults[[i]]$Rain)
  wresults[[i]]$Date <- mdy(wresults[[i]]$Date)
  print(wresults[[i]])
  weather_data_hist <- rbind(weather_data_hist,wresults[[i]])
}   
weather_data_hist <- weather_data_hist[!(is.na(weather_data_hist$Date)),]
weather_data <- rbind(weather_data_hist15, weather_data_hist)

#get future weather --- it needs to be edited every month to be consistent
getWeather <- function(x){
  date <- html_nodes(x,'tbody tr.lo.calendar-list-cl-tr th a')
  temp <- html_nodes(x,'tbody tr.lo.calendar-list-cl-tr td:nth-child(2)')
  rain <- html_nodes(x,'tbody tr.lo.calendar-list-cl-tr td:nth-child(4)')
  forecasthtml_date <- html_text(date)
  forecasthtml_temp <- html_text(temp)
  forecasthtml_rain <- html_text(rain)
  weather_data_for <- data.table(Date=forecasthtml_date,
                                 Temp=forecasthtml_temp,
                                 Rain=forecasthtml_rain)
  colnames(weather_data_for) <- c('Date','Temp','Rain') 
  return(weather_data_for) 
}

for(i in 8:9){
  htmlpage <- read_html(paste0("http://www.accuweather.com/en/nl/amsterdam/249758/",months[i],"-weather/249758?monyr=",nmonth[i],"/1/2016&view=table"))
  wresults[[i]] <- getWeather(htmlpage)
  wresults[[i]]$Date <- gsub("[a-z]|[A-Z]","",wresults[[i]]$Date)
  wresults[[i]]$Date <- gsub(" UTC","",wresults[[i]]$Date)
  wresults[[i]]$Date <- as.list(wresults[[i]]$Date)
  wresults[[i]]$Temp <- as.list(wresults[[i]]$Temp)
  wresults[[i]]$Rain <- as.list(wresults[[i]]$Rain)
  wresults[[i]]$Date <- mdy(wresults[[i]]$Date)
  #merge all historical and forecast data
  weather_data <- rbind(weather_data, wresults[[i]])
  weather_data$Temp <- gsub("Â°","",weather_data$Temp)
  weather_data$Rain <- gsub(" mm","",weather_data$Rain)
}

library(dplyr)
tw <- weather_data
tw$Date <- ymd(tw$Date)
tw$Temp <- gsub("°","",tw$Temp)
tw$Temp <- as.numeric(tw$Temp)
tw$Rain <- as.numeric(tw$Rain)
colnames(tw) <- c("Day","Temp", "Rain")

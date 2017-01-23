library(lubridate)
library(dplyr)
#this script contains the functionality of getting the programme name and retrieving a subset based on that programme from the dataset

#### online data ss ####
dataset <- rbind(read.csv2("sbs_progr_ss_20160107.csv", header = TRUE, sep = ";", dec= ".", quote = "\"", stringsAsFactors = FALSE),
                 read.csv2("sbs_progr_ss_20150107.csv", header = TRUE, sep = ";", dec= ".", quote = "\"", stringsAsFactors = FALSE),
                 read.csv2("sbs_progr_ss_20150812.csv", header = TRUE, sep = ";", dec= ".", quote = "\"", stringsAsFactors = FALSE),
                 read.csv2("sbs_progr_ss_20160708.csv", header = TRUE, sep = ";", dec= ".", quote = "\"", stringsAsFactors = FALSE)
)
dataset$ns_st_pr <- tolower(dataset$ns_st_pr)
dataset$Stream.starts <- gsub("\\.", "", dataset$Stream.starts)
dataset$Stream.starts <- as.numeric(dataset$Stream.starts)
dataset$Day <- dmy(dataset$Day)
dataset <- dataset[dataset$ns_st_pr!="advertentie",]
dataset$ns_st_pr <- gsub(" - seizoen \\d{1,2}$", "", dataset$ns_st_pr)
dataset$ns_st_pr <- gsub(" aflevering \\d{1,3}$", "", dataset$ns_st_pr)
dataset$ns_st_pr <- gsub(" aflevering \\d{1,3} en \\d{1,3}$", "", dataset$ns_st_pr)
dataset$ns_st_pr <- gsub(": aflevering \\d{1,3}$", "", dataset$ns_st_pr)
dataset <- dataset[(dataset$ns_st_pr!=" "),]

#### Import FUTURE SCHEDULE ####
futureSchedule <- read.csv2('FutureScheduling.csv', header = TRUE, sep = ";", dec = ",", quote = "\"", stringsAsFactors = FALSE )
futureSchedule <- futureSchedule[,c(1,3,4)]
futureSchedule$Product <- tolower(futureSchedule$Product)
futureSchedule$Platforms <- tolower(futureSchedule$Platforms)
futureSchedule$Platforms <- gsub("sbs 6","sbs6",futureSchedule$Platforms)
futureSchedule$Platforms <- gsub("sbs 9","sbs9",futureSchedule$Platforms)
futureSchedule$Platforms <- gsub("net 5","net5",futureSchedule$Platforms)
futureSchedule$Start.date <- dmy(futureSchedule$Start.date)

#### Import genres ####
genres <- read.csv2('genres1516.csv', header = TRUE, sep = ",", dec = ",", quote="\"", stringsAsFactors = FALSE)
genres <- genres[,-c(6,7)]
genres$Week <- gsub("^\\d{1,3}\\dw", '', genres$Week )
genres$Main.Title <- tolower(genres$Main.Title)
genres$Prog.Share <- as.numeric(genres$Prog.Share)
genres$Week <- as.numeric(genres$Week)
genres <- genres[!is.na(genres$Week),]
colnames(genres) <- c('week','year','programme','genre','share')
genres$year <- as.numeric(genres$year)

# #### Import promo campaigns data ####
# promo <- rbind(read.csv2('promo1516_1.csv', header=TRUE, sep = ";", quote = "\"", dec = ",", stringsAsFactors = FALSE),
#                read.csv2('promo1516_2.csv', header=TRUE, sep = ";", quote = "\"", dec = ",", stringsAsFactors = FALSE),
#                read.csv2('promo1516_3.csv', header=TRUE, sep = ";", quote = "\"", dec = ",", stringsAsFactors = FALSE),
#                read.csv2('promo1516_4.csv', header=TRUE, sep = ";", quote = "\"", dec = ",", stringsAsFactors = FALSE),
#                read.csv2('promo1516_5.csv', header=TRUE, sep = ";", quote = "\"", dec = ",", stringsAsFactors = FALSE)
#                )
# promo$Impressions <- gsub('\\.','', promo$Impressions)
# promo$Impressions <- as.numeric(promo$Impressions)
# promo$Date <- dmy(promo$Date)
# colnames(promo) <- c('Day','programme','impressions')
# promo$programme <- tolower(promo$programme)
# promo <- group_by(promo, Day, programme)
# promo <- summarize(promo, impressions=sum(impressions))
# 
# #TV promos
# tvPromo <- read.csv2('PromosTV_20160810.csv', header=TRUE, sep = ";", quote = "\"", dec = ",", stringsAsFactors = FALSE)
# tvPromo <- tvPromo[,c(2,6,9)]
# tvPromo$Date <- dmy(tvPromo$Date)
# tvPromo$Main.Title <- tolower(tvPromo$Main.Title)
# tvPromo$Share.A3. <- as.numeric(tvPromo$Share.A3.)
# tvPromo <- tvPromo[!grepl('promo',tvPromo$Main.Title),]
# colnames(tvPromo) <- c('Day','programme','share')
# tvPromo <- group_by(tvPromo, Day, programme)
# tvPromo <- summarize(tvPromo, share=mean(share))
# 

#### FILL here programme name or keyword of title #####
programme <- '' 

# #check in promo
# promoSubset <- promo[grepl(programme, promo$programme),]


#check in scheduling data for this keyword and take subset
futuresubset <- futureSchedule[grepl(programme, futureSchedule$Product),]
futuresubset$n <- 1
futuresubset <- group_by(futuresubset, Start.date)
futuresubset <- summarize(futuresubset, n=sum(n))
colnames(futuresubset) <- c('Day','Freq')

#check in online data for this keyword and take subset
subsetdata <- dataset[grepl(programme, dataset$ns_st_pr), c(1,4)]
subsetdata <- group_by(subsetdata, Day)
subsetdata <- summarize(subsetdata, Stream.starts=sum(Stream.starts))

#create dates columns
subsetdata$wday <- wday(subsetdata$Day)
subsetdata$mday <- mday(subsetdata$Day)
subsetdata$yday <- yday(subsetdata$Day)
subsetdata$week <- week(subsetdata$Day)
subsetdata$month <- month(subsetdata$Day)
subsetdata$year <- year(subsetdata$Day)

#### bind table with weather & futuredataset ####
subsetdata <- left_join(subsetdata, tw, by='Day')
subsetdata <- left_join(subsetdata, futuresubset, by='Day')
subsetdata <- mutate(subsetdata, Freq=ifelse(is.na(Freq)==TRUE, 0, Freq))

#### Join with genre table ####

#Make cases here: if it finds the programme in the list, otherwise in case of a new programme, fill the more likely genre of the programme
#and/or a similar programme 

genre1 <- NA
programme1 <- NA
genresubset <- genres[grepl(programme, genres$programme), c(1,2,5)]

if(is.na(genresubset)==TRUE){
  #ask for an AdEdge valid genre name SKO 2
  genre1 <- ""
  genresubset <- genres[grepl(genre1, genres$genre), c(1,2,5)]
  
  if(is.na(genre1)==TRUE){
    #ask for a similar programme name 
    programme1 <- ""
    genresubset <- genres[grepl(programme1, genres$programme), c(1,2,5)]
    
  }
}

subsetdata <- left_join(subsetdata, genresubset, by=c('week','year'))
subsetdata <- mutate(subsetdata, share=ifelse(is.na(share)==TRUE, 0, share))

#Fill weather data for 2015-11 (accuweather return a hole for those dates)
subsetdata <- mutate(subsetdata, Temp=ifelse(is.na(Temp)==TRUE, mean(!is.na(subsetdata$Temp)), Temp))
subsetdata <- mutate(subsetdata, Rain=ifelse(is.na(Rain)==TRUE, 0, Rain))

#### import holidays ####
total_hol <- rbind(read.csv2("holidays15.csv", header = TRUE, sep = ";", dec =",", quote = "\"", stringsAsFactors = FALSE), read.csv2("holidays16.csv", header = TRUE, sep = ";", dec =",", quote = "\"", stringsAsFactors = FALSE))
total_hol$Date <- ymd(total_hol$Date)
total_hol <- total_hol[,c(1,3)]
colnames(total_hol) <- c("Day","Holiday")
#bind
subsetdata <- left_join(subsetdata, total_hol, by="Day")
subsetdata <- mutate(subsetdata, Holiday=ifelse(is.na(Holiday)==TRUE, "NO", Holiday))
subsetdata <- unique(subsetdata)
subsetdata <- group_by(subsetdata, Day, Stream.starts, wday, mday, yday, week, month, year, Temp, Rain, Freq, Holiday)
subsetdata <- summarize(subsetdata, share = max(share))
subsetdata <- subsetdata[,c(1:11,13,12)]

#### create column NE for days from NewEpisodes ####
subsetdata$NE <- 0
pos <- 1

for(i in 2:nrow(subsetdata)){
  if(subsetdata$Freq[i] == 0 & subsetdata$NE[i] == 0 & subsetdata$NE[i-1] > 0){
    pos <- 1 + subsetdata$NE[i-1]
    subsetdata$NE[i] <- pos
  }else if(subsetdata$Freq[i] != 0){
    subsetdata$NE[i] <- 1
  }
}

#### get tw table and create future table ####
library(data.table)
tw1 <- tw[as.Date(tw$Day) >= "2016-08-01",] 
testDays <- unique(subsetdata[subsetdata$Day >='2016-08-01', c(1,2,11)])
futureTbl <- tw1
futureTbl <- right_join(futureTbl, testDays, by="Day")
tw1 <- tw[as.Date(tw$Day) >= "2016-08-15",] 
tw1$Stream.starts <- 0
tw1 <- as.data.frame(tw1)
tw1 <- left_join(tw1, futuresubset, by='Day')
futureTbl <- rbind(futureTbl,tw1) 
futureTbl$wday <- wday(futureTbl$Day)
futureTbl$mday <- mday(futureTbl$Day)
futureTbl$yday <- yday(futureTbl$Day)
futureTbl$week <- week(futureTbl$Day)
futureTbl$month <- month(futureTbl$Day)
futureTbl$year <- year(futureTbl$Day)
futureTbl <- right_join(total_hol,futureTbl)
for(i in 1:nrow(futureTbl)){
  if(is.na(futureTbl$Holiday[i])==TRUE){
    futureTbl$Holiday[i] <- "NO"
  }
}

futureTbl$share <- 0
futureTbl <- mutate(futureTbl, Temp=ifelse(is.na(Temp)==TRUE, 15, Temp))
futureTbl <- mutate(futureTbl, Rain=ifelse(is.na(Rain)==TRUE, 0, Rain))
futureTbl <- mutate(futureTbl, Freq=ifelse(is.na(Freq)==TRUE, 0, Freq))
futureTbl <- unique(futureTbl)
library(dplyr)
futureTbl <- futureTbl[, c(1,5,7:12,3:4,6,13,2)]
futureTbl$NE <- 0
pos <- 1

for(i in 2:nrow(futureTbl)){
  if(futureTbl$Freq[i] == 0 & futureTbl$NE[i] == 0 & futureTbl$NE[i-1] > 0){
    pos <- 1 + subsetdata$NE[i-1]
    futureTbl$NE[i] <- pos
  }else if(futureTbl$Freq[i] != 0){
    futureTbl$NE[i] <- 1
  }
}


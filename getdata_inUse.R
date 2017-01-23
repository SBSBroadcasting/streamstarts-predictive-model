library(lubridate)
library(dplyr)
# ####read  comscore online data for checking devices+demographics #### ####
# onlineDevStr <- rbind(read.csv2("ProgDevStr_20150103.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA"),
#                       read.csv2("ProgDevStr_20150406.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA"),
#                       read.csv2("ProgDevStr_20150709.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA"),
#                       read.csv2("ProgDevStr_20151012.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA"),
#                       read.csv2("ProgDevStr_20160103.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA"),
#                       read.csv2("ProgDevStr_20160405.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA"))                      
# ####read hollandOnline data #### ####
# online <- read.csv2("onlineProg15-16.csv", header = TRUE, sep = ",", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA")
# online <- online[,-c(12)]
# 

####read AdEdge tv data####
tvTotal1516 <- read.csv2("W:/R scripts/streamstarts_prediction/TvProg1516.csv", header = TRUE, sep = ",", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA")
tvTotal1516 <- tvTotal1516[,-c(8)]
tvTotal1516$TRP..avgW. <- as.numeric(tvTotal1516$TRP..avgW.)
tvTotal1516$Prog.Share <- as.numeric(tvTotal1516$Prog.Share)
tvTotal1516$Main.Title <- tolower(tvTotal1516$Main.Title)
tvTotal1516$Main.Title <- gsub(" ","",tvTotal1516$Main.Title)
tvTotal1516$Date <- ymd(tvTotal1516$Date)

####import holidays for 2015+2016 ####
hol15 <- read.csv2('holidays15.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
hol16 <- read.csv2('holidays16.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
hol15 <- hol15[,c(1,2,3,4)]
hol16 <- hol16[,c(1,2,3,4)]

#join holidays to totaltable
totalHol <- rbind(hol15, hol16)
totalHol$Date <- ymd(totalHol$Date)
tvTotal1516 <- left_join(tvTotal1516, totalHol, by="Date")
for(i in 1:nrow(tvTotal1516)){
  if(is.na(tvTotal1516$Holiday.name[i])==TRUE){
    tvTotal1516$Holiday.name[i] <- "NH"
  }
}
tvTotal1516 <- tvTotal1516[!(is.na(tvTotal1516$Date)),c(1:7,9)]

#####merge all kijk channels into one####
OnlineTotal$V2 <- gsub("kijk-app","kijk", OnlineTotal$V2)
OnlineTotal$V2 <- gsub("kijk-embed","kijk", OnlineTotal$V2)

#### Fix Titles #####
OnlineTotal$V4 <- gsub(" ","", OnlineTotal$V4)
OnlineTotal$V4 <- tolower(OnlineTotal$V4)
OnlineTotal$V4 <- gsub("\\'","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("%","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("-","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("\\.","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[+]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[!]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[?]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[#]","", OnlineTotal$V4)
OnlineTotal <- OnlineTotal[!grepl("aflevering", OnlineTotal$V4),]
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("seizoen",V4)==TRUE,substr(V4, 1, nchar(V4)-8),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^de",V4)==TRUE,substr(V4, 3, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^programmas",V4)==TRUE,substr(V4, 11, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("vod$",V4)==TRUE,substr(V4, 1, nchar(V4)-10),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("[&]",V4)==TRUE,"en",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("k2zoektk3",V4)==TRUE,"k3zoektk3",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("yayoga",V4)==TRUE,"yayoga",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("achtergeslotendeuren",V4)==TRUE,"achtergeslotendeuren",V4))

colnames(OnlineTotal) <- c("Date","Channel","Partner","Title","n")
OnlineTotal <- OnlineTotal[!(OnlineTotal$Title == ""),]
OnlineTotal <- OnlineTotal[!(OnlineTotal$Title == "_uimodalitemappviewcontroller"|OnlineTotal$Title=="_uialertshimpresentingviewcontroller"),]

#### group by & summarize by date & Channel ####
OnlineTotal <- group_by(OnlineTotal, Date, Channel, Title)
OnlineTotal <- summarise(OnlineTotal, n=sum(n))
OnlineTotal <- left_join(OnlineTotal, totalHol, by="Date")
for(i in 1:nrow(OnlineTotal)){
  if(is.na(OnlineTotal$Holiday.name[i])==TRUE){
    OnlineTotal$Holiday.name[i] <- "NH"
  }
}
OnlineTotal <- OnlineTotal[,c(1,2,3,4,6)]

####Set genreTbl for genre matching#####
genreTbl <- subset(tvTotal1516, select = c(3,4))
genreTbl <- group_by(genreTbl, Main.Title, Type..Subject.3..SKO.)
genreTbl <- summarise(genreTbl, n=n())
genreTbl <- unique(genreTbl)
genreTbl <- genreTbl[,-c(3)]
colnames(genreTbl) <- c("Title","Type..Subject.3..SKO.")

####Match/join tv titles wth video title programmes and fill the genres ####
onlineMatch <- merge(OnlineTotal, genreTbl, by = "Title", all = TRUE)
onlineMatch <- mutate(onlineMatch,Type..Subject.3..SKO.=ifelse(grepl("film",Title)==TRUE,"BTL FILMS",Type..Subject.3..SKO.))
onlineMatch <- mutate(onlineMatch,Type..Subject.3..SKO.=ifelse(grepl("25",Title)==TRUE,"Overig amusement",Type..Subject.3..SKO.))
onlineMatch <- mutate(onlineMatch,Type..Subject.3..SKO.=ifelse(grepl("20",Title)==TRUE,"Overig amusement",Type..Subject.3..SKO.))
onlineMatch <- mutate(onlineMatch,Type..Subject.3..SKO.=ifelse(grepl("veronicamusic",Title)==TRUE,"OVERIGE NON FICTIE",Type..Subject.3..SKO.))
onlineMatch <- mutate(onlineMatch,Type..Subject.3..SKO.=ifelse(grepl("achtergeslotendeuren",Title)==TRUE,"NLD SERIES",Type..Subject.3..SKO.))
onlineMatch <- mutate(onlineMatch,Type..Subject.3..SKO.=ifelse(grepl("hartvannederlandlaat",Title)==TRUE,"Actualiteiten",Type..Subject.3..SKO.))
onlineMatch <- mutate(onlineMatch,Type..Subject.3..SKO.=ifelse(grepl("hartvannederlandvroeg",Title)==TRUE,"Actualiteiten",Type..Subject.3..SKO.))

#### count total first episodes on linear and transform the count +1day to digital #### 
##---- > to create columns for every genre vs NE --- Add the part from old script: rewrite the for loops with mutate()
tvTotal1516$yday <- yday(tvTotal1516$Date)
tvTotal1516$year <- year(tvTotal1516$Date)
tvTotal1516$wday <- wday(tvTotal1516$Date)
tvTotal1516$week <- week(tvTotal1516$Date)
tvTotal1516$mday <- mday(tvTotal1516$Date)
tvTotal1516$month <- month(tvTotal1516$Date)
tvTotal1516 <- mutate(tvTotal1516,NEL=ifelse(Type..Frequency=="EERSTE UITZENDING",1,0)) #mark the actual day for linear
tvTotal1516 <- mutate(tvTotal1516,NENL=ifelse(!(is.na(yday)==TRUE),0,0))                #mark the actual day for non linear
tvTotal1516 <- group_by(tvTotal1516, Date, Channel,Main.Title, Type..Subject.3..SKO., Holiday.name, yday, year, wday, week, mday, month)
tvTotal1516 <- summarize(tvTotal1516, 
                         TRP..avgW.=sum(TRP..avgW.),
                         Prog.Share=mean(Prog.Share),
                         NEL=sum(NEL),
                         NENL=sum(NENL))
##### count the total number of genres broadcasted per day/channel #####
counterGenre <- unique(tvTotal1516[,c(1,2,4)])
counterGenre <- group_by(counterGenre, Date, Channel, Type..Subject.3..SKO.)
counterGenre <- summarize(counterGenre, n=n())
counterGenre <- group_by(counterGenre, Date, Channel)
counterGenre <- summarize(counterGenre, n=sum(n))
#### Insert count of genres into tvTotal1516 table ####
tvTotal1516 <- left_join(tvTotal1516, counterGenre, by=c("Date","Channel"))
#tvTotal1516 <- tvTotal1516[,c(1,2,4:15)]
tvTotal1516 <- group_by(tvTotal1516, Date, Channel,Main.Title, Holiday.name, yday, year, wday, week, mday, month, n)
tvTotal1516 <- summarize(tvTotal1516, 
                         TRP..avgW.=sum(TRP..avgW.),
                         Prog.Share=mean(Prog.Share),
                         NEL=sum(NEL),
                         NENL=sum(NENL))

#### take out main title and sum NE variables in another table ####
tvTotal1516_NE <- group_by(tvTotal1516, Date, Channel, Holiday.name, yday, year, wday, week, mday, month, n)
tvTotal1516_NE <- summarize(tvTotal1516_NE, 
                        TRP..avgW.=sum(TRP..avgW.),
                         Prog.Share=mean(Prog.Share),
                         NEL=sum(NEL),
                         NENL=sum(NENL))
#### next day transferring info #####
pday <- 0
count <- 0
channel <- ""
year <- ""
for(i in 1:nrow(tvTotal1516_NE)){
  if(!is.na(tvTotal1516_NE$NEL[i])){
    pday <- tvTotal1516_NE$yday[i] + 1
    count <- tvTotal1516_NE$NEL[i]
    channel <- tvTotal1516_NE$Channel[i]
    year <- tvTotal1516_NE$year[i]
  }
  for(j in 1:nrow(tvTotal1516_NE)){
    if((tvTotal1516_NE$Channel[j] == channel)&(tvTotal1516_NE$yday[j] == pday)&(tvTotal1516_NE$year[j] == year)){
      print(paste("channel found:",channel))
      print(paste("next day found:",pday))
      print(paste("year found:",year))
      tvTotal1516_NE$NENL[j] <- count
      print(paste("inserted:", count))
    }
  }
}

tvTotal1516 <- left_join(tvTotal1516[,-c(14,15)],tvTotal1516_NE[,c(1,2,13,14)],by=c("Date","Channel"))  

# TODO check how to connect with online data
colnames(OnlineTotal) <- c("Date","Channel","Main.Title","ss","Holiday.name")
OnlineTotal <- left_join(OnlineTotal,tvTotal1516,by=c("Date","Main.Title"))                 

#### alternative matching TV variables with online ####
tvALT <- tvTotal1516[,c(3,12,13)]
tvALT <- group_by(tvALT,Main.Title)
tvALT <- summarize(tvALT,TRP..avgW.=mean(TRP..avgW.),Prog.Share=mean(Prog.Share))

tvALT_dateNENL <- tvTotal1516[,c(1,2,4:11,15)]
tvALT_dateNENL <- group_by(tvALT_dateNENL,Date,Channel,Holiday.name,yday,year,mday,month,week,n)
tvALT_dateNENL <- summarize(tvALT_dateNENL,NENL=sum(NENL))

#Match tvALT with onlineTotal
merged_1 <- left_join(OnlineTotal,tvALT, by='Main.Title')
merged_1$Channel <- gsub(' ',"",merged_1$Channel)
merged_1$Channel <- tolower(merged_1$Channel)
merged_1$Channel <- gsub('veronicatv',"veronica",merged_1$Channel)
tvALT_dateNENL$Channel <- gsub(' ',"",tvALT_dateNENL$Channel)
tvALT_dateNENL$Channel <- tolower(tvALT_dateNENL$Channel)

#Match tvALT with onlineTotal
merged_2 <- left_join(merged_1,tvALT_dateNENL, by=c('Date','Channel','Holiday.name'))

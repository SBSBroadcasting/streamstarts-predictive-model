totalStreamData1 <- read.csv2('strstarts15_sep-dec.csv',header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
totalStreamData2 <- read.csv2('strstarts15_jun-aug.csv',header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
totalStreamData3 <- read.csv2('strstarts16.csv',header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
totalStreamData4 <- read.csv2('strstarts15_mar-may.csv',header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
totalStreamData5 <- read.csv2('strstarts15_jan-feb.csv',header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")

totalStreamData <- rbind(totalStreamData1,totalStreamData2,totalStreamData3,totalStreamData4,totalStreamData5)
totalStreamData$Title <- tolower(totalStreamData$Title)
totalStreamData$Title <- gsub("[+]","",totalStreamData$Title)
totalStreamData$Title <- gsub("[-]","",totalStreamData$Title)
totalStreamData$Title <- gsub("[#]","",totalStreamData$Title)
totalStreamData$Title <- gsub("[:]", "",totalStreamData$Title)
totalStreamData$Title <- gsub(" seizoen +\\d+","",totalStreamData$Title)
totalStreamData$Title <- gsub("\\d+[.]", "",totalStreamData$Title)
totalStreamData$Title <- gsub("^de ", "",totalStreamData$Title)
totalStreamData$Title <- gsub("^the ", "",totalStreamData$Title)
totalStreamData$Title <- gsub("kevin%27sgranddesigns", "kevins grand design",totalStreamData$Title)
totalStreamData$Title <- gsub("aflevering n +\\d+", "",totalStreamData$Title)
totalStreamData$Title <- gsub("[?]", "",totalStreamData$Title)
totalStreamData$Title <- gsub("[!]", "",totalStreamData$Title)
totalStreamData$Title <- gsub("[&]", "and",totalStreamData$Title)
totalStreamData$Title <- gsub("[...]", "",totalStreamData$Title)
totalStreamData$Title <- gsub("programmas.hartvannederland.+[[:alpha:]]+.vod", "hart van nederland",totalStreamData$Title)
totalStreamData <- totalStreamData[!totalStreamData$Title =="film",]
totalStreamData$Title <- gsub(" ","",totalStreamData$Title)

write.csv2(tvTotal1516,"tvTotal1516.csv")
#totalStreamData <- read.csv2("totalStreamData.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")

#read tv data
library(lubridate)
library(dplyr)
tvTotal1516 <- read.csv2('//sbs/dfs-rootshare/Users/Konstantina Mavridou/R scripts/R_scripts_Konstantina/Programmes_20160317_162316.csv', header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
tvTotal1516 <- tvTotal1516[,-c(9)]
tvTotal1516 <- tvTotal1516[!(tvTotal1516$Channel=="Total"),]
tvTotal1516$Date <- as.character(tvTotal1516$Date)
tvTotal1516$Date <- gsub(" UTC","", tvTotal1516$Date)
tvTotal1516$Channel <- as.character(tvTotal1516$Channel)
tvTotal1516$TRP..avgW. <- as.numeric(tvTotal1516$TRP..avgW.)
tvTotal1516$Prog.Share <- as.numeric(tvTotal1516$Prog.Share)
tvTotal1516$Main.Title <- tolower(tvTotal1516$Main.Title)
tvTotal1516$Main.Title <- gsub(" ","",tvTotal1516$Main.Title)

#import holidays for 2015+2016
hol15 <- read.csv2('holidays15.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
hol16 <- read.csv2('holidays16.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
hol15 <- hol15[,c(1,2,3,4)]
hol16 <- hol16[,c(1,2,3,4)]

#join holidays to totaltable
totalHol <- rbind(hol15, hol16)
totalHol$Date <- as.POSIXct(totalHol$Date)
tvTotal1516$Date <- as.POSIXct(tvTotal1516$Date)
tvTotal1516_2 <- left_join(tvTotal1516, totalHol, by="Date")
#group tv data subest by channel and tv rating
tvTotal1516_1 <- group_by(tvTotal1516_2, Date, Channel, Holiday.name)
tvTotal1516_1 <- summarise(tvTotal1516_1, n=mean(TRP..avgW.))
tvTotal1516_1$n <- format(tvTotal1516_1$n, digits = 2)
for(i in 1:nrow(tvTotal1516_1)){
  if(is.na(tvTotal1516_1$Holiday.name[i])==TRUE){
    tvTotal1516_1$Holiday.name[i] <- "NH"
  }
}

partition1 <- read.csv2("totalStreamData.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
partition1 <- subset(partition1, select = c(2,3,6))

#merge all kijk channels into one
partition1$sourcesite <- gsub("kijk-app","kijk", partition1$sourcesite)
partition1$sourcesite <- gsub("kijk-embed","kijk", partition1$sourcesite)
partition1$Day <- as.POSIXct(partition1$Day, format = "%d-%m-%Y")
colnames(partition1) <- c("Date","sourcesite","n")
partition1$sourcesite <- as.character(partition1$sourcesite)
partition1$n <- as.numeric(partition1$n)
partition1 <- partition1[!is.na(partition1$Date),]
#group by & summarize by day & sourcesite
partition1 <- group_by(partition1, Date, sourcesite)
partition1 <- summarise(partition1, n=sum(n))
partition1 <- left_join(partition1, totalHol, by="Date")
for(i in 1:nrow(partition1)){
  if(is.na(partition1$Holiday.name[i])==TRUE){
    partition1$Holiday.name[i] <- "NH"
  }
}
#Set genreTbl for genre matching
genreTbl <- subset(tvTotal1516, select = c(4,6))
genreTbl$Type..Subject.4..SKO. <- as.character(genreTbl$Type..Subject.4..SKO.)
genreTbl <- group_by(genreTbl, Main.Title, Type..Subject.4..SKO.)
genreTbl <- summarise(genreTbl, n=n())
genreTbl <- unique(genreTbl)
genreTbl <- genreTbl[,-c(3)]
colnames(genreTbl) <- c("Title","Type..Subject.4..SKO.")

#Match/join tv titles wth video title programmes and fill the genres
totalStreamData_cm <- merge(totalStreamData, genreTbl, by = "Title")
totalStreamData_cm$Day <- dmy(as.character(totalStreamData_cm$Day))
totalStreamData_cm$sourcesite <- as.character(totalStreamData_cm$sourcesite)
totalStreamData_cm$Stream.starts <- gsub("[.]","",totalStreamData_cm$Stream.starts)
totalStreamData_cm$Stream.starts <- as.numeric(totalStreamData_cm$Stream.starts)

totalStreamData_ncm <- merge(totalStreamData, genreTbl, by = "Title", all = TRUE)
totalStreamData_ncm <- totalStreamData_ncm[!(totalStreamData_ncm$Title == ""),]
totalStreamData_ncm$Day <- dmy(as.character(totalStreamData_ncm$Day))
totalStreamData_ncm$sourcesite <- as.character(totalStreamData_ncm$sourcesite)
totalStreamData_ncm$Stream.starts <- gsub("[.]","",totalStreamData_ncm$Stream.starts)
totalStreamData_ncm$Stream.starts <- as.numeric(totalStreamData_ncm$Stream.starts)

for(i in row(totalStreamData_ncm)){
  if(totalStreamData_ncm$Title[i] == "veronicamusic"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Overige non fictie"
  }
  if(totalStreamData_ncm$Title[i] == "achtergeslotendeuren"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Nld series drama"
  }
  if(totalStreamData_ncm$Title[i] =="bagelsenbubbels" ){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Nld series: overig"
  }
  if(totalStreamData_ncm$Title[i] == "debuitenlandsedroom"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Wonen/interieurs/tuin/doe het zelf"
  }
  if(totalStreamData_ncm$Title[i] == "decoassistent"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Nld series (Sit) comedy"
  }
  if(totalStreamData_ncm$Title[i] == "degeheimewereldvan"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Overige non fictie"
  }
  if(totalStreamData_ncm$Title[i] == "degevaarlijkstewegenvannederland"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Auto/motor/boot/fiets/verkeer"
  }
  if(totalStreamData_ncm$Title[i] == "degroteherkansing"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Gezondheid/lifestyle/opvoeding/coaching"
  }
  if(totalStreamData_ncm$Title[i] == "deleukstewebsnacks"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "Overig amusement"
  }
  if(totalStreamData_ncm$Title[i] == "delijfshow"){
    totalStreamData_ncm$Type..Subject.4..SKO.[i] <- "SPEL & QUIZ"
  }
}
write.csv2(totalStreamData_ncm,"totalStreamData_ncm.csv")

#Group by genre and sum strstarts
onlineGenreSum <- group_by(totalStreamData_cm, Day, Type..Subject.4..SKO.)
onlineGenreSum <- summarise(onlineGenreSum, n=sum(Stream.starts))
#Group tv and sum
tvGenreSum <- group_by(tvTotal1516, Date, Type..Subject.4..SKO.)
tvGenreSum <- summarise(tvGenreSum, n=mean(TRP..avgW.))

#test table as ratio/difference btw online and tv ____ to be fixed!
test <- onlineGenreSum[sample(nrow(onlineGenreSum), 6109), ]
tvGenreSum1 <- tvGenreSum
colnames(tvGenreSum1) <- c("Day", "Type..Subject.4..SKO.", "n")
tvGenreSum1$Type..Subject.4..SKO. <- as.character(tvGenreSum1$Type..Subject.4..SKO.)
testMod <- left_join(tvGenreSum1, test, by=c("Day","Type..Subject.4..SKO."))
testMod$n <- tvGenreSum1$n/test$n
testMod <- testMod[,c(1,2,5)]

#create seasonal boxplots http://stackoverflow.com/questions/12052305/what-is-the-most-elegant-way-to-split-data-and-produce-seasonal-boxplots
library(plotly)
#plot the ratio across tv-online on genres
plot_ly(data = testMod, x = testMod$Day, y = testMod$n, color = testMod$Type..Subject.4..SKO., mode = "markers")

#plot time series stremstarts across channels
pal <- RColorBrewer::brewer.pal(nlevels(partition1$sourcesite), "Set2")
p0 <- plot_ly(data = partition1, x = partition1$Date, y = partition1$n, color = partition1$sourcesite , hoverinfo = "all",
              text = partition1$Holiday.name,
              colors = pal, mode = "line")
p0
#plot tv data series
pal1 <- RColorBrewer::brewer.pal(nlevels(tvTotal1516_1$Channel), "Set2")
p1 <- plot_ly(data = tvTotal1516_1, x = tvTotal1516_1$Date, y = tvTotal1516_1$n, color = tvTotal1516_1$Channel, hoverinfo = "all",
              text = tvTotal1516_1$Holiday.name,
              colors = pal1, mode = "line")
p1
#plot online genres
pal2 <- RColorBrewer::brewer.pal(38, "Paired")
p2 <- plot_ly(data = onlineGenreSum, x = onlineGenreSum$Day, y = onlineGenreSum$n, color = onlineGenreSum$Type..Subject.4..SKO.,
              colors = pal2, mode = "markers", size = onlineGenreSum$n)
#plot tv genres
pal3 <- RColorBrewer::brewer.pal(38, "Paired")
p3 <- plot_ly(data = tvGenreSum, x = tvGenreSum$Date, y = tvGenreSum$n, color = tvGenreSum$Type..Subject.4..SKO.,
              colors = pal3, mode = "markers", size = tvGenreSum$n)

p2
p3


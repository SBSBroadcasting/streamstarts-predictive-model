library(lubridate)
library(dplyr)
#### read online data #####
onlinecomscore1 <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/datasets_comscore/201501_201503.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
onlinecomscore2 <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/datasets_comscore/201504_201506.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
onlinecomscore3 <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/datasets_comscore/201507_201509.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
onlinecomscore4 <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/datasets_comscore/201510_201512.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
onlinecomscore5 <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/datasets_comscore/201601_20160512.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
onlinecomscore <- rbind(onlinecomscore1,onlinecomscore2, onlinecomscore3, onlinecomscore4, onlinecomscore5)
onlinecomscore$Stream.starts <- as.character(onlinecomscore$Stream.starts)
onlinecomscore$Stream.starts <- gsub("\\.","",onlinecomscore$Stream.starts)
onlinecomscore$Stream.starts <- as.numeric(onlinecomscore$Stream.starts)
onlinecomscore$sourcesite <- as.character(onlinecomscore$sourcesite)
onlinecomscore$sbs_partner <- as.character(onlinecomscore$sbs_partner)
onlinecomscore$Day <- as.character(onlinecomscore$Day)
onlinecomscore$Day <- dmy(onlinecomscore$Day)
onlinecomscore$Title <- as.character(onlinecomscore$Title)

#update <- read.delim(unzipped, header = FALSE, sep = "\t", quote = "\"",
#                     dec = ",")
#colnames(update) <- c("Day","sourcesite","sbs_partner","Title","Stream.starts")
#update$Day <- as.character(update$Day)
#update$Day <- dmy(update$Day)
#update$Title <- as.character(update$Title)
#update$sourcesite <- as.character(update$sourcesite)
#update$sbs_partner <- as.character(update$sbs_partner)

#update the historical data with today's feed
#onlinecomscore <- rbind(onlinecomscore,update)

write.csv2(onlinecomscore,"onlinecomscore.csv")

onlinecomscore <- read.csv2("onlinecomscore.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
#### subset  online data ####
totalStreamData_3 <- onlinecomscore
totalStreamData_3 <- subset(totalStreamData_3,Title !="Advertentie", select = c(2,3,4,5,6))
####read tv data####
tvTotal1516_3 <- read.csv2('W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/Programmes predictiveModel_20160512_115825.csv', header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
tvTotal1516_3 <- tvTotal1516_3[,-c(8)]
#tvTotal1516_3$Date <- gsub(" UTC","", tvTotal1516_3$Date)
tvTotal1516_3$Date <- as.character(tvTotal1516_3$Date)
tvTotal1516_3$Channel <- as.character(tvTotal1516_3$Channel)
tvTotal1516_3$TRP..avgW. <- as.numeric(tvTotal1516_3$TRP..avgW.)
tvTotal1516_3$Prog.Share <- as.numeric(tvTotal1516_3$Prog.Share)
tvTotal1516_3$Main.Title <- tolower(tvTotal1516_3$Main.Title)
tvTotal1516_3$Main.Title <- gsub(" ","",tvTotal1516_3$Main.Title)
tvTotal1516_3$Date <- ymd(tvTotal1516_3$Date)

write.csv2(tvTotal1516_3,"tvTotal1516_3.csv")

#### import holidays for 2015+2016 ####
hol15 <- read.csv2('holidays15.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
hol16 <- read.csv2('holidays16.csv', header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
hol15 <- hol15[,c(1,2,3,4)]
hol16 <- hol16[,c(1,2,3,4)]

#join holidays to totaltable
totalHol <- rbind(hol15, hol16)
totalHol$Date <- as.character(totalHol$Date)
totalHol$Date <- gsub(" CET","", totalHol$Date)
totalHol$Date <- ymd(totalHol$Date)

#tvTotal1516_3 <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/tvTotal1516_3.csv", header = TRUE, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
#tvTotal1516_3$Date <- as.character(tvTotal1516_3$Date)
#tvTotal1516_3 <- tvTotal1516_3[,-c(1)]
#tvTotal1516_3$Date <- as.character(tvTotal1516_3$Date)
#tvTotal1516_3$Date <- dmy(tvTotal1516_3$Date)
tvTotal1516_3_2 <- left_join(tvTotal1516_3, totalHol)
tvTotal1516_3_2$TRP..avgW. <- as.character(tvTotal1516_3_2$TRP..avgW.)
tvTotal1516_3_2$TRP..avgW. <- gsub(",",".",tvTotal1516_3_2$TRP..avgW.)
tvTotal1516_3_2$TRP..avgW. <- as.numeric(tvTotal1516_3_2$TRP..avgW.)
tvTotal1516_3_2$TRP..avgW. <- signif(tvTotal1516_3_2$TRP..avgW., digits = 2)

tvTotal1516_3$TRP..avgW. <- as.character(tvTotal1516_3$TRP..avgW.)
tvTotal1516_3$TRP..avgW. <- gsub(",",".",tvTotal1516_3$TRP..avgW.)
tvTotal1516_3$TRP..avgW. <- as.numeric(tvTotal1516_3$TRP..avgW.)
tvTotal1516_3$TRP..avgW. <- signif(tvTotal1516_3$TRP..avgW., digits = 2)

#### group tv data subest by channel and tv rating ####
tvTotal1516_3_1 <- group_by(tvTotal1516_3_2, Date, Channel, Holiday.name)
tvTotal1516_3_1 <- summarise(tvTotal1516_3_1, n=mean(TRP..avgW.))
tvTotal1516_3_1$n <- format(tvTotal1516_3_1$n, digits = 2)
for(i in 1:nrow(tvTotal1516_3_1)){
  if(is.na(tvTotal1516_3_1$Holiday.name[i])==TRUE){
    tvTotal1516_3_1$Holiday.name[i] <- "NH"
  }
}

partition1 <- read.csv2("totalStreamData.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "", stringsAsFactors=FALSE)
partition1 <- subset(partition1, select = c(2,3,6))

####merge all kijk channels into one####
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

#####Set genreTbl_3_3 for genre matching####
genreTbl_3 <- subset(tvTotal1516_3, select = c(3,4))
genreTbl_3$Type..Subject.3..SKO. <- as.character(genreTbl_3$Type..Subject.3..SKO.)
genreTbl_3 <- group_by(genreTbl_3, Main.Title, Type..Subject.3..SKO.)
genreTbl_3 <- summarise(genreTbl_3, n=n())
genreTbl_3 <- unique(genreTbl_3)
genreTbl_3 <- genreTbl_3[,-c(3)]
colnames(genreTbl_3) <- c("Title","Type..Subject.3..SKO.")

####Match/join tv titles wth video title programmes and fill the genres ####
totalStreamData_3 <- totalStreamData_3[!totalStreamData_3$Title=="_uialertshimpresentingviewcontroller",]
totalStreamData_3$Title <- gsub("[']","",totalStreamData_3$Title)
totalStreamData_3$Title <- gsub('["]',"",totalStreamData_3$Title)
totalStreamData_3$Title <- gsub('[%]',"",totalStreamData_3$Title)
totalStreamData_3$Title <- gsub('[%]+e',"",totalStreamData_3$Title)
totalStreamData_3$Title <- gsub("7het0was0net0een0tsunami0die0op0me0af0kwam7","hetwasneteentsunamidieopmeafkwam",totalStreamData_3$Title)

totalStreamData_3_cm <- merge(totalStreamData_3, genreTbl_3, by = "Title")
totalStreamData_3_cm$Day <- as.character(totalStreamData_3_cm$Day)
totalStreamData_3_cm$Day <- ymd(totalStreamData_3_cm$Day)
totalStreamData_3_cm$sourcesite <- as.character(totalStreamData_3_cm$sourcesite)
totalStreamData_3_cm$Stream.starts <- gsub("\\.","",totalStreamData_3_cm$Stream.starts)
totalStreamData_3_cm$Stream.starts <- as.numeric(totalStreamData_3_cm$Stream.starts)

totalStreamData_3$Stream.starts <- gsub("\\.","",totalStreamData_3$Stream.starts)
totalStreamData_3$Stream.starts <- as.numeric(totalStreamData_3$Stream.starts)

totalStreamData_3_ncm <- merge(totalStreamData_3, genreTbl_3, by = "Title", all = TRUE)
totalStreamData_3_ncm <- totalStreamData_3_ncm[!(totalStreamData_3_ncm$Title == ""),]
totalStreamData_3_ncm$Day <- ymd(as.character(totalStreamData_3_ncm$Day))
totalStreamData_3_ncm$sourcesite <- as.character(totalStreamData_3_ncm$sourcesite)
totalStreamData_3_ncm$Stream.starts <- gsub("[.]","",totalStreamData_3_ncm$Stream.starts)
totalStreamData_3_ncm$Stream.starts <- as.numeric(totalStreamData_3_ncm$Stream.starts)

####Adjust na values with existing tv genres####
for(i in 1:nrow(totalStreamData_3_ncm)){
  if(totalStreamData_3_ncm$Title[i] == "bc"){
    totalStreamData_3_ncm$Title[i] <- "10000bc"
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "BTL FILMS"
  }
  if(totalStreamData_3_ncm$Title[i] == "veronicamusic"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Overige non fictie"
  }
  if(totalStreamData_3_ncm$Title[i] == "achtergeslotendeuren"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Nld series"
  }
  if(totalStreamData_3_ncm$Title[i] =="bagelsenbubbels" ){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Nld series"
  }
  if(totalStreamData_3_ncm$Title[i] == "debuitenlandsedroom"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "OVERIGE NON FICTIE"
  }
  if(totalStreamData_3_ncm$Title[i] == "decoassistent"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Nld series"
  }
  if(totalStreamData_3_ncm$Title[i] == "degeheimewereldvan"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Overige non fictie"
  }
  if(totalStreamData_3_ncm$Title[i] == "degevaarlijkstewegenvannederland"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "OVERIGE NON FICTIE"
  }
  if(totalStreamData_3_ncm$Title[i] == "degroteherkansing"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "OVERIGE NON FICTIE"
  }
  if(totalStreamData_3_ncm$Title[i] == "deleukstewebsnacks"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Overig amusement"
  }
  if(totalStreamData_3_ncm$Title[i] == "delijfshow"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "SPEL & QUIZ"
  }
  if(totalStreamData_3_ncm$Title[i] == "hartvannederlandlaat"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Actualiteiten"
  }
}
for(i in 1:nrow(totalStreamData_3_ncm)){
  if(totalStreamData_3_ncm$Title[i] == "hartvannederlandlaat"|totalStreamData_3_ncm$Title[i] == "hartvannederlandvroeg"){
    totalStreamData_3_ncm$Type..Subject.3..SKO.[i] <- "Actualiteiten"
  }
}

write.csv2(totalStreamData_3_ncm,"totalStreamData_3_ncm.csv")
#Group by genre and sum strstarts
onlineGenreSum_3 <- group_by(totalStreamData_3_cm, Day, Type..Subject.3..SKO.)
onlineGenreSum_3 <- summarise(onlineGenreSum_3, n=sum(Stream.starts))
#Group tv and sum
tvGenreSum_3 <- group_by(tvTotal1516_3, Date, Type..Subject.3..SKO.)
tvGenreSum_3 <- summarise(tvGenreSum_3, n=mean(TRP..avgW.))

#### Match new episodes dates with online date ####
totalStreamData_3_ncm <- read.csv2("totalStreamData_3_ncm.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
totalStreamData_3_ncm <- totalStreamData_3_ncm[,-c(1)]
colnames(totalStreamData_3_ncm) <- c("Title","Date","sourcesite","sbs_partner","Stream.starts","Type..Subject.3..SKO.")
colnames(tvTotal1516_3) <- c("Date","Channel","Title","Type..Subject.2..SKO.","Freq","TRP..avgw.","Prog.Share")
tvTotal1516_3$Title <- as.character(tvTotal1516_3$Title)
totalStreamData_3_ncm$mday <- mday(totalStreamData_3_ncm$Date)
totalStreamData_3_ncm$wday <- wday(totalStreamData_3_ncm$Date)
totalStreamData_3_ncm$yday <- yday(totalStreamData_3_ncm$Date)
totalStreamData_3_ncm$month <- month(totalStreamData_3_ncm$Date)
totalStreamData_3_ncm$`day+1` <- totalStreamData_3_ncm$yday+1
totalStreamData_3_ncm$Title <- as.character(totalStreamData_3_ncm$Title)
totalStreamData_3_ncm$Date <- as.character(totalStreamData_3_ncm$Date)
totalStreamData_3_ncm$Date <- ymd(totalStreamData_3_ncm$Date)

totalStreamData_3_ncm <- left_join(totalStreamData_3_ncm, tvTotal1516_3, by=c("Date","Title"))
# [1] <NA>                               BTL FILMS                          OVERIGE NON FICTIE                 Overig amusement                  
#[5] Nld series                         BTL SERIES                         Talentenjacht/Auditie/Programma    KINDERFILMS                       
#[9] NLD SERIES                         Overige non fictie                 SPEL & QUIZ                        NLD FILMS                         
#[13] Actualiteiten                      Nieuws                             Overige muziek: Programa           Overige muziek: Live registratie  
#[17] Voetbalreportage                   Overige muziek: overig             Satire                             Weerbericht                       
#[21] Populaire muziek: Live registratie Populaire muziek: Videoclips       Actuele sportinformatie            Cabaret/kleinkunst                
#[25] Overige sportreportage             Overige sportinformatie            Tekstuele informatie  


totalStreamData_3_ncm$F_BTL_FILMS <- 0
totalStreamData_3_ncm$F_OVERIGE_NON_FICTIE <- 0
totalStreamData_3_ncm$F_Overig_amusement <- 0
totalStreamData_3_ncm$F_TAP <- 0
totalStreamData_3_ncm$F_KINDERFILMS <- 0
totalStreamData_3_ncm$F_Actualiteiten <- 0
totalStreamData_3_ncm$F_Nieuws <- 0
totalStreamData_3_ncm$F_OM_Programa <- 0
totalStreamData_3_ncm$F_OM_Live_registratie <- 0
totalStreamData_3_ncm$F_SPEL_QUIZ <- 0
totalStreamData_3_ncm$F_NLD_FILMS <- 0 
totalStreamData_3_ncm$F_NLD_SERIES <- 0
totalStreamData_3_ncm$F_BTL_SERIES <- 0
totalStreamData_3_ncm$F_Nld_series <- 0
totalStreamData_3_ncm$F_Voetbalreportage <- 0
totalStreamData_3_ncm$F_OM_overig <- 0
totalStreamData_3_ncm$F_Satire <- 0
totalStreamData_3_ncm$F_PM_Live_registratie <- 0
totalStreamData_3_ncm$F_PM_Videoclips <- 0
totalStreamData_3_ncm$F_Actuele_sportinformatie <- 0
totalStreamData_3_ncm$F_Cabaret_kleinkunst <- 0
totalStreamData_3_ncm$F_Overige_sportreportage <- 0
totalStreamData_3_ncm$F_Overige_sportinformatie <- 0
totalStreamData_3_ncm$F_Tekstuele_informatie <- 0
totalStreamData_3_ncm$F_Weerbericht <- 0
totalStreamData_3_ncm$Voetball <- 0 

write.csv2(totalStreamData_3_ncm,"totalStreamData_3_ncm.csv")
#add champiosleague variable

totalStreamData_3_ncm$Type..Subject.3..SKO. <- as.character(totalStreamData_3_ncm$Type..Subject.3..SKO.)
totalStreamData_3_ncm$sourcesite <- as.character(totalStreamData_3_ncm$sourcesite)
totalStreamData_3_ncm$sbs_partner <- as.character(totalStreamData_3_ncm$sbs_partner)
totalStreamData_3_ncm$Channel <- as.character(totalStreamData_3_ncm$Channel)
totalStreamData_3_ncm$Type..Subject.2..SKO. <- as.character(totalStreamData_3_ncm$Type..Subject.2..SKO.)
totalStreamData_3_ncm$Freq <- as.character(totalStreamData_3_ncm$Freq)

for(i in 1:nrow(totalStreamData_3_ncm)){
  if(ifelse(!is.na(totalStreamData_3_ncm$Freq[i]),totalStreamData_3_ncm$Freq[i],"foobar") =="EERSTE UITZENDING"){
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="BTL FILMS"){
      totalStreamData_3_ncm$F_BTL_FILMS[i] <- 1
    }
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="OVERIGE NON FICTIE"){
      totalStreamData_3_ncm$F_OVERIGE_NON_FICTIE[i] <- 1
    }
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Overig amusement"){
      totalStreamData_3_ncm$F_Overig_amusement[i] <- 1
    }
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Talentenjacht/Auditie/Programma"){
      totalStreamData_3_ncm$F_TAP[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="KINDERFILMS"){
      totalStreamData_3_ncm$F_KINDERFILMS[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Actualiteiten"){
      totalStreamData_3_ncm$F_Actualiteiten[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Nieuws"){
      totalStreamData_3_ncm$F_Nieuws[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: Programa"){
      totalStreamData_3_ncm$F_OM_Programa[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: Live registratie"){
      totalStreamData_3_ncm$F_OM_Live_registratie[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="SPEL & QUIZ"){
      totalStreamData_3_ncm$F_SPEL_QUIZ[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="NLD FILMS"){
      totalStreamData_3_ncm$F_NLD_FILMS[i] <- 1 
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="NLD SERIES"){
      totalStreamData_3_ncm$F_NLD_SERIES[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="BTL SERIES"){
      totalStreamData_3_ncm$F_BTL_SERIES[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Nld series"){
      totalStreamData_3_ncm$F_Nld_series[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Voetbalreportage"){
      totalStreamData_3_ncm$F_Voetbalreportage[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: overig"){
      totalStreamData_3_ncm$F_BTL_OM_overig[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Satire"){
      totalStreamData_3_ncm$F_Satire[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: Live registratie"){
      totalStreamData_3_ncm$F_PM_Live_registratie[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Populaire muziek: Videoclips"){
      totalStreamData_3_ncm$F_PM_Videoclips[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Actuele sportinformatie"){
      totalStreamData_3_ncm$F_Actuele_sportinformatie[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Cabaret/kleinkunst"){
      totalStreamData_3_ncm$F_Cabaret_kleinkunst[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Overige sportreportage"){
      totalStreamData_3_ncm$F_Overige_sportreportage[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Overige sportinformatie"){
      totalStreamData_3_ncm$F_Overige_sportinformatie[i] <- 1
    } 
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Tekstuele informatie"){
      totalStreamData_3_ncm$F_Tekstuele_informatie[i] <- 1
    }
    if(ifelse(!is.na(totalStreamData_3_ncm$Type..Subject.3..SKO.[i]),totalStreamData_3_ncm$Type..Subject.3..SKO.[i],"foobar")=="Weerbericht"){
      totalStreamData_3_ncm$F_Weerbericht[i] <- 1
    } 
  }
}
for(i in 1:nrow(totalStreamData_3_ncm)){
  if(grepl("championsleague",totalStreamData_3_ncm$Title[i])==TRUE){
    totalStreamData_3_ncm$Voetball[i] <- 1 
  }
}

#### export final tables for each channel####

#sbs6
final_sbs6 <- subset(totalStreamData_3_ncm, 
                     totalStreamData_3_ncm$Channel=="SBS 6"|(totalStreamData_3_ncm$sourcesite=="sbs6"&totalStreamData_3_ncm$sbs_partner=="n/a"), 
                     select = c(2,5:42)) 
for(i in 1:nrow(final_sbs6)){
  nday <- final_sbs6$`day+1`[i]
  #BTL FILMS
  if(ifelse(!is.na(final_sbs6$F_BTL_FILMS[i]),final_sbs6$F_BTL_FILMS[i],"foobar") == 1){
    final_sbs6$F_BTL_FILMS[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="BTL FILMS"){
        final_sbs6$F_BTL_FILMS[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIGE NON FICTIE
  else if(ifelse(!is.na(final_sbs6$F_OVERIGE_NON_FICTIE[i]),final_sbs6$F_OVERIGE_NON_FICTIE[i],"foobar") == 1){
    final_sbs6$F_OVERIGE_NON_FICTIE[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="OVERIGE NON FICTIE"){
        final_sbs6$F_OVERIGE_NON_FICTIE[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIG AMUSEMENT
  else if(ifelse(!is.na(final_sbs6$F_Overig_amusement[i]),final_sbs6$F_Overig_amusement[i],"foobar") == 1){
    final_sbs6$F_Overig_amusement[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="Overig amusement"){
        final_sbs6$F_Overig_amusement[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Talentenjacht/Auditie/Programma
  else if(ifelse(!is.na(final_sbs6$F_TAP[i]),final_sbs6$F_TAP[i],"foobar") == 1){
    final_sbs6$F_TAP[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="Talentenjacht/Auditie/Programma"){
        final_sbs6$F_TAP[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #KINDERFILMS
  else if(ifelse(!is.na(final_sbs6$F_KINDERFILMS[i]),final_sbs6$F_KINDERFILMS[i],"foobar") == 1){
    final_sbs6$F_KINDERFILMS[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="KINDERFILMS"){
        final_sbs6$F_KINDERFILMS[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actualiteiten
  else if(ifelse(!is.na(final_sbs6$F_Actualiteiten[i]),final_sbs6$F_Actualiteiten[i],"foobar") == 1){
    final_sbs6$F_Actualiteiten[i] <- 0
    for(j in 1:nrow(final_sbs6)){     
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="Actualiteiten"){
        final_sbs6$F_Actualiteiten[final_sbs6$yday[j]==nday][j] <- 1
      }
    }     
  }
  #Nieuws
  else if(ifelse(!is.na(final_sbs6$F_Nieuws[i]),final_sbs6$F_Nieuws[i],"foobar") == 1){
    final_sbs6$F_Nieuws[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="Nieuws"){
        final_sbs6$F_Nieuws[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Programa
  else if(ifelse(!is.na(final_sbs6$F_OM_Programa[i]),final_sbs6$F_OM_Programa[i],"foobar") == 1){
    final_sbs6$F_OM_Programa[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar") =="Overige muziek: Programa"){
        final_sbs6$F_OM_Programa[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Live registratie
  else if(ifelse(!is.na(final_sbs6$F_OM_Live_registratie[i]),final_sbs6$F_OM_Live_registratie[i],"foobar") == 1){
    final_sbs6$F_OM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: Live registratie"){
        final_sbs6$F_OM_Live_registratie[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #SPEL & QUIZ
  else if(ifelse(!is.na(final_sbs6$F_SPEL_QUIZ[i]),final_sbs6$F_SPEL_QUIZ[i],"foobar") == 1){
    final_sbs6$F_SPEL_QUIZ[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="SPEL & QUIZ"){
        final_sbs6$F_SPEL_QUIZ[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD FILMS
  else if(ifelse(!is.na(final_sbs6$F_NLD_FILMS[i]),final_sbs6$F_NLD_FILMS[i],"foobar") == 1){
    final_sbs6$F_NLD_FILMS[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="NLD FILMS"){
        final_sbs6$F_NLD_FILMS[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD SERIES
  else if(ifelse(!is.na(final_sbs6$F_NLD_SERIES[i]),final_sbs6$F_NLD_SERIES[i],"foobar") == 1){
    final_sbs6$F_NLD_SERIES[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="NLD SERIES"){
        final_sbs6$F_NLD_SERIES[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #BTL SERIES
  else if(ifelse(!is.na(final_sbs6$F_BTL_SERIES[i]),final_sbs6$F_BTL_SERIES[i],"foobar") == 1){
    final_sbs6$F_BTL_SERIES[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="BTL SERIES"){
        final_sbs6$F_BTL_SERIES[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Nld Series
  else if(ifelse(!is.na(final_sbs6$F_Nld_series[i]),final_sbs6$F_Nld_series[i],"foobar") == 1){
    final_sbs6$F_Nld_series[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Nld series"){
        final_sbs6$F_Nld_series[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Voetbalreportage
  else if(ifelse(!is.na(final_sbs6$F_Voetbalreportage[i]),final_sbs6$F_Voetbalreportage[i],"foobar") == 1){
    final_sbs6$F_Voetbalreportage[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Voetbalreportage"){
        final_sbs6$F_Voetbalreportage[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: overig
  else if(ifelse(!is.na(final_sbs6$F_OM_overig[i]),final_sbs6$F_OM_overig[i],"foobar") == 1){
    final_sbs6$F_OM_overig[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: overig"){
        final_sbs6$F_OM_overig[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Satire
  else if(ifelse(!is.na(final_sbs6$F_Satire[i]),final_sbs6$F_Satire[i],"foobar") == 1){
    final_sbs6$F_Satire[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Satire"){
        final_sbs6$F_Satire[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Live registratie
  else if(ifelse(!is.na(final_sbs6$F_PM_Live_registratie[i]),final_sbs6$F_PM_Live_registratie[i],"foobar") == 1){
    final_sbs6$F_PM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Live registratie"){
        final_sbs6$F_PM_Live_registratie[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Videoclips
  else if(ifelse(!is.na(final_sbs6$F_PM_Videoclips[i]),final_sbs6$F_PM_Videoclips[i],"foobar") == 1){
    final_sbs6$F_PM_Videoclips[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Videoclips"){
        final_sbs6$F_PM_Videoclips[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actuele sportinformatie
  else if(ifelse(!is.na(final_sbs6$F_Actuele_sportinformatie[i]),final_sbs6$F_Actuele_sportinformatie[i],"foobar") == 1){
    final_sbs6$F_Actuele_sportinformatie[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Actuele sportinformatie"){
        final_sbs6$F_Actuele_sportinformatie[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Cabaret/kleinkunst
  else if(ifelse(!is.na(final_sbs6$F_Cabaret_kleinkunst[i]),final_sbs6$F_Cabaret_kleinkunst[i],"foobar") == 1){
    final_sbs6$F_Cabaret_kleinkunst[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Cabaret/kleinkunst"){
        final_sbs6$F_Cabaret_kleinkunst[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportreportage
  else if(ifelse(!is.na(final_sbs6$F_Overige_sportreportage[i]),final_sbs6$F_Overige_sportreportage[i],"foobar") == 1){
    final_sbs6$F_Overige_sportreportage[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Overige sportreportage"){
        final_sbs6$F_Overige_sportreportage[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportinformatie
  else if(ifelse(!is.na(final_sbs6$F_Overige_sportinformatie[i]),final_sbs6$F_Overige_sportinformatie[i],"foobar") == 1){
    final_sbs6$F_Overige_sportinformatie[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Overige sportinformatie"){
        final_sbs6$F_Overige_sportinformatie[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  #Tekstuele informatie
  else if(ifelse(!is.na(final_sbs6$F_Tekstuele_informatie[i]),final_sbs6$F_Tekstuele_informatie[i],"foobar") == 1){
    final_sbs6$F_Tekstuele_informatie[i] <- 0
    for(j in 1:nrow(final_sbs6)){
      if(final_sbs6$yday[j]==nday & ifelse(!is.na(final_sbs6$Type..Subject.2..SKO.[j]),final_sbs6$Type..Subject.2..SKO.[j],"foobar")=="Tekstuele informatie"){
        final_sbs6$F_Tekstuele_informatie[final_sbs6$yday[j]==nday][j] <- 1
      }
    }
  }
  
}

#SBS9
final_sbs9 <- subset(totalStreamData_3_ncm, 
                     totalStreamData_3_ncm$Channel=="SBS 9"|(totalStreamData_3_ncm$sourcesite=="sbs9"&totalStreamData_3_ncm$sbs_partner=="n/a"), 
                     select = c(2,5:42)) 
for(i in 1:nrow(final_sbs9)){
  nday <- final_sbs9$`day+1`[i]
  #BTL FILMS
  if(ifelse(!is.na(final_sbs9$F_BTL_FILMS[i]),final_sbs9$F_BTL_FILMS[i],"foobar") == 1){
    final_sbs9$F_BTL_FILMS[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="BTL FILMS"){
        final_sbs9$F_BTL_FILMS[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIGE NON FICTIE
  else if(ifelse(!is.na(final_sbs9$F_OVERIGE_NON_FICTIE[i]),final_sbs9$F_OVERIGE_NON_FICTIE[i],"foobar") == 1){
    final_sbs9$F_OVERIGE_NON_FICTIE[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="OVERIGE NON FICTIE"){
        final_sbs9$F_OVERIGE_NON_FICTIE[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIG AMUSEMENT
  else if(ifelse(!is.na(final_sbs9$F_Overig_amusement[i]),final_sbs9$F_Overig_amusement[i],"foobar") == 1){
    final_sbs9$F_Overig_amusement[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="Overig amusement"){
        final_sbs9$F_Overig_amusement[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Talentenjacht/Auditie/Programma
  else if(ifelse(!is.na(final_sbs9$F_TAP[i]),final_sbs9$F_TAP[i],"foobar") == 1){
    final_sbs9$F_TAP[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="Talentenjacht/Auditie/Programma"){
        final_sbs9$F_TAP[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #KINDERFILMS
  else if(ifelse(!is.na(final_sbs9$F_KINDERFILMS[i]),final_sbs9$F_KINDERFILMS[i],"foobar") == 1){
    final_sbs9$F_KINDERFILMS[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="KINDERFILMS"){
        final_sbs9$F_KINDERFILMS[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actualiteiten
  else if(ifelse(!is.na(final_sbs9$F_Actualiteiten[i]),final_sbs9$F_Actualiteiten[i],"foobar") == 1){
    final_sbs9$F_Actualiteiten[i] <- 0
    for(j in 1:nrow(final_sbs9)){     
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="Actualiteiten"){
        final_sbs9$F_Actualiteiten[final_sbs9$yday[j]==nday][j] <- 1
      }
    }     
  }
  #Nieuws
  else if(ifelse(!is.na(final_sbs9$F_Nieuws[i]),final_sbs9$F_Nieuws[i],"foobar") == 1){
    final_sbs9$F_Nieuws[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="Nieuws"){
        final_sbs9$F_Nieuws[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Programa
  else if(ifelse(!is.na(final_sbs9$F_OM_Programa[i]),final_sbs9$F_OM_Programa[i],"foobar") == 1){
    final_sbs9$F_OM_Programa[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar") =="Overige muziek: Programa"){
        final_sbs9$F_OM_Programa[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Live registratie
  else if(ifelse(!is.na(final_sbs9$F_OM_Live_registratie[i]),final_sbs9$F_OM_Live_registratie[i],"foobar") == 1){
    final_sbs9$F_OM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: Live registratie"){
        final_sbs9$F_OM_Live_registratie[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #SPEL & QUIZ
  else if(ifelse(!is.na(final_sbs9$F_SPEL_QUIZ[i]),final_sbs9$F_SPEL_QUIZ[i],"foobar") == 1){
    final_sbs9$F_SPEL_QUIZ[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="SPEL & QUIZ"){
        final_sbs9$F_SPEL_QUIZ[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD FILMS
  else if(ifelse(!is.na(final_sbs9$F_NLD_FILMS[i]),final_sbs9$F_NLD_FILMS[i],"foobar") == 1){
    final_sbs9$F_NLD_FILMS[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="NLD FILMS"){
        final_sbs9$F_NLD_FILMS[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD SERIES
  else if(ifelse(!is.na(final_sbs9$F_NLD_SERIES[i]),final_sbs9$F_NLD_SERIES[i],"foobar") == 1){
    final_sbs9$F_NLD_SERIES[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="NLD SERIES"){
        final_sbs9$F_NLD_SERIES[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #BTL SERIES
  else if(ifelse(!is.na(final_sbs9$F_BTL_SERIES[i]),final_sbs9$F_BTL_SERIES[i],"foobar") == 1){
    final_sbs9$F_BTL_SERIES[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="BTL SERIES"){
        final_sbs9$F_BTL_SERIES[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Nld Series
  else if(ifelse(!is.na(final_sbs9$F_Nld_series[i]),final_sbs9$F_Nld_series[i],"foobar") == 1){
    final_sbs9$F_Nld_series[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Nld series"){
        final_sbs9$F_Nld_series[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Voetbalreportage
  else if(ifelse(!is.na(final_sbs9$F_Voetbalreportage[i]),final_sbs9$F_Voetbalreportage[i],"foobar") == 1){
    final_sbs9$F_Voetbalreportage[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Voetbalreportage"){
        final_sbs9$F_Voetbalreportage[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: overig
  else if(ifelse(!is.na(final_sbs9$F_OM_overig[i]),final_sbs9$F_OM_overig[i],"foobar") == 1){
    final_sbs9$F_OM_overig[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: overig"){
        final_sbs9$F_OM_overig[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Satire
  else if(ifelse(!is.na(final_sbs9$F_Satire[i]),final_sbs9$F_Satire[i],"foobar") == 1){
    final_sbs9$F_Satire[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Satire"){
        final_sbs9$F_Satire[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Live registratie
  else if(ifelse(!is.na(final_sbs9$F_PM_Live_registratie[i]),final_sbs9$F_PM_Live_registratie[i],"foobar") == 1){
    final_sbs9$F_PM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Live registratie"){
        final_sbs9$F_PM_Live_registratie[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Videoclips
  else if(ifelse(!is.na(final_sbs9$F_PM_Videoclips[i]),final_sbs9$F_PM_Videoclips[i],"foobar") == 1){
    final_sbs9$F_PM_Videoclips[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Videoclips"){
        final_sbs9$F_PM_Videoclips[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actuele sportinformatie
  else if(ifelse(!is.na(final_sbs9$F_Actuele_sportinformatie[i]),final_sbs9$F_Actuele_sportinformatie[i],"foobar") == 1){
    final_sbs9$F_Actuele_sportinformatie[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Actuele sportinformatie"){
        final_sbs9$F_Actuele_sportinformatie[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Cabaret/kleinkunst
  else if(ifelse(!is.na(final_sbs9$F_Cabaret_kleinkunst[i]),final_sbs9$F_Cabaret_kleinkunst[i],"foobar") == 1){
    final_sbs9$F_Cabaret_kleinkunst[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Cabaret/kleinkunst"){
        final_sbs9$F_Cabaret_kleinkunst[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportreportage
  else if(ifelse(!is.na(final_sbs9$F_Overige_sportreportage[i]),final_sbs9$F_Overige_sportreportage[i],"foobar") == 1){
    final_sbs9$F_Overige_sportreportage[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Overige sportreportage"){
        final_sbs9$F_Overige_sportreportage[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportinformatie
  else if(ifelse(!is.na(final_sbs9$F_Overige_sportinformatie[i]),final_sbs9$F_Overige_sportinformatie[i],"foobar") == 1){
    final_sbs9$F_Overige_sportinformatie[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Overige sportinformatie"){
        final_sbs9$F_Overige_sportinformatie[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  #Tekstuele informatie
  else if(ifelse(!is.na(final_sbs9$F_Tekstuele_informatie[i]),final_sbs9$F_Tekstuele_informatie[i],"foobar") == 1){
    final_sbs9$F_Tekstuele_informatie[i] <- 0
    for(j in 1:nrow(final_sbs9)){
      if(final_sbs9$yday[j]==nday & ifelse(!is.na(final_sbs9$Type..Subject.2..SKO.[j]),final_sbs9$Type..Subject.2..SKO.[j],"foobar")=="Tekstuele informatie"){
        final_sbs9$F_Tekstuele_informatie[final_sbs9$yday[j]==nday][j] <- 1
      }
    }
  }
  
}

#VERONICATV
final_veronicatv <- subset(totalStreamData_3_ncm, 
                           totalStreamData_3_ncm$Channel=="VERONICA"|(totalStreamData_3_ncm$sourcesite=="veronicatv"&totalStreamData_3_ncm$sbs_partner=="n/a"), 
                           select = c(2,5:42)) 
for(i in 1:nrow(final_veronicatv)){
  nday <- final_veronicatv$`day+1`[i]
  #BTL FILMS
  if(ifelse(!is.na(final_veronicatv$F_BTL_FILMS[i]),final_veronicatv$F_BTL_FILMS[i],"foobar") == 1){
    final_veronicatv$F_BTL_FILMS[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="BTL FILMS"){
        final_veronicatv$F_BTL_FILMS[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIGE NON FICTIE
  else if(ifelse(!is.na(final_veronicatv$F_OVERIGE_NON_FICTIE[i]),final_veronicatv$F_OVERIGE_NON_FICTIE[i],"foobar") == 1){
    final_veronicatv$F_OVERIGE_NON_FICTIE[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="OVERIGE NON FICTIE"){
        final_veronicatv$F_OVERIGE_NON_FICTIE[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIG AMUSEMENT
  else if(ifelse(!is.na(final_veronicatv$F_Overig_amusement[i]),final_veronicatv$F_Overig_amusement[i],"foobar") == 1){
    final_veronicatv$F_Overig_amusement[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="Overig amusement"){
        final_veronicatv$F_Overig_amusement[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Talentenjacht/Auditie/Programma
  else if(ifelse(!is.na(final_veronicatv$F_TAP[i]),final_veronicatv$F_TAP[i],"foobar") == 1){
    final_veronicatv$F_TAP[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="Talentenjacht/Auditie/Programma"){
        final_veronicatv$F_TAP[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #KINDERFILMS
  else if(ifelse(!is.na(final_veronicatv$F_KINDERFILMS[i]),final_veronicatv$F_KINDERFILMS[i],"foobar") == 1){
    final_veronicatv$F_KINDERFILMS[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="KINDERFILMS"){
        final_veronicatv$F_KINDERFILMS[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actualiteiten
  else if(ifelse(!is.na(final_veronicatv$F_Actualiteiten[i]),final_veronicatv$F_Actualiteiten[i],"foobar") == 1){
    final_veronicatv$F_Actualiteiten[i] <- 0
    for(j in 1:nrow(final_veronicatv)){     
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="Actualiteiten"){
        final_veronicatv$F_Actualiteiten[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }     
  }
  #Nieuws
  else if(ifelse(!is.na(final_veronicatv$F_Nieuws[i]),final_veronicatv$F_Nieuws[i],"foobar") == 1){
    final_veronicatv$F_Nieuws[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="Nieuws"){
        final_veronicatv$F_Nieuws[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Programa
  else if(ifelse(!is.na(final_veronicatv$F_OM_Programa[i]),final_veronicatv$F_OM_Programa[i],"foobar") == 1){
    final_veronicatv$F_OM_Programa[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar") =="Overige muziek: Programa"){
        final_veronicatv$F_OM_Programa[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Live registratie
  else if(ifelse(!is.na(final_veronicatv$F_OM_Live_registratie[i]),final_veronicatv$F_OM_Live_registratie[i],"foobar") == 1){
    final_veronicatv$F_OM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: Live registratie"){
        final_veronicatv$F_OM_Live_registratie[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #SPEL & QUIZ
  else if(ifelse(!is.na(final_veronicatv$F_SPEL_QUIZ[i]),final_veronicatv$F_SPEL_QUIZ[i],"foobar") == 1){
    final_veronicatv$F_SPEL_QUIZ[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="SPEL & QUIZ"){
        final_veronicatv$F_SPEL_QUIZ[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD FILMS
  else if(ifelse(!is.na(final_veronicatv$F_NLD_FILMS[i]),final_veronicatv$F_NLD_FILMS[i],"foobar") == 1){
    final_veronicatv$F_NLD_FILMS[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="NLD FILMS"){
        final_veronicatv$F_NLD_FILMS[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD SERIES
  else if(ifelse(!is.na(final_veronicatv$F_NLD_SERIES[i]),final_veronicatv$F_NLD_SERIES[i],"foobar") == 1){
    final_veronicatv$F_NLD_SERIES[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="NLD SERIES"){
        final_veronicatv$F_NLD_SERIES[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #BTL SERIES
  else if(ifelse(!is.na(final_veronicatv$F_BTL_SERIES[i]),final_veronicatv$F_BTL_SERIES[i],"foobar") == 1){
    final_veronicatv$F_BTL_SERIES[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="BTL SERIES"){
        final_veronicatv$F_BTL_SERIES[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Nld Series
  else if(ifelse(!is.na(final_veronicatv$F_Nld_series[i]),final_veronicatv$F_Nld_series[i],"foobar") == 1){
    final_veronicatv$F_Nld_series[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Nld series"){
        final_veronicatv$F_Nld_series[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Voetbalreportage
  else if(ifelse(!is.na(final_veronicatv$F_Voetbalreportage[i]),final_veronicatv$F_Voetbalreportage[i],"foobar") == 1){
    final_veronicatv$F_Voetbalreportage[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Voetbalreportage"){
        final_veronicatv$F_Voetbalreportage[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: overig
  else if(ifelse(!is.na(final_veronicatv$F_OM_overig[i]),final_veronicatv$F_OM_overig[i],"foobar") == 1){
    final_veronicatv$F_OM_overig[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: overig"){
        final_veronicatv$F_OM_overig[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Satire
  else if(ifelse(!is.na(final_veronicatv$F_Satire[i]),final_veronicatv$F_Satire[i],"foobar") == 1){
    final_veronicatv$F_Satire[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Satire"){
        final_veronicatv$F_Satire[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Live registratie
  else if(ifelse(!is.na(final_veronicatv$F_PM_Live_registratie[i]),final_veronicatv$F_PM_Live_registratie[i],"foobar") == 1){
    final_veronicatv$F_PM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Live registratie"){
        final_veronicatv$F_PM_Live_registratie[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Videoclips
  else if(ifelse(!is.na(final_veronicatv$F_PM_Videoclips[i]),final_veronicatv$F_PM_Videoclips[i],"foobar") == 1){
    final_veronicatv$F_PM_Videoclips[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Videoclips"){
        final_veronicatv$F_PM_Videoclips[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actuele sportinformatie
  else if(ifelse(!is.na(final_veronicatv$F_Actuele_sportinformatie[i]),final_veronicatv$F_Actuele_sportinformatie[i],"foobar") == 1){
    final_veronicatv$F_Actuele_sportinformatie[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Actuele sportinformatie"){
        final_veronicatv$F_Actuele_sportinformatie[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Cabaret/kleinkunst
  else if(ifelse(!is.na(final_veronicatv$F_Cabaret_kleinkunst[i]),final_veronicatv$F_Cabaret_kleinkunst[i],"foobar") == 1){
    final_veronicatv$F_Cabaret_kleinkunst[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Cabaret/kleinkunst"){
        final_veronicatv$F_Cabaret_kleinkunst[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportreportage
  else if(ifelse(!is.na(final_veronicatv$F_Overige_sportreportage[i]),final_veronicatv$F_Overige_sportreportage[i],"foobar") == 1){
    final_veronicatv$F_Overige_sportreportage[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Overige sportreportage"){
        final_veronicatv$F_Overige_sportreportage[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportinformatie
  else if(ifelse(!is.na(final_veronicatv$F_Overige_sportinformatie[i]),final_veronicatv$F_Overige_sportinformatie[i],"foobar") == 1){
    final_veronicatv$F_Overige_sportinformatie[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Overige sportinformatie"){
        final_veronicatv$F_Overige_sportinformatie[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  #Tekstuele informatie
  else if(ifelse(!is.na(final_veronicatv$F_Tekstuele_informatie[i]),final_veronicatv$F_Tekstuele_informatie[i],"foobar") == 1){
    final_veronicatv$F_Tekstuele_informatie[i] <- 0
    for(j in 1:nrow(final_veronicatv)){
      if(final_veronicatv$yday[j]==nday & ifelse(!is.na(final_veronicatv$Type..Subject.2..SKO.[j]),final_veronicatv$Type..Subject.2..SKO.[j],"foobar")=="Tekstuele informatie"){
        final_veronicatv$F_Tekstuele_informatie[final_veronicatv$yday[j]==nday][j] <- 1
      }
    }
  }
  
}

#net5
final_net5 <- subset(totalStreamData_3_ncm, 
                     totalStreamData_3_ncm$Channel=="NET 5"|(totalStreamData_3_ncm$sourcesite=="net5"&totalStreamData_3_ncm$sbs_partner=="n/a"), 
                     select = c(2,5:42)) 
for(i in 1:nrow(final_net5)){
  nday <- final_net5$`day+1`[i]
  #BTL FILMS
  if(ifelse(!is.na(final_net5$F_BTL_FILMS[i]),final_net5$F_BTL_FILMS[i],"foobar") == 1){
    final_net5$F_BTL_FILMS[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="BTL FILMS"){
        final_net5$F_BTL_FILMS[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIGE NON FICTIE
  else if(ifelse(!is.na(final_net5$F_OVERIGE_NON_FICTIE[i]),final_net5$F_OVERIGE_NON_FICTIE[i],"foobar") == 1){
    final_net5$F_OVERIGE_NON_FICTIE[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="OVERIGE NON FICTIE"){
        final_net5$F_OVERIGE_NON_FICTIE[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIG AMUSEMENT
  else if(ifelse(!is.na(final_net5$F_Overig_amusement[i]),final_net5$F_Overig_amusement[i],"foobar") == 1){
    final_net5$F_Overig_amusement[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="Overig amusement"){
        final_net5$F_Overig_amusement[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Talentenjacht/Auditie/Programma
  else if(ifelse(!is.na(final_net5$F_TAP[i]),final_net5$F_TAP[i],"foobar") == 1){
    final_net5$F_TAP[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="Talentenjacht/Auditie/Programma"){
        final_net5$F_TAP[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #KINDERFILMS
  else if(ifelse(!is.na(final_net5$F_KINDERFILMS[i]),final_net5$F_KINDERFILMS[i],"foobar") == 1){
    final_net5$F_KINDERFILMS[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="KINDERFILMS"){
        final_net5$F_KINDERFILMS[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actualiteiten
  else if(ifelse(!is.na(final_net5$F_Actualiteiten[i]),final_net5$F_Actualiteiten[i],"foobar") == 1){
    final_net5$F_Actualiteiten[i] <- 0
    for(j in 1:nrow(final_net5)){     
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="Actualiteiten"){
        final_net5$F_Actualiteiten[final_net5$yday[j]==nday][j] <- 1
      }
    }     
  }
  #Nieuws
  else if(ifelse(!is.na(final_net5$F_Nieuws[i]),final_net5$F_Nieuws[i],"foobar") == 1){
    final_net5$F_Nieuws[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="Nieuws"){
        final_net5$F_Nieuws[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Programa
  else if(ifelse(!is.na(final_net5$F_OM_Programa[i]),final_net5$F_OM_Programa[i],"foobar") == 1){
    final_net5$F_OM_Programa[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar") =="Overige muziek: Programa"){
        final_net5$F_OM_Programa[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Live registratie
  else if(ifelse(!is.na(final_net5$F_OM_Live_registratie[i]),final_net5$F_OM_Live_registratie[i],"foobar") == 1){
    final_net5$F_OM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: Live registratie"){
        final_net5$F_OM_Live_registratie[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #SPEL & QUIZ
  else if(ifelse(!is.na(final_net5$F_SPEL_QUIZ[i]),final_net5$F_SPEL_QUIZ[i],"foobar") == 1){
    final_net5$F_SPEL_QUIZ[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="SPEL & QUIZ"){
        final_net5$F_SPEL_QUIZ[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD FILMS
  else if(ifelse(!is.na(final_net5$F_NLD_FILMS[i]),final_net5$F_NLD_FILMS[i],"foobar") == 1){
    final_net5$F_NLD_FILMS[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="NLD FILMS"){
        final_net5$F_NLD_FILMS[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD SERIES
  else if(ifelse(!is.na(final_net5$F_NLD_SERIES[i]),final_net5$F_NLD_SERIES[i],"foobar") == 1){
    final_net5$F_NLD_SERIES[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="NLD SERIES"){
        final_net5$F_NLD_SERIES[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #BTL SERIES
  else if(ifelse(!is.na(final_net5$F_BTL_SERIES[i]),final_net5$F_BTL_SERIES[i],"foobar") == 1){
    final_net5$F_BTL_SERIES[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="BTL SERIES"){
        final_net5$F_BTL_SERIES[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Nld Series
  else if(ifelse(!is.na(final_net5$F_Nld_series[i]),final_net5$F_Nld_series[i],"foobar") == 1){
    final_net5$F_Nld_series[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Nld series"){
        final_net5$F_Nld_series[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Voetbalreportage
  else if(ifelse(!is.na(final_net5$F_Voetbalreportage[i]),final_net5$F_Voetbalreportage[i],"foobar") == 1){
    final_net5$F_Voetbalreportage[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Voetbalreportage"){
        final_net5$F_Voetbalreportage[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: overig
  else if(ifelse(!is.na(final_net5$F_OM_overig[i]),final_net5$F_OM_overig[i],"foobar") == 1){
    final_net5$F_OM_overig[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: overig"){
        final_net5$F_OM_overig[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Satire
  else if(ifelse(!is.na(final_net5$F_Satire[i]),final_net5$F_Satire[i],"foobar") == 1){
    final_net5$F_Satire[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Satire"){
        final_net5$F_Satire[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Live registratie
  else if(ifelse(!is.na(final_net5$F_PM_Live_registratie[i]),final_net5$F_PM_Live_registratie[i],"foobar") == 1){
    final_net5$F_PM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Live registratie"){
        final_net5$F_PM_Live_registratie[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Videoclips
  else if(ifelse(!is.na(final_net5$F_PM_Videoclips[i]),final_net5$F_PM_Videoclips[i],"foobar") == 1){
    final_net5$F_PM_Videoclips[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Videoclips"){
        final_net5$F_PM_Videoclips[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actuele sportinformatie
  else if(ifelse(!is.na(final_net5$F_Actuele_sportinformatie[i]),final_net5$F_Actuele_sportinformatie[i],"foobar") == 1){
    final_net5$F_Actuele_sportinformatie[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Actuele sportinformatie"){
        final_net5$F_Actuele_sportinformatie[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Cabaret/kleinkunst
  else if(ifelse(!is.na(final_net5$F_Cabaret_kleinkunst[i]),final_net5$F_Cabaret_kleinkunst[i],"foobar") == 1){
    final_net5$F_Cabaret_kleinkunst[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Cabaret/kleinkunst"){
        final_net5$F_Cabaret_kleinkunst[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportreportage
  else if(ifelse(!is.na(final_net5$F_Overige_sportreportage[i]),final_net5$F_Overige_sportreportage[i],"foobar") == 1){
    final_net5$F_Overige_sportreportage[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Overige sportreportage"){
        final_net5$F_Overige_sportreportage[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportinformatie
  else if(ifelse(!is.na(final_net5$F_Overige_sportinformatie[i]),final_net5$F_Overige_sportinformatie[i],"foobar") == 1){
    final_net5$F_Overige_sportinformatie[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Overige sportinformatie"){
        final_net5$F_Overige_sportinformatie[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  #Tekstuele informatie
  else if(ifelse(!is.na(final_net5$F_Tekstuele_informatie[i]),final_net5$F_Tekstuele_informatie[i],"foobar") == 1){
    final_net5$F_Tekstuele_informatie[i] <- 0
    for(j in 1:nrow(final_net5)){
      if(final_net5$yday[j]==nday & ifelse(!is.na(final_net5$Type..Subject.2..SKO.[j]),final_net5$Type..Subject.2..SKO.[j],"foobar")=="Tekstuele informatie"){
        final_net5$F_Tekstuele_informatie[final_net5$yday[j]==nday][j] <- 1
      }
    }
  }
  
}

#S1
final_s1 <- subset(totalStreamData_3_ncm, 
                   (totalStreamData_3_ncm$sbs_partner=="s1tv"), 
                   select = c(2,5:42)) 
for(i in 1:nrow(final_s1)){
  nday <- final_s1$`day+1`[i]
  #BTL FILMS
  if(ifelse(!is.na(final_s1$F_BTL_FILMS[i]),final_s1$F_BTL_FILMS[i],"foobar") == 1){
    final_s1$F_BTL_FILMS[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="BTL FILMS"){
        final_s1$F_BTL_FILMS[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIGE NON FICTIE
  else if(ifelse(!is.na(final_s1$F_OVERIGE_NON_FICTIE[i]),final_s1$F_OVERIGE_NON_FICTIE[i],"foobar") == 1){
    final_s1$F_OVERIGE_NON_FICTIE[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="OVERIGE NON FICTIE"){
        final_s1$F_OVERIGE_NON_FICTIE[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #OVERIG AMUSEMENT
  else if(ifelse(!is.na(final_s1$F_Overig_amusement[i]),final_s1$F_Overig_amusement[i],"foobar") == 1){
    final_s1$F_Overig_amusement[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="Overig amusement"){
        final_s1$F_Overig_amusement[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Talentenjacht/Auditie/Programma
  else if(ifelse(!is.na(final_s1$F_TAP[i]),final_s1$F_TAP[i],"foobar") == 1){
    final_s1$F_TAP[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="Talentenjacht/Auditie/Programma"){
        final_s1$F_TAP[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #KINDERFILMS
  else if(ifelse(!is.na(final_s1$F_KINDERFILMS[i]),final_s1$F_KINDERFILMS[i],"foobar") == 1){
    final_s1$F_KINDERFILMS[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="KINDERFILMS"){
        final_s1$F_KINDERFILMS[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actualiteiten
  else if(ifelse(!is.na(final_s1$F_Actualiteiten[i]),final_s1$F_Actualiteiten[i],"foobar") == 1){
    final_s1$F_Actualiteiten[i] <- 0
    for(j in 1:nrow(final_s1)){     
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="Actualiteiten"){
        final_s1$F_Actualiteiten[final_s1$yday[j]==nday][j] <- 1
      }
    }     
  }
  #Nieuws
  else if(ifelse(!is.na(final_s1$F_Nieuws[i]),final_s1$F_Nieuws[i],"foobar") == 1){
    final_s1$F_Nieuws[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="Nieuws"){
        final_s1$F_Nieuws[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Programa
  else if(ifelse(!is.na(final_s1$F_OM_Programa[i]),final_s1$F_OM_Programa[i],"foobar") == 1){
    final_s1$F_OM_Programa[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar") =="Overige muziek: Programa"){
        final_s1$F_OM_Programa[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: Live registratie
  else if(ifelse(!is.na(final_s1$F_OM_Live_registratie[i]),final_s1$F_OM_Live_registratie[i],"foobar") == 1){
    final_s1$F_OM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: Live registratie"){
        final_s1$F_OM_Live_registratie[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #SPEL & QUIZ
  else if(ifelse(!is.na(final_s1$F_SPEL_QUIZ[i]),final_s1$F_SPEL_QUIZ[i],"foobar") == 1){
    final_s1$F_SPEL_QUIZ[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="SPEL & QUIZ"){
        final_s1$F_SPEL_QUIZ[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD FILMS
  else if(ifelse(!is.na(final_s1$F_NLD_FILMS[i]),final_s1$F_NLD_FILMS[i],"foobar") == 1){
    final_s1$F_NLD_FILMS[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="NLD FILMS"){
        final_s1$F_NLD_FILMS[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #NLD SERIES
  else if(ifelse(!is.na(final_s1$F_NLD_SERIES[i]),final_s1$F_NLD_SERIES[i],"foobar") == 1){
    final_s1$F_NLD_SERIES[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="NLD SERIES"){
        final_s1$F_NLD_SERIES[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #BTL SERIES
  else if(ifelse(!is.na(final_s1$F_BTL_SERIES[i]),final_s1$F_BTL_SERIES[i],"foobar") == 1){
    final_s1$F_BTL_SERIES[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="BTL SERIES"){
        final_s1$F_BTL_SERIES[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Nld Series
  else if(ifelse(!is.na(final_s1$F_Nld_series[i]),final_s1$F_Nld_series[i],"foobar") == 1){
    final_s1$F_Nld_series[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Nld series"){
        final_s1$F_Nld_series[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Voetbalreportage
  else if(ifelse(!is.na(final_s1$F_Voetbalreportage[i]),final_s1$F_Voetbalreportage[i],"foobar") == 1){
    final_s1$F_Voetbalreportage[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Voetbalreportage"){
        final_s1$F_Voetbalreportage[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige muziek: overig
  else if(ifelse(!is.na(final_s1$F_OM_overig[i]),final_s1$F_OM_overig[i],"foobar") == 1){
    final_s1$F_OM_overig[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Overige muziek: overig"){
        final_s1$F_OM_overig[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Satire
  else if(ifelse(!is.na(final_s1$F_Satire[i]),final_s1$F_Satire[i],"foobar") == 1){
    final_s1$F_Satire[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Satire"){
        final_s1$F_Satire[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Live registratie
  else if(ifelse(!is.na(final_s1$F_PM_Live_registratie[i]),final_s1$F_PM_Live_registratie[i],"foobar") == 1){
    final_s1$F_PM_Live_registratie[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Live registratie"){
        final_s1$F_PM_Live_registratie[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Populaire muziek: Videoclips
  else if(ifelse(!is.na(final_s1$F_PM_Videoclips[i]),final_s1$F_PM_Videoclips[i],"foobar") == 1){
    final_s1$F_PM_Videoclips[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Populaire muziek: Videoclips"){
        final_s1$F_PM_Videoclips[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Actuele sportinformatie
  else if(ifelse(!is.na(final_s1$F_Actuele_sportinformatie[i]),final_s1$F_Actuele_sportinformatie[i],"foobar") == 1){
    final_s1$F_Actuele_sportinformatie[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Actuele sportinformatie"){
        final_s1$F_Actuele_sportinformatie[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Cabaret/kleinkunst
  else if(ifelse(!is.na(final_s1$F_Cabaret_kleinkunst[i]),final_s1$F_Cabaret_kleinkunst[i],"foobar") == 1){
    final_s1$F_Cabaret_kleinkunst[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Cabaret/kleinkunst"){
        final_s1$F_Cabaret_kleinkunst[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportreportage
  else if(ifelse(!is.na(final_s1$F_Overige_sportreportage[i]),final_s1$F_Overige_sportreportage[i],"foobar") == 1){
    final_s1$F_Overige_sportreportage[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Overige sportreportage"){
        final_s1$F_Overige_sportreportage[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Overige sportinformatie
  else if(ifelse(!is.na(final_s1$F_Overige_sportinformatie[i]),final_s1$F_Overige_sportinformatie[i],"foobar") == 1){
    final_s1$F_Overige_sportinformatie[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Overige sportinformatie"){
        final_s1$F_Overige_sportinformatie[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  #Tekstuele informatie
  else if(ifelse(!is.na(final_s1$F_Tekstuele_informatie[i]),final_s1$F_Tekstuele_informatie[i],"foobar") == 1){
    final_s1$F_Tekstuele_informatie[i] <- 0
    for(j in 1:nrow(final_s1)){
      if(final_s1$yday[j]==nday & ifelse(!is.na(final_s1$Type..Subject.2..SKO.[j]),final_s1$Type..Subject.2..SKO.[j],"foobar")=="Tekstuele informatie"){
        final_s1$F_Tekstuele_informatie[final_s1$yday[j]==nday][j] <- 1
      }
    }
  }
  
}

####import weather data####
weather <- read.table("KNMI_20160511.txt", header = TRUE, sep=',')
weather <- weather[,-c(1)]
colnames(weather) <- c("Date","Temp","Rain")
weather$Date <- as.character(weather$Date)
weather$Date <- ymd(weather$Date)

#### final sbs6 ####
final_hol <- totalHol[,c(1,3,4)]

final_sbs6_1 <- left_join(final_sbs6, final_hol, by="Date")
for(i in 1:nrow(final_sbs6_1)){
  if(is.na(final_sbs6_1$Holiday.name[i]) & is.na(final_sbs6_1$Holiday.type[i])){
    final_sbs6_1$Holiday.name[i] <- "NH"
    final_sbs6_1$Holiday.type[i] <- "NH"
  }
}
#BTL FILMS
#OVERIGE NON FICTIE
#OVERIG AMUSEMENT
#Talentenjacht/Auditie/Programma
#KINDERFILMS
#Actualiteiten
#Niews
#Overige muziek: Programa
#Overige muziek: Live registratie
#SPEL & QUIZ
#NLD FILMS
#OVERIGE NON FICTIE
#NLD SERIES
#BTL SERIES
#Nld Series
#Voetbalreportage
#Overige muziek: overig
#Satire
#Populaire muziek: Live registratie
#Populaire muziek: Videoclips
#Actuele sportinformatie
#Cabaret/kleinkunst
#Overige sportreportage
#Overige sportinformatie
#Tekstuele informatie

colnames(final_sbs6_1) <- c("Date",
                            "Stream.starts","Type..Subject.3..SKO.",
                            "mday","wday","yday","month","day.1",
                            "Channel",
                            "Type..Subject.2..SKO.",
                            "Freq",
                            "TRP..avgw.","Prog.Share",
                            'F_BTL_FILMS',
                            'F_OVERIGE_NON_FICTIE',
                            'F_Overig_Amusement',
                            'F_TAP',
                            'F_KINDERFILMS',
                            'F_Actualiteiten',
                            'F_Nieuws',
                            'F_OM_Programa',
                            'F_OM_Live_registratie',
                            'F_SPEL_QUIZ',
                            'F_NLD_FILMS',
                            'F_NLD_SERIES',
                            'F_BTL_SERIES',
                            'F_Nld_Series',
                            'F_Voetbalreportage',
                            'F_OM_overig',
                            'F_Satire',
                            'F_PM_Live_registratie',
                            'F_PM_Videoclips',
                            'F_Actuele_sportinformatie',
                            'F_Cabaret_kleinkunst',
                            'F_Overige_sportreportage',
                            'F_Overige_sportinformatie',
                            'F_Tekstuele_informatie',
                            'F_Weerbericht',
                            'Voetball',
                            'Holiday.name',
                            'Holiday.type')

final_sbs6_2 <- subset(final_sbs6_1,select=c(Date,
                                             Stream.starts,
                                             mday,
                                             wday,
                                             yday,
                                             month,
                                             Prog.Share,
                                             F_BTL_FILMS,
                                             F_OVERIGE_NON_FICTIE,
                                             F_Overig_Amusement,
                                             F_TAP,
                                             F_KINDERFILMS,
                                             F_Actualiteiten,
                                             F_Nieuws,
                                             F_OM_Programa,
                                             F_OM_Live_registratie,
                                             F_SPEL_QUIZ,
                                             F_NLD_FILMS,
                                             F_NLD_SERIES,
                                             F_BTL_SERIES,
                                             F_Nld_Series,
                                             F_Voetbalreportage,
                                             F_OM_overig,
                                             F_Satire,
                                             F_PM_Live_registratie,
                                             F_PM_Videoclips,
                                             F_Actuele_sportinformatie,
                                             F_Cabaret_kleinkunst,
                                             F_Overige_sportreportage,
                                             F_Overige_sportinformatie,
                                             F_Tekstuele_informatie,
                                             F_Weerbericht,
                                             Voetball,
                                             Holiday.name,
                                             Holiday.type)
)
colnames(final_sbs6_2) <- c('Date',
                            'Stream.starts',
                            'mday',
                            'wday',
                            'yday',
                            'month',
                            'Prog.Share',
                            'F_BTL_FILMS',
                            'F_OVERIGE_NON_FICTIE',
                            'F_Overig_Amusement',
                            'F_TAP',
                            'F_KINDERFILMS',
                            'F_Actualiteiten',
                            'F_Nieuws',
                            'F_OM_Programa',
                            'F_OM_Live_registratie',
                            'F_SPEL_QUIZ',
                            'F_NLD_FILMS',
                            'F_NLD_SERIES',
                            'F_BTL_SERIES',
                            'F_Nld_Series',
                            'F_Voetbalreportage',
                            'F_OM_overig',
                            'F_Satire',
                            'F_PM_Live_registratie',
                            'F_PM_Videoclips',
                            'F_Actuele_sportinformatie',
                            'F_Cabaret_kleinkunst',
                            'F_Overige_sportreportage',
                            'F_Overige_sportinformatie',
                            'F_Tekstuele_informatie',
                            'F_Weerbericht',
                            'Voetball', 
                            'Holiday.name',
                            'Holiday.type')
final_sbs6_2$Prog.Share <- as.character(final_sbs6_2$Prog.Share)
final_sbs6_2$Prog.Share <- gsub(",",".",final_sbs6_2$Prog.Share)
final_sbs6_2$Prog.Share <- as.numeric(final_sbs6_2$Prog.Share)

final_sbs6_3 <- final_sbs6_2 %>% group_by(Date,
                                          mday,
                                          wday,
                                          yday,
                                          month,
                                          Holiday.name,
                                          Holiday.type
) %>% dplyr::summarise( n=sum(Stream.starts), 
                        Prog.Share=mean(Prog.Share),
                        F_BTL_FILMS=sum(),
                        F_OVERIGE_NON_FICTIE=sum(),
                        F_Overig_Amusement=sum(),
                        F_TAP=sum(),
                        F_KINDERFILMS=sum(),
                        F_Actualiteiten=sum(),
                        F_Nieuws=sum(),
                        F_OM_Programa=sum(),
                        F_OM_Live_registratie=sum(),
                        F_SPEL_QUIZ=sum(),
                        F_NLD_FILMS=sum(),
                        F_NLD_SERIES=sum(),
                        F_BTL_SERIES=sum(),
                        F_Nld_Series=sum(),
                        F_Voetbalreportage=sum(),
                        F_OM_overig=sum(),
                        F_Satire=sum(),
                        F_PM_Live_registratie=sum(),
                        F_PM_Videoclips=sum(),
                        F_Actuele_sportinformatie=sum(),
                        F_Cabaret_kleinkunst=sum(),
                        F_Overige_sportreportage=sum(),
                        F_Overige_sportinformatie=sum(),
                        F_Tekstuele_informatie=sum(),                          
                        Voetball=sum(Voetball),
                        F_Weerbericht=sum(F_Weerbericht)
)
#match weather sbs6
final_sbs6_3 <- left_join(final_sbs6_3, weather, by="Date")
final_sbs6_3$Temp <- final_sbs6_3$Temp/10
final_sbs6_3$Rain <- final_sbs6_3$Rain/100

write.csv(final_sbs6_3,"final_sbs6_2.csv")

#### final sbs9 ####

final_sbs9_1 <- left_join(final_sbs9, final_hol, by="Date")
for(i in 1:nrow(final_sbs9_1)){
  if(is.na(final_sbs9_1$Holiday.name[i]) & is.na(final_sbs9_1$Holiday.type[i])){
    final_sbs9_1$Holiday.name[i] <- "NH"
    final_sbs9_1$Holiday.type[i] <- "NH"
  }
}
#BTL FILMS
#OVERIGE NON FICTIE
#OVERIG AMUSEMENT
#Talentenjacht/Auditie/Programma
#KINDERFILMS
#Actualiteiten
#Niews
#Overige muziek: Programa
#Overige muziek: Live registratie
#SPEL & QUIZ
#NLD FILMS
#OVERIGE NON FICTIE
#NLD SERIES
#BTL SERIES
#Nld Series
#Voetbalreportage
#Overige muziek: overig
#Satire
#Populaire muziek: Live registratie
#Populaire muziek: Videoclips
#Actuele sportinformatie
#Cabaret/kleinkunst
#Overige sportreportage
#Overige sportinformatie
#Tekstuele informatie

colnames(final_sbs9_1) <- c("Date",
                            "Stream.starts","Type..Subject.3..SKO.",
                            "mday","wday","yday","month","day.1",
                            "Channel",
                            "Type..Subject.2..SKO.",
                            "Freq",
                            "TRP..avgw.","Prog.Share",
                            'F_BTL_FILMS',
                            'F_OVERIGE_NON_FICTIE',
                            'F_Overig_Amusement',
                            'F_TAP',
                            'F_KINDERFILMS',
                            'F_Actualiteiten',
                            'F_Nieuws',
                            'F_OM_Programa',
                            'F_OM_Live_registratie',
                            'F_SPEL_QUIZ',
                            'F_NLD_FILMS',
                            'F_NLD_SERIES',
                            'F_BTL_SERIES',
                            'F_Nld_Series',
                            'F_Voetbalreportage',
                            'F_OM_overig',
                            'F_Satire',
                            'F_PM_Live_registratie',
                            'F_PM_Videoclips',
                            'F_Actuele_sportinformatie',
                            'F_Cabaret_kleinkunst',
                            'F_Overige_sportreportage',
                            'F_Overige_sportinformatie',
                            'F_Tekstuele_informatie',
                            'F_Weerbericht',
                            'Voetball',
                            'Holiday.name',
                            'Holiday.type')

final_sbs9_2 <- subset(final_sbs9_1,select=c(Date,
                                             Stream.starts,
                                             mday,
                                             wday,
                                             yday,
                                             month,
                                             Prog.Share,
                                             F_BTL_FILMS,
                                             F_OVERIGE_NON_FICTIE,
                                             F_Overig_Amusement,
                                             F_TAP,
                                             F_KINDERFILMS,
                                             F_Actualiteiten,
                                             F_Nieuws,
                                             F_OM_Programa,
                                             F_OM_Live_registratie,
                                             F_SPEL_QUIZ,
                                             F_NLD_FILMS,
                                             F_NLD_SERIES,
                                             F_BTL_SERIES,
                                             F_Nld_Series,
                                             F_Voetbalreportage,
                                             F_OM_overig,
                                             F_Satire,
                                             F_PM_Live_registratie,
                                             F_PM_Videoclips,
                                             F_Actuele_sportinformatie,
                                             F_Cabaret_kleinkunst,
                                             F_Overige_sportreportage,
                                             F_Overige_sportinformatie,
                                             F_Tekstuele_informatie,
                                             F_Weerbericht,
                                             Voetball,
                                             Holiday.name,
                                             Holiday.type)
)
colnames(final_sbs9_2) <- c('Date',
                            'Stream.starts',
                            'mday',
                            'wday',
                            'yday',
                            'month',
                            'Prog.Share',
                            'F_BTL_FILMS',
                            'F_OVERIGE_NON_FICTIE',
                            'F_Overig_Amusement',
                            'F_TAP',
                            'F_KINDERFILMS',
                            'F_Actualiteiten',
                            'F_Nieuws',
                            'F_OM_Programa',
                            'F_OM_Live_registratie',
                            'F_SPEL_QUIZ',
                            'F_NLD_FILMS',
                            'F_NLD_SERIES',
                            'F_BTL_SERIES',
                            'F_Nld_Series',
                            'F_Voetbalreportage',
                            'F_OM_overig',
                            'F_Satire',
                            'F_PM_Live_registratie',
                            'F_PM_Videoclips',
                            'F_Actuele_sportinformatie',
                            'F_Cabaret_kleinkunst',
                            'F_Overige_sportreportage',
                            'F_Overige_sportinformatie',
                            'F_Tekstuele_informatie',
                            'F_Weerbericht',
                            'Voetball', 
                            'Holiday.name',
                            'Holiday.type')
final_sbs9_2$Prog.Share <- as.character(final_sbs9_2$Prog.Share)
final_sbs9_2$Prog.Share <- gsub(",",".",final_sbs9_2$Prog.Share)
final_sbs9_2$Prog.Share <- as.numeric(final_sbs9_2$Prog.Share)

final_sbs9_3 <- final_sbs9_2 %>% group_by(Date,
                                          mday,
                                          wday,
                                          yday,
                                          month,
                                          Holiday.name,
                                          Holiday.type
) %>% dplyr::summarise( n=sum(Stream.starts), 
                        Prog.Share=mean(Prog.Share),
                        F_BTL_FILMS=sum(),
                        F_OVERIGE_NON_FICTIE=sum(),
                        F_Overig_Amusement=sum(),
                        F_TAP=sum(),
                        F_KINDERFILMS=sum(),
                        F_Actualiteiten=sum(),
                        F_Nieuws=sum(),
                        F_OM_Programa=sum(),
                        F_OM_Live_registratie=sum(),
                        F_SPEL_QUIZ=sum(),
                        F_NLD_FILMS=sum(),
                        F_NLD_SERIES=sum(),
                        F_BTL_SERIES=sum(),
                        F_Nld_Series=sum(),
                        F_Voetbalreportage=sum(),
                        F_OM_overig=sum(),
                        F_Satire=sum(),
                        F_PM_Live_registratie=sum(),
                        F_PM_Videoclips=sum(),
                        F_Actuele_sportinformatie=sum(),
                        F_Cabaret_kleinkunst=sum(),
                        F_Overige_sportreportage=sum(),
                        F_Overige_sportinformatie=sum(),
                        F_Tekstuele_informatie=sum(),                          
                        Voetball=sum(Voetball),
                        F_Weerbericht=sum(F_Weerbericht)
)
#match weather sbs9
final_sbs9_3 <- left_join(final_sbs9_3, weather, by="Date")
final_sbs9_3$Temp <- final_sbs9_3$Temp/10
final_sbs9_3$Rain <- final_sbs9_3$Rain/100

write.csv(final_sbs9_3,"final_sbs9_2.csv")

#### final veronicatv ####
final_hol <- totalHol[,c(1,3,4)]

final_veronicatv_1 <- left_join(final_veronicatv, final_hol, by="Date")
for(i in 1:nrow(final_veronicatv_1)){
  if(is.na(final_veronicatv_1$Holiday.name[i]) & is.na(final_veronicatv_1$Holiday.type[i])){
    final_veronicatv_1$Holiday.name[i] <- "NH"
    final_veronicatv_1$Holiday.type[i] <- "NH"
  }
}
#BTL FILMS
#OVERIGE NON FICTIE
#OVERIG AMUSEMENT
#Talentenjacht/Auditie/Programma
#KINDERFILMS
#Actualiteiten
#Niews
#Overige muziek: Programa
#Overige muziek: Live registratie
#SPEL & QUIZ
#NLD FILMS
#OVERIGE NON FICTIE
#NLD SERIES
#BTL SERIES
#Nld Series
#Voetbalreportage
#Overige muziek: overig
#Satire
#Populaire muziek: Live registratie
#Populaire muziek: Videoclips
#Actuele sportinformatie
#Cabaret/kleinkunst
#Overige sportreportage
#Overige sportinformatie
#Tekstuele informatie

colnames(final_veronicatv_1) <- c("Date",
                                  "Stream.starts","Type..Subject.3..SKO.",
                                  "mday","wday","yday","month","day.1",
                                  "Channel",
                                  "Type..Subject.2..SKO.",
                                  "Freq",
                                  "TRP..avgw.","Prog.Share",
                                  'F_BTL_FILMS',
                                  'F_OVERIGE_NON_FICTIE',
                                  'F_Overig_Amusement',
                                  'F_TAP',
                                  'F_KINDERFILMS',
                                  'F_Actualiteiten',
                                  'F_Nieuws',
                                  'F_OM_Programa',
                                  'F_OM_Live_registratie',
                                  'F_SPEL_QUIZ',
                                  'F_NLD_FILMS',
                                  'F_NLD_SERIES',
                                  'F_BTL_SERIES',
                                  'F_Nld_Series',
                                  'F_Voetbalreportage',
                                  'F_OM_overig',
                                  'F_Satire',
                                  'F_PM_Live_registratie',
                                  'F_PM_Videoclips',
                                  'F_Actuele_sportinformatie',
                                  'F_Cabaret_kleinkunst',
                                  'F_Overige_sportreportage',
                                  'F_Overige_sportinformatie',
                                  'F_Tekstuele_informatie',
                                  'F_Weerbericht',
                                  'Voetball',
                                  'Holiday.name',
                                  'Holiday.type')

final_veronicatv_2 <- subset(final_veronicatv_1,select=c(Date,
                                                         Stream.starts,
                                                         mday,
                                                         wday,
                                                         yday,
                                                         month,
                                                         Prog.Share,
                                                         F_BTL_FILMS,
                                                         F_OVERIGE_NON_FICTIE,
                                                         F_Overig_Amusement,
                                                         F_TAP,
                                                         F_KINDERFILMS,
                                                         F_Actualiteiten,
                                                         F_Nieuws,
                                                         F_OM_Programa,
                                                         F_OM_Live_registratie,
                                                         F_SPEL_QUIZ,
                                                         F_NLD_FILMS,
                                                         F_NLD_SERIES,
                                                         F_BTL_SERIES,
                                                         F_Nld_Series,
                                                         F_Voetbalreportage,
                                                         F_OM_overig,
                                                         F_Satire,
                                                         F_PM_Live_registratie,
                                                         F_PM_Videoclips,
                                                         F_Actuele_sportinformatie,
                                                         F_Cabaret_kleinkunst,
                                                         F_Overige_sportreportage,
                                                         F_Overige_sportinformatie,
                                                         F_Tekstuele_informatie,
                                                         F_Weerbericht,
                                                         Voetball,
                                                         Holiday.name,
                                                         Holiday.type)
)
colnames(final_veronicatv_2) <- c('Date',
                                  'Stream.starts',
                                  'mday',
                                  'wday',
                                  'yday',
                                  'month',
                                  'Prog.Share',
                                  'F_BTL_FILMS',
                                  'F_OVERIGE_NON_FICTIE',
                                  'F_Overig_Amusement',
                                  'F_TAP',
                                  'F_KINDERFILMS',
                                  'F_Actualiteiten',
                                  'F_Nieuws',
                                  'F_OM_Programa',
                                  'F_OM_Live_registratie',
                                  'F_SPEL_QUIZ',
                                  'F_NLD_FILMS',
                                  'F_NLD_SERIES',
                                  'F_BTL_SERIES',
                                  'F_Nld_Series',
                                  'F_Voetbalreportage',
                                  'F_OM_overig',
                                  'F_Satire',
                                  'F_PM_Live_registratie',
                                  'F_PM_Videoclips',
                                  'F_Actuele_sportinformatie',
                                  'F_Cabaret_kleinkunst',
                                  'F_Overige_sportreportage',
                                  'F_Overige_sportinformatie',
                                  'F_Tekstuele_informatie',
                                  'F_Weerbericht',
                                  'Voetball', 
                                  'Holiday.name',
                                  'Holiday.type')
final_veronicatv_2$Prog.Share <- as.character(final_veronicatv_2$Prog.Share)
final_veronicatv_2$Prog.Share <- gsub(",",".",final_veronicatv_2$Prog.Share)
final_veronicatv_2$Prog.Share <- as.numeric(final_veronicatv_2$Prog.Share)

final_veronicatv_3 <- final_veronicatv_2 %>% group_by(Date,
                                                      mday,
                                                      wday,
                                                      yday,
                                                      month,
                                                      Holiday.name,
                                                      Holiday.type
) %>% dplyr::summarise( n=sum(Stream.starts), 
                        Prog.Share=mean(Prog.Share),
                        F_BTL_FILMS=sum(),
                        F_OVERIGE_NON_FICTIE=sum(),
                        F_Overig_Amusement=sum(),
                        F_TAP=sum(),
                        F_KINDERFILMS=sum(),
                        F_Actualiteiten=sum(),
                        F_Nieuws=sum(),
                        F_OM_Programa=sum(),
                        F_OM_Live_registratie=sum(),
                        F_SPEL_QUIZ=sum(),
                        F_NLD_FILMS=sum(),
                        F_NLD_SERIES=sum(),
                        F_BTL_SERIES=sum(),
                        F_Nld_Series=sum(),
                        F_Voetbalreportage=sum(),
                        F_OM_overig=sum(),
                        F_Satire=sum(),
                        F_PM_Live_registratie=sum(),
                        F_PM_Videoclips=sum(),
                        F_Actuele_sportinformatie=sum(),
                        F_Cabaret_kleinkunst=sum(),
                        F_Overige_sportreportage=sum(),
                        F_Overige_sportinformatie=sum(),
                        F_Tekstuele_informatie=sum(),                          
                        Voetball=sum(Voetball),
                        F_Weerbericht=sum(F_Weerbericht)
)
#match weather ver
final_veronicatv_3 <- left_join(final_veronicatv_3, weather, by="Date")
final_veronicatv_3$Temp <- final_veronicatv_3$Temp/10
final_veronicatv_3$Rain <- final_veronicatv_3$Rain/100

write.csv(final_veronicatv_3,"final_veronicatv_2.csv")

#### final net5 ####
final_hol <- totalHol[,c(1,3,4)]

final_net5_1 <- left_join(final_net5, final_hol, by="Date")
for(i in 1:nrow(final_net5_1)){
  if(is.na(final_net5_1$Holiday.name[i]) & is.na(final_net5_1$Holiday.type[i])){
    final_net5_1$Holiday.name[i] <- "NH"
    final_net5_1$Holiday.type[i] <- "NH"
  }
}
#BTL FILMS
#OVERIGE NON FICTIE
#OVERIG AMUSEMENT
#Talentenjacht/Auditie/Programma
#KINDERFILMS
#Actualiteiten
#Niews
#Overige muziek: Programa
#Overige muziek: Live registratie
#SPEL & QUIZ
#NLD FILMS
#OVERIGE NON FICTIE
#NLD SERIES
#BTL SERIES
#Nld Series
#Voetbalreportage
#Overige muziek: overig
#Satire
#Populaire muziek: Live registratie
#Populaire muziek: Videoclips
#Actuele sportinformatie
#Cabaret/kleinkunst
#Overige sportreportage
#Overige sportinformatie
#Tekstuele informatie

colnames(final_net5_1) <- c("Date",
                            "Stream.starts","Type..Subject.3..SKO.",
                            "mday","wday","yday","month","day.1",
                            "Channel",
                            "Type..Subject.2..SKO.",
                            "Freq",
                            "TRP..avgw.","Prog.Share",
                            'F_BTL_FILMS',
                            'F_OVERIGE_NON_FICTIE',
                            'F_Overig_Amusement',
                            'F_TAP',
                            'F_KINDERFILMS',
                            'F_Actualiteiten',
                            'F_Nieuws',
                            'F_OM_Programa',
                            'F_OM_Live_registratie',
                            'F_SPEL_QUIZ',
                            'F_NLD_FILMS',
                            'F_NLD_SERIES',
                            'F_BTL_SERIES',
                            'F_Nld_Series',
                            'F_Voetbalreportage',
                            'F_OM_overig',
                            'F_Satire',
                            'F_PM_Live_registratie',
                            'F_PM_Videoclips',
                            'F_Actuele_sportinformatie',
                            'F_Cabaret_kleinkunst',
                            'F_Overige_sportreportage',
                            'F_Overige_sportinformatie',
                            'F_Tekstuele_informatie',
                            'F_Weerbericht',
                            'Voetball',
                            'Holiday.name',
                            'Holiday.type')

final_net5_2 <- subset(final_net5_1,select=c(Date,
                                             Stream.starts,
                                             mday,
                                             wday,
                                             yday,
                                             month,
                                             Prog.Share,
                                             F_BTL_FILMS,
                                             F_OVERIGE_NON_FICTIE,
                                             F_Overig_Amusement,
                                             F_TAP,
                                             F_KINDERFILMS,
                                             F_Actualiteiten,
                                             F_Nieuws,
                                             F_OM_Programa,
                                             F_OM_Live_registratie,
                                             F_SPEL_QUIZ,
                                             F_NLD_FILMS,
                                             F_NLD_SERIES,
                                             F_BTL_SERIES,
                                             F_Nld_Series,
                                             F_Voetbalreportage,
                                             F_OM_overig,
                                             F_Satire,
                                             F_PM_Live_registratie,
                                             F_PM_Videoclips,
                                             F_Actuele_sportinformatie,
                                             F_Cabaret_kleinkunst,
                                             F_Overige_sportreportage,
                                             F_Overige_sportinformatie,
                                             F_Tekstuele_informatie,
                                             F_Weerbericht,
                                             Voetball,
                                             Holiday.name,
                                             Holiday.type)
)
colnames(final_net5_2) <- c('Date',
                            'Stream.starts',
                            'mday',
                            'wday',
                            'yday',
                            'month',
                            'Prog.Share',
                            'F_BTL_FILMS',
                            'F_OVERIGE_NON_FICTIE',
                            'F_Overig_Amusement',
                            'F_TAP',
                            'F_KINDERFILMS',
                            'F_Actualiteiten',
                            'F_Nieuws',
                            'F_OM_Programa',
                            'F_OM_Live_registratie',
                            'F_SPEL_QUIZ',
                            'F_NLD_FILMS',
                            'F_NLD_SERIES',
                            'F_BTL_SERIES',
                            'F_Nld_Series',
                            'F_Voetbalreportage',
                            'F_OM_overig',
                            'F_Satire',
                            'F_PM_Live_registratie',
                            'F_PM_Videoclips',
                            'F_Actuele_sportinformatie',
                            'F_Cabaret_kleinkunst',
                            'F_Overige_sportreportage',
                            'F_Overige_sportinformatie',
                            'F_Tekstuele_informatie',
                            'F_Weerbericht',
                            'Voetball', 
                            'Holiday.name',
                            'Holiday.type')
final_net5_2$Prog.Share <- as.character(final_net5_2$Prog.Share)
final_net5_2$Prog.Share <- gsub(",",".",final_net5_2$Prog.Share)
final_net5_2$Prog.Share <- as.numeric(final_net5_2$Prog.Share)

final_net5_3 <- final_net5_2 %>% group_by(Date,
                                          mday,
                                          wday,
                                          yday,
                                          month,
                                          Holiday.name,
                                          Holiday.type
) %>% dplyr::summarise( n=sum(Stream.starts), 
                        Prog.Share=mean(Prog.Share),
                        F_BTL_FILMS=sum(),
                        F_OVERIGE_NON_FICTIE=sum(),
                        F_Overig_Amusement=sum(),
                        F_TAP=sum(),
                        F_KINDERFILMS=sum(),
                        F_Actualiteiten=sum(),
                        F_Nieuws=sum(),
                        F_OM_Programa=sum(),
                        F_OM_Live_registratie=sum(),
                        F_SPEL_QUIZ=sum(),
                        F_NLD_FILMS=sum(),
                        F_NLD_SERIES=sum(),
                        F_BTL_SERIES=sum(),
                        F_Nld_Series=sum(),
                        F_Voetbalreportage=sum(),
                        F_OM_overig=sum(),
                        F_Satire=sum(),
                        F_PM_Live_registratie=sum(),
                        F_PM_Videoclips=sum(),
                        F_Actuele_sportinformatie=sum(),
                        F_Cabaret_kleinkunst=sum(),
                        F_Overige_sportreportage=sum(),
                        F_Overige_sportinformatie=sum(),
                        F_Tekstuele_informatie=sum(),                          
                        Voetball=sum(Voetball),
                        F_Weerbericht=sum(F_Weerbericht)
)
#match weather net5
final_net5_3 <- left_join(final_net5_3, weather, by="Date")
final_net5_3$Temp <- final_net5_3$Temp/10
final_net5_3$Rain <- final_net5_3$Rain/100

write.csv(final_net5_3,"final_net5_2.csv")

#### final s1 ####
final_hol <- totalHol[,c(1,3,4)]

final_s1_1 <- left_join(final_s1, final_hol, by="Date")
for(i in 1:nrow(final_s1_1)){
  if(is.na(final_s1_1$Holiday.name[i]) & is.na(final_s1_1$Holiday.type[i])){
    final_s1_1$Holiday.name[i] <- "NH"
    final_s1_1$Holiday.type[i] <- "NH"
  }
}
#BTL FILMS
#OVERIGE NON FICTIE
#OVERIG AMUSEMENT
#Talentenjacht/Auditie/Programma
#KINDERFILMS
#Actualiteiten
#Niews
#Overige muziek: Programa
#Overige muziek: Live registratie
#SPEL & QUIZ
#NLD FILMS
#OVERIGE NON FICTIE
#NLD SERIES
#BTL SERIES
#Nld Series
#Voetbalreportage
#Overige muziek: overig
#Satire
#Populaire muziek: Live registratie
#Populaire muziek: Videoclips
#Actuele sportinformatie
#Cabaret/kleinkunst
#Overige sportreportage
#Overige sportinformatie
#Tekstuele informatie

colnames(final_s1_1) <- c("Date",
                          "Stream.starts","Type..Subject.3..SKO.",
                          "mday","wday","yday","month","day.1",
                          "Channel",
                          "Type..Subject.2..SKO.",
                          "Freq",
                          "TRP..avgw.","Prog.Share",
                          'F_BTL_FILMS',
                          'F_OVERIGE_NON_FICTIE',
                          'F_Overig_Amusement',
                          'F_TAP',
                          'F_KINDERFILMS',
                          'F_Actualiteiten',
                          'F_Nieuws',
                          'F_OM_Programa',
                          'F_OM_Live_registratie',
                          'F_SPEL_QUIZ',
                          'F_NLD_FILMS',
                          'F_NLD_SERIES',
                          'F_BTL_SERIES',
                          'F_Nld_Series',
                          'F_Voetbalreportage',
                          'F_OM_overig',
                          'F_Satire',
                          'F_PM_Live_registratie',
                          'F_PM_Videoclips',
                          'F_Actuele_sportinformatie',
                          'F_Cabaret_kleinkunst',
                          'F_Overige_sportreportage',
                          'F_Overige_sportinformatie',
                          'F_Tekstuele_informatie',
                          'F_Weerbericht',
                          'Voetball',
                          'Holiday.name',
                          'Holiday.type')

final_s1_2 <- subset(final_s1_1,select=c(Date,
                                         Stream.starts,
                                         mday,
                                         wday,
                                         yday,
                                         month,
                                         Prog.Share,
                                         F_BTL_FILMS,
                                         F_OVERIGE_NON_FICTIE,
                                         F_Overig_Amusement,
                                         F_TAP,
                                         F_KINDERFILMS,
                                         F_Actualiteiten,
                                         F_Nieuws,
                                         F_OM_Programa,
                                         F_OM_Live_registratie,
                                         F_SPEL_QUIZ,
                                         F_NLD_FILMS,
                                         F_NLD_SERIES,
                                         F_BTL_SERIES,
                                         F_Nld_Series,
                                         F_Voetbalreportage,
                                         F_OM_overig,
                                         F_Satire,
                                         F_PM_Live_registratie,
                                         F_PM_Videoclips,
                                         F_Actuele_sportinformatie,
                                         F_Cabaret_kleinkunst,
                                         F_Overige_sportreportage,
                                         F_Overige_sportinformatie,
                                         F_Tekstuele_informatie,
                                         F_Weerbericht,
                                         Voetball,
                                         Holiday.name,
                                         Holiday.type)
)
colnames(final_s1_2) <- c('Date',
                          'Stream.starts',
                          'mday',
                          'wday',
                          'yday',
                          'month',
                          'Prog.Share',
                          'F_BTL_FILMS',
                          'F_OVERIGE_NON_FICTIE',
                          'F_Overig_Amusement',
                          'F_TAP',
                          'F_KINDERFILMS',
                          'F_Actualiteiten',
                          'F_Nieuws',
                          'F_OM_Programa',
                          'F_OM_Live_registratie',
                          'F_SPEL_QUIZ',
                          'F_NLD_FILMS',
                          'F_NLD_SERIES',
                          'F_BTL_SERIES',
                          'F_Nld_Series',
                          'F_Voetbalreportage',
                          'F_OM_overig',
                          'F_Satire',
                          'F_PM_Live_registratie',
                          'F_PM_Videoclips',
                          'F_Actuele_sportinformatie',
                          'F_Cabaret_kleinkunst',
                          'F_Overige_sportreportage',
                          'F_Overige_sportinformatie',
                          'F_Tekstuele_informatie',
                          'F_Weerbericht',
                          'Voetball', 
                          'Holiday.name',
                          'Holiday.type')
final_s1_2$Prog.Share <- as.character(final_s1_2$Prog.Share)
final_s1_2$Prog.Share <- gsub(",",".",final_s1_2$Prog.Share)
final_s1_2$Prog.Share <- as.numeric(final_s1_2$Prog.Share)

final_s1_3 <- final_s1_2 %>% group_by(Date,
                                      mday,
                                      wday,
                                      yday,
                                      month,
                                      Holiday.name,
                                      Holiday.type
) %>% dplyr::summarise( n=sum(Stream.starts), 
                        Prog.Share=mean(Prog.Share),
                        F_BTL_FILMS=sum(),
                        F_OVERIGE_NON_FICTIE=sum(),
                        F_Overig_Amusement=sum(),
                        F_TAP=sum(),
                        F_KINDERFILMS=sum(),
                        F_Actualiteiten=sum(),
                        F_Nieuws=sum(),
                        F_OM_Programa=sum(),
                        F_OM_Live_registratie=sum(),
                        F_SPEL_QUIZ=sum(),
                        F_NLD_FILMS=sum(),
                        F_NLD_SERIES=sum(),
                        F_BTL_SERIES=sum(),
                        F_Nld_Series=sum(),
                        F_Voetbalreportage=sum(),
                        F_OM_overig=sum(),
                        F_Satire=sum(),
                        F_PM_Live_registratie=sum(),
                        F_PM_Videoclips=sum(),
                        F_Actuele_sportinformatie=sum(),
                        F_Cabaret_kleinkunst=sum(),
                        F_Overige_sportreportage=sum(),
                        F_Overige_sportinformatie=sum(),
                        F_Tekstuele_informatie=sum(),                          
                        Voetball=sum(Voetball),
                        F_Weerbericht=sum(F_Weerbericht)
)
#match weather s1
final_s1_3 <- left_join(final_s1_3, weather, by="Date")
final_s1_3$Temp <- final_s1_3$Temp/10
final_s1_3$Rain <- final_s1_3$Rain/100

write.csv(final_s1_3,"final_s1_2.csv")


#### import sanoma data SS####
#2015
sanoma150103 <- read.csv2("sanomaSS150103.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma150406 <- read.csv2("sanomaSS150406.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma150709 <- read.csv2("sanomaSS150709.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma151012 <- read.csv2("sanomaSS151012.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
#2016
sanoma160102 <- read.csv2("sanomaSS160102.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma160304 <- read.csv2("sanomaSS160304.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")

totalSanoma <- rbind(sanoma150103,sanoma150406,sanoma150709,sanoma151012,sanoma160102,sanoma160304)
totalSanoma$Day <- as.character(totalSanoma$Day)
totalSanoma$ns_st_cu <- as.character(totalSanoma$ns_st_cu)
totalSanoma$Stream.starts <- as.character(totalSanoma$Stream.starts)
totalSanoma$Stream.starts <- gsub("\\.","",totalSanoma$Stream.starts)
totalSanoma$Stream.starts <- as.numeric(totalSanoma$Stream.starts)

#538 Groep: radio 538, SLAM fm, juize, tv 538--- check to see which links, as now we miss data from this one
final_libelle <- subset(totalSanoma, grepl("libelle",totalSanoma$ns_st_cu), select = c(1,2,3))
final_538G <- subset(totalSanoma,grepl("538",totalSanoma$ns_st_cu)|grepl("slam",totalSanoma$ns_st_cu), select = c(1,2,3))
finalSanomaNL <- subset(totalSanoma,!(grepl("libelle",totalSanoma$ns_st_cu)|grepl("538",totalSanoma$ns_st_cu)|grepl("slam",totalSanoma$ns_st_cu)), select = c(1,2,3))

#### import sanoma inventory data####
san_inv1 <- read.csv2("SanomaInv_1605.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv2 <- read.csv2("SanomaInv_160304.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv3 <- read.csv2("SanomaInv_160102.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv4 <- read.csv2("SanomaInv_151112.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv5 <- read.csv2("SanomaInv_150910.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv6 <- read.csv2("SanomaInv_150708.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv7 <- read.csv2("SanomaInv_150506.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv8 <- read.csv2("SanomaInv_150304.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
san_inv9 <- read.csv2("SanomaInv_150102.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma_inventory <- rbind(san_inv9, san_inv8, san_inv7, san_inv6, san_inv5, san_inv4, san_inv3, san_inv2, san_inv1)
sanoma_inventory <- sanoma_inventory[,c(1,3,4,5)]

sanoma_inventory$Inventory <- as.character(sanoma_inventory$Inventory)
sanoma_inventory$Date <- as.character(sanoma_inventory$Date)
sanoma_inventory$Category.1 <- as.character(sanoma_inventory$Category.1)
sanoma_inventory$Format.type <- as.character(sanoma_inventory$Format.type)
sanoma_inventory$Inventory <- gsub("\\.","",sanoma_inventory$Inventory)
sanoma_inventory$Date <- dmy(sanoma_inventory$Date)
sanoma_inventory$Inventory <- as.numeric(sanoma_inventory$Inventory)

#join Inpage inventory and make calculation
inpage_inventory <- read.csv2("inpage_inventory.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
inpage_inventory <- inpage_inventory[,c(1,3,5)]
inpage_inventory$Date <- as.character(inpage_inventory$Date)
inpage_inventory$Date <- dmy(inpage_inventory$Date)
inpage_inventory$Inventory <- as.character(inpage_inventory$Inventory)
inpage_inventory$Inventory <- gsub("\\.","",inpage_inventory$Inventory)
inpage_inventory$Inventory <- as.numeric(inpage_inventory$Inventory)
inpage_inventory$Category.1 <- as.character(inpage_inventory$Category.1)
inpage_inventory <- group_by(inpage_inventory,Date, Category.1)
inpage_inventory <- summarize(inpage_inventory, Inventory=sum(Inventory))

sanoma_inventory <- subset(sanoma_inventory,sanoma_inventory$Format.type=="Preroll" |sanoma_inventory$Format.type=="Midroll" |sanoma_inventory$Format.type=="Postroll",select=c(1,2,3,4))
sanoma_inventory <- left_join(sanoma_inventory, inpage_inventory, by=c("Date","Category.1"))
sanoma_inventory$invTot <- 0
for(i in 1:nrow(sanoma_inventory)){
  if(!(is.na(sanoma_inventory$Inventory.y[i])==TRUE)&sanoma_inventory$Format.type[i]=="Preroll"){
    sanoma_inventory$invTot[i] <- sanoma_inventory$Inventory.x[i] - sanoma_inventory$Inventory.y[i]
  }
}
for(i in 1:nrow(sanoma_inventory)){
  if(sanoma_inventory$invTot[i]==0){
    sanoma_inventory$invTot[i] <- sanoma_inventory$Inventory.x[i]
  }
}
sanoma_inventory <- sanoma_inventory[,c(1,2,3,6)]
sanoma_inventory_summed <- group_by(sanoma_inventory, Date, Category.1)
sanoma_inventory_summed <- summarize(sanoma_inventory_summed,invTot=sum(invTot))

#plot to see which brands to keep
plot1 <- plot_ly(x=sanoma_inventory_summed$Date,y=sanoma_inventory_summed$invTot, color = sanoma_inventory_summed$Category.1)
plot1

#filtered sanoma inventory to only 6 highest groups
sanoma_inventory_filt <- subset(sanoma_inventory,(sanoma_inventory$Format.type!="Midroll")&(sanoma_inventory$Category.1=="Nu.nl" 
                                                                                            |sanoma_inventory$Category.1=="Unassigned" 
                                                                                            |sanoma_inventory$Category.1=="www.autoweek.nl"
                                                                                            |sanoma_inventory$Category.1=="www.zie.nl"
                                                                                            |sanoma_inventory$Category.1=="Libelle.nl"
                                                                                            |sanoma_inventory$Category.1=="wtf.nl")
                                ,select=c(1,2,3,4))
sanoma_inventory_filt <- group_by(sanoma_inventory_filt, Date, Category.1, Format.type)
sanoma_inventory_filt <- summarize(sanoma_inventory_filt, invTot=sum(invTot))
sanoma_inventory_filt <- subset(sanoma_inventory_filt, !(sanoma_inventory_filt$Category.1=="Unassigned" & sanoma_inventory_filt$Format.type=="Postroll"), select = c(1,2,3,4))
sanoma_inventory_filt_pre <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Format.type=="Preroll"), select = c(1,2,4))
sanoma_inventory_filt_post <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Format.type=="Postroll"), select = c(1,2,4))

sanoma_inventory_filt$year <- year(sanoma_inventory_filt$Date)
sanoma_inventory_filt$yday <- yday(sanoma_inventory_filt$Date)
sanoma_inventory_filt$wday <- wday(sanoma_inventory_filt$Date)
sanoma_inventory_filt$month <- month(sanoma_inventory_filt$Date)
sanoma_inventory_filt$week <- week(sanoma_inventory_filt$Date)

#### sanoma brands ####
#nu
nu_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Nu.nl"), select = c(1,3,4,5,6,7,8,9))
#libelle
libelleSan_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Libelle.nl"), select = c(1,3,4,5,6,7,8,9))
#wtf
wtf_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="wtf.nl"), select = c(1,3,4,5,6,7,8,9))
#zie
zie_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="www.zie.nl"), select = c(1,3,4,5,6,7,8,9))
#autoweek
autoweek_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="www.autoweek.nl"), select = c(1,3,4,5,6,7,8,9))
#unassigned
unassignedSan_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Unassigned"), select = c(1,3,4,5,6,7,8,9))

#total version
#nu
nu_inv_tot <- group_by(nu_inv, Date, yday, year, wday, month, week)
nu_inv_tot <- summarize(nu_inv_tot, invTot=sum(invTot))
#libelle
libelle_inv_tot <- group_by(libelleSan_inv, Date, yday, year, wday, month, week)
libelle_inv_tot <- summarize(libelle_inv_tot, invTot=sum(invTot))
#wtf
wtf_inv_tot <- group_by(wtf_inv, Date, yday, year, wday, month, week)
wtf_inv_tot <- summarize(wtf_inv_tot, invTot=sum(invTot))
#zie
zie_inv_tot <- group_by(zie_inv, Date, yday, year, wday, month, week)
zie_inv_tot <- summarize(zie_inv_tot, invTot=sum(invTot))
#autoweek
autoweek_inv_tot <- group_by(autoweek_inv, Date, yday, year, wday, month, week)
autoweek_inv_tot <- summarize(autoweek_inv_tot, invTot=sum(invTot))
#unassigned
unassignedSan_inv_tot <- group_by(unassignedSan_inv, Date, yday, year, wday, month, week)
unassignedSan_inv_tot <- summarize(unassignedSan_inv_tot, invTot=sum(invTot))

#preroll version
#nu
nu_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Nu.nl"), select = c(1,3,4))
#libelle
libelleSan_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Libelle.nl"), select = c(1,3,4))
#wtf
wtf_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="wtf.nl"), select = c(1,3,4))
#zie
zie_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="www.zie.nl"), select = c(1,3,4))
#autoweek
autoweek_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="www.autoweek.nl"), select = c(1,3,4))
#unassigned
unassignedSan_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Unassigned"), select = c(1,3,4))

#postroll version
#nu
nu_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Nu.nl"), select = c(1,3,4))
#libelle
libelleSan_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Libelle.nl"), select = c(1,3,4))
#wtf
wtf_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="wtf.nl"), select = c(1,3,4))
#zie
zie_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="www.zie.nl"), select = c(1,3,4))
#autoweek
autoweek_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="www.autoweek.nl"), select = c(1,3,4))
#unassigned
unassignedSan_inv <- subset(sanoma_inventory_filt, (sanoma_inventory_filt$Category.1=="Unassigned"), select = c(1,3,4))

#### import sbs inventory data ####
sbs_inventory1 <- read.csv2("sbs_inventory_1501.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory2 <- read.csv2("sbs_inventory_1502.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory3 <- read.csv2("sbs_inventory_1503.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory4 <- read.csv2("sbs_inventory_1504.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory5 <- read.csv2("sbs_inventory_1505.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory6 <- read.csv2("sbs_inventory_1506.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory7 <- read.csv2("sbs_inventory_1507.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory8 <- read.csv2("sbs_inventory_1508.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory9 <- read.csv2("sbs_inventory_1509.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory10 <- read.csv2("sbs_inventory_1510.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory11 <- read.csv2("sbs_inventory_1511.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory12 <- read.csv2("sbs_inventory_1512.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory13 <- read.csv2("sbs_inventory_1601.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory14 <- read.csv2("sbs_inventory_1602.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory15 <- read.csv2("sbs_inventory_1603.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory16 <- read.csv2("sbs_inventory_1604.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inventory17 <- read.csv2("sbs_inventory_1605half.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sbs_inv_total <- rbind(sbs_inventory1, sbs_inventory17, sbs_inventory16, sbs_inventory15, sbs_inventory15, sbs_inventory14, sbs_inventory13, sbs_inventory12, sbs_inventory11, sbs_inventory10,
                       sbs_inventory9, sbs_inventory8, sbs_inventory7, sbs_inventory6, sbs_inventory5, sbs_inventory4, sbs_inventory3, sbs_inventory2)

#### sbs6 inventory dataset ####
sbs6_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="sbs6"), select = c(1,4,5,6))
#### sbs9 inventory dataset ####
sbs9_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="sbs9"), select = c(1,4,5,6))
#### net5 inventory dataset ####
net5_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="net5"), select = c(1,4,5,6))
#### veronicatv inventory dataset ####
veronica_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="veronicatv"), select = c(1,4,5,6))
#### 538 groep inventory dataset ####
G538_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="538 Groep"), select = c(1,4,5,6))
#### libelle inventory dataset ####
libelle_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="libelle"), select = c(1,4,5,6))
#### s1 inventory dataset ####
s1_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="s1"), select = c(1,4,5,6))
#### Unassigned inventory dataset ####
unassigned_inv <- subset(sbs_inv_total, (sbs_inv_total$Category.1=="Unassigned"), select = c(1,4,5,6))

#### group inventory datasets####
#### 538groep ####
G538_inv$Date <- as.character(G538_inv$Date)
G538_inv$Date <- dmy(G538_inv$Date)
G538_inv$Inventory <- as.character(G538_inv$Inventory)
G538_inv$Inventory <- gsub("\\.","",G538_inv$Inventory)
G538_inv$Inventory <- as.numeric(G538_inv$Inventory)
G538_inv <- group_by(G538_inv, Date, Format.type)
G538_inv <- summarize(G538_inv, Inventory=sum(Inventory))
#split into pre-mid-post roll
G538_inv_pre <- subset(G538_inv, (G538_inv$Format.type=="Preroll"), select = c(1,3))
G538_inv_mid <- subset(G538_inv, (G538_inv$Format.type=="Midroll"), select = c(1,3))
G538_inv_post <- subset(G538_inv, (G538_inv$Format.type=="Postroll"), select = c(1,3))

####Libelle ####
libelle_inv$Date <- as.character(libelle_inv$Date)
libelle_inv$Date <- dmy(libelle_inv$Date)
libelle_inv$Inventory <- as.character(libelle_inv$Inventory)
libelle_inv$Inventory <- gsub("\\.","",libelle_inv$Inventory)
libelle_inv$Inventory <- as.numeric(libelle_inv$Inventory)
libelle_inv <- group_by(libelle_inv, Date, Format.type)
libelle_inv <- summarize(libelle_inv, Inventory=sum(Inventory))
#split into pre-mid-post roll
libelle_inv_pre <- subset(libelle_inv, (libelle_inv$Format.type=="Preroll"), select = c(1,3))
#libelle_inv_mid <- subset(libelle_inv, (libelle_inv$Format.type=="Midroll"), select = c(1,3))
libelle_inv_post <- subset(libelle_inv, (libelle_inv$Format.type=="Postroll"), select = c(1,3))

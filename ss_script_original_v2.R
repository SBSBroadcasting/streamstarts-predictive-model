library(lubridate)
library(dplyr)

####read AdEdge tv data####
tvTotal1516 <- read.csv2("TvProg1516.csv", header = TRUE, sep = ",", quote = "\"", dec = ",", fill = TRUE, comment.char = "",stringsAsFactors = FALSE,na.strings = "NA")
#tvTotal1516 <- tvTotal1516[,-c(8)]
tvTotal1516$TRP..avgW. <- as.numeric(tvTotal1516$TRP..avgW.)
tvTotal1516$Prog.Share <- as.numeric(tvTotal1516$Prog.Share)
tvTotal1516$Main.Title <- tolower(tvTotal1516$Main.Title)
tvTotal1516$Main.Title <- gsub(" ","",tvTotal1516$Main.Title)
tvTotal1516$Date <- dmy(tvTotal1516$Date)

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
#ADD PATCHES FOR PERIOD:MARCH16-APRIL16
#----FOR SBS6,NET5,SBS9,KIJK,VERONICATV,S1TV ---#
#patch160304 <- patch160304[,-c(6,7)]
colnames(patch160304) <- c("V1","V2","V3","V4","V5")
patch160304$V1 <- dmy(patch160304$V1)
patch160304$V5 <- as.character(patch160304$V5)
patch160304$V5 <- gsub('\\.',"",patch160304$V5)
patch160304$V5 <- as.numeric(patch160304$V5)


#----RBIND PATCH WITH TOTAL----#
OnlineTotal <- rbind(OnlineTotal,patch160304)

OnlineTotal$V2 <- gsub("kijk-app","kijk", OnlineTotal$V2)
OnlineTotal$V2 <- gsub("kijk-embed","kijk", OnlineTotal$V2)

#### Fix Titles #####
OnlineTotal$V4 <- gsub(" ","", OnlineTotal$V4)
OnlineTotal$V4 <- tolower(OnlineTotal$V4)
OnlineTotal$V4 <- gsub("\\'","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("%","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("-","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("_","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub(":","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[,]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("\\.","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[+]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[!]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[?]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[#]","", OnlineTotal$V4)
OnlineTotal$V4 <- gsub("[']","",OnlineTotal$V4)
OnlineTotal$V4 <- gsub('["]',"",OnlineTotal$V4)
OnlineTotal$V4<- gsub('[%]+e',"",OnlineTotal$V4)
OnlineTotal$V4 <- gsub("7het0was0net0een0tsunami0die0op0me0af0kwam7","hetwasneteentsunamidieopmeafkwam",OnlineTotal$V4)

OnlineTotal <- OnlineTotal[!grepl("aflevering", OnlineTotal$V4),]
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("seizoen",V4)==TRUE,substr(V4, 1, nchar(V4)-8),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^de",V4)==TRUE,substr(V4, 3, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^the",V4)==TRUE,substr(V4, 4, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^\\d{2}",V4)==TRUE,substr(V4, 3, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^\\d{1}",V4)==TRUE,substr(V4, 2, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("0dagenvanwillemalexanderenmc3a1xima",V4)==TRUE,"1000dagenvanwillemalexanderenmaxima",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("0dagenvanwillemalexanderenmã¡xima",V4)==TRUE,"1000dagenvanwillemalexanderenmaxima",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("0dagenvanwillemalexanderenmãƒâ¡xima",V4)==TRUE,"1000dagenvanwillemalexanderenmaxima",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^programmas",V4)==TRUE,substr(V4, 11, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("vod$",V4)==TRUE,substr(V4, 1, nchar(V4)-10),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("[&]",V4)==TRUE,"en",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("k2zoektk3",V4)==TRUE,"k3zoektk3",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("yayoga",V4)==TRUE,"yayoga",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("achtergeslotendeuren",V4)==TRUE,"achtergeslotendeuren",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("bigbangtheoryse",V4)==TRUE,"bigbangtheory",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("tishierfantasties",V4)==TRUE,"ishierfantasties",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("s1ngle",V4)==TRUE,"single",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("^s1",V4)==TRUE,substr(V4, 3, nchar(V4)),V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("shownieuws",V4)==TRUE,"shownieuws",V4))
OnlineTotal <- mutate(OnlineTotal,V4=ifelse(grepl("shanghai",V4)==TRUE,"shanghainoon",V4))

colnames(OnlineTotal) <- c("Date","sourcesite","sbs_partner","Title","n")
OnlineTotal <- OnlineTotal[!(OnlineTotal$Title == ""),]
OnlineTotal <- OnlineTotal[!(OnlineTotal$Title == "_uimodalitemappviewcontroller"|OnlineTotal$Title=="_uialertshimpresentingviewcontroller"
                             |OnlineTotal$Title=="20"
                             |OnlineTotal$Title=="25"),]
OnlineTotal <- OnlineTotal[!grepl("kijk",OnlineTotal$Title)|grepl("net5",OnlineTotal$Title),]

#### group by & summarize by date & Channel ####
OnlineTotal <- group_by(OnlineTotal, Date, sourcesite,sbs_partner, Title)
OnlineTotal <- summarise(OnlineTotal, n=sum(n))
OnlineTotal <- left_join(OnlineTotal, totalHol, by="Date")
OnlineTotal <- OnlineTotal[,c(1,2,3,4,5,7)]

####Set genreTbl for genre matching#####
genreTbl <- subset(tvTotal1516, select = c(3,4))
genreTbl <- group_by(genreTbl, Main.Title, Type..Subject.3..SKO.)
genreTbl <- summarise(genreTbl, n=n())
genreTbl <- unique(genreTbl)
genreTbl <- genreTbl[,-c(3)]
colnames(genreTbl) <- c("Title","Type..Subject.3..SKO.")

####Match/join tv titles wth video title programmes and fill the genres ####
OnlineTotal <- left_join(OnlineTotal, genreTbl, by = "Title", all = TRUE)
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("film",Title)==TRUE,"BTL FILMS",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("veronicamusic",Title)==TRUE,"OVERIGE NON FICTIE",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("achtergeslotendeuren",Title)==TRUE,"NLD SERIES",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("hartvannederlandlaat",Title)==TRUE,"Actualiteiten",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("hartvannederlandvroeg",Title)==TRUE,"Actualiteiten",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("masterchef",Title)==TRUE,"OVERIGE NON FICTIE",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("veronicaformule1",Title)==TRUE,"Overige sportreportage",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("uefachampionsleague",Title)==TRUE,"Voetball",Type..Subject.3..SKO.))
OnlineTotal <- mutate(OnlineTotal,Type..Subject.3..SKO.=ifelse(grepl("voetbalsbs6",Title)==TRUE,"Voetball",Type..Subject.3..SKO.))

OnlineTotal <- OnlineTotal[!is.na(OnlineTotal$Type..Subject.3..SKO.),]
#### Match new episodes dates with online date ####
OnlineTotal$mday <- mday(OnlineTotal$Date)
OnlineTotal$wday <- wday(OnlineTotal$Date)
OnlineTotal$yday <- yday(OnlineTotal$Date)
OnlineTotal$month <- month(OnlineTotal$Date)
OnlineTotal$year <- year(OnlineTotal$Date)

OnlineTotal$`day+1` <- OnlineTotal$yday+1
colnames(tvTotal1516) <- c("Date","Channel","Title","Type..Subject.3..SKO.","Type..Frequency","TRP..avgW.","Prog.Share","Holiday.name")
OnlineTotal <- left_join(OnlineTotal, tvTotal1516[,c(1,2,3,5,7,8)], by=c("Date","Title"))
OnlineTotal <- OnlineTotal[!(is.na(OnlineTotal$Date)|OnlineTotal$Title=="kykslidemenuviewcontroller"),]
OnlineTotal <- OnlineTotal[,-c(16)]


# [1] <NA>                               BTL FILMS                          OVERIGE NON FICTIE                 Overig amusement                  
#[5] Nld series                         BTL SERIES                         Talentenjacht/Auditie/Programma    KINDERFILMS                       
#[9] NLD SERIES                         Overige non fictie                 SPEL & QUIZ                        NLD FILMS                         
#[13] Actualiteiten                      Nieuws                             Overige muziek: Programa           Overige muziek: Live registratie  
#[17] Voetbalreportage                   Overige muziek: overig             Satire                             Weerbericht                       
#[21] Populaire muziek: Live registratie Populaire muziek: Videoclips       Actuele sportinformatie            Cabaret/kleinkunst                
#[25] Overige sportreportage             Overige sportinformatie            Tekstuele informatie  

OnlineTotal$F_BTL_FILMS <- 0
OnlineTotal$F_OVERIGE_NON_FICTIE <- 0
OnlineTotal$F_Overig_amusement <- 0
OnlineTotal$F_TAP <- 0
OnlineTotal$F_KINDERFILMS <- 0
OnlineTotal$F_Actualiteiten <- 0
OnlineTotal$F_Nieuws <- 0
OnlineTotal$F_OM_Programa <- 0
OnlineTotal$F_OM_Live_registratie <- 0
OnlineTotal$F_SPEL_QUIZ <- 0
OnlineTotal$F_NLD_FILMS <- 0 
OnlineTotal$F_NLD_SERIES <- 0
OnlineTotal$F_BTL_SERIES <- 0
OnlineTotal$F_Nld_series <- 0
OnlineTotal$F_Voetbalreportage <- 0
OnlineTotal$F_OM_overig <- 0
OnlineTotal$F_Satire <- 0
OnlineTotal$F_PM_Live_registratie <- 0
OnlineTotal$F_PM_Videoclips <- 0
OnlineTotal$F_Actuele_sportinformatie <- 0
OnlineTotal$F_Cabaret_kleinkunst <- 0
OnlineTotal$F_Overige_sportreportage <- 0
OnlineTotal$F_Overige_sportinformatie <- 0
OnlineTotal$F_Tekstuele_informatie <- 0
OnlineTotal$F_Weerbericht <- 0
OnlineTotal$Voetball <- 0 

for(i in 1:nrow(OnlineTotal)){
  if(ifelse(!is.na(OnlineTotal$Type..Frequency[i]),OnlineTotal$Type..Frequency[i],"foobar") =="EERSTE UITZENDING"){
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="BTL FILMS"){
      OnlineTotal$F_BTL_FILMS[i] <- 1
    }
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="OVERIGE NON FICTIE"){
      OnlineTotal$F_OVERIGE_NON_FICTIE[i] <- 1
    }
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Overig amusement"){
      OnlineTotal$F_Overig_amusement[i] <- 1
    }
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Talentenjacht/Auditie/Programma"){
      OnlineTotal$F_TAP[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="KINDERFILMS"){
      OnlineTotal$F_KINDERFILMS[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Actualiteiten"){
      OnlineTotal$F_Actualiteiten[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Nieuws"){
      OnlineTotal$F_Nieuws[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: Programa"){
      OnlineTotal$F_OM_Programa[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: Live registratie"){
      OnlineTotal$F_OM_Live_registratie[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="SPEL & QUIZ"){
      OnlineTotal$F_SPEL_QUIZ[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="NLD FILMS"){
      OnlineTotal$F_NLD_FILMS[i] <- 1 
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="NLD SERIES"){
      OnlineTotal$F_NLD_SERIES[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="BTL SERIES"){
      OnlineTotal$F_BTL_SERIES[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Nld series"){
      OnlineTotal$F_Nld_series[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Voetbalreportage"){
      OnlineTotal$F_Voetbalreportage[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: overig"){
      OnlineTotal$F_OM_overig[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Satire"){
      OnlineTotal$F_Satire[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Overige muziek: Live registratie"){
      OnlineTotal$F_PM_Live_registratie[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Populaire muziek: Videoclips"){
      OnlineTotal$F_PM_Videoclips[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Actuele sportinformatie"){
      OnlineTotal$F_Actuele_sportinformatie[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Cabaret/kleinkunst"){
      OnlineTotal$F_Cabaret_kleinkunst[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Overige sportreportage"){
      OnlineTotal$F_Overige_sportreportage[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Overige sportinformatie"){
      OnlineTotal$F_Overige_sportinformatie[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Tekstuele informatie"){
      OnlineTotal$F_Tekstuele_informatie[i] <- 1
    }
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Weerbericht"){
      OnlineTotal$F_Weerbericht[i] <- 1
    } 
    if(ifelse(!is.na(OnlineTotal$Type..Subject.3..SKO.[i]),OnlineTotal$Type..Subject.3..SKO.[i],"foobar")=="Voetball"){
      OnlineTotal$Voetball[i] <- 1
    } 
  }}
#### export final tables for each channel####

#### get tw table and create future table ####
futureTbl <- tw[as.Date(tw$Date) >= Sys.Date(),] 
#totalHol$Date <- ymd(totalHol$Date)
#futureTbl$Date <- ymd(futureTbl$Date)
futureTbl <- right_join(totalHol,futureTbl)

for(i in 1:nrow(futureTbl)){
  if(is.na(futureTbl$Holiday.name[i])==TRUE){
    futureTbl$Holiday.name[i] <- "NH"
  }
}
futureTbl <- futureTbl[,c(1,3,5,6)]
colnames(futureTbl) <- c("Date","Holiday.name.x","Temp","Rain")
futureTbl$mday <- mday(futureTbl$Date)
futureTbl$wday <- wday(futureTbl$Date)
futureTbl$yday <- yday(futureTbl$Date)
futureTbl$month <- month(futureTbl$Date)
futureTbl$year <- year(futureTbl$Date)
futureTbl$n <- 0
futureTbl$F_BTL_FILMS <- 0
futureTbl$F_OVERIGE_NON_FICTIE <- 0
futureTbl$F_Overig_amusement<- 0
futureTbl$F_TAP <- 0
futureTbl$F_KINDERFILMS <- 0
futureTbl$F_Actualiteiten <- 0
futureTbl$F_Nieuws <- 0
futureTbl$F_OM_Programa<- 0
futureTbl$F_OM_Live_registratie <- 0
futureTbl$F_SPEL_QUIZ <- 0
futureTbl$F_NLD_FILMS<- 0
futureTbl$F_NLD_SERIES<- 0
futureTbl$F_BTL_SERIES <- 0
futureTbl$F_Nld_series <- 0
futureTbl$F_Voetbalreportage <- 0
futureTbl$F_OM_overig<- 0
futureTbl$F_Satire <- 0
futureTbl$F_PM_Live_registratie <- 0
futureTbl$F_PM_Videoclips <- 0
futureTbl$F_Actuele_sportinformatie <- 0
futureTbl$F_Cabaret_kleinkunst <- 0
futureTbl$F_Overige_sportreportage <- 0
futureTbl$F_Overige_sportinformatie <- 0
futureTbl$F_Tekstuele_informatie <- 0
futureTbl$F_Weerbericht <- 0
futureTbl$Voetball <- 0
futureTbl <- mutate(futureTbl,Temp=ifelse(is.na(Temp)==TRUE,mean(Temp),Temp))
futureTbl <- mutate(futureTbl,Rain=ifelse(is.na(Rain)==TRUE,mean(Rain),Rain))


####sbs6 pre ####
final_sbs6 <- subset(OnlineTotal,(OnlineTotal$sourcesite == "sbs6" & OnlineTotal$sbs_partner == "n/a"), select = c(1,5:13,17:42)) 
final_sbs6 <- mutate(final_sbs6,Type..Subject.3..SKO.=ifelse(is.na(Type..Subject.3..SKO.)==TRUE|Type..Subject.3..SKO.=='',"NA",Type..Subject.3..SKO.))
final_sbs6 <- mutate(final_sbs6,Holiday.name.x=ifelse(Holiday.name.x==""|is.na(Holiday.name.x)==TRUE,"NH",Holiday.name.x))

final_sbs6 <- group_by(final_sbs6,Date,Holiday.name.x,mday,wday,yday,month,year,`day+1`)
final_sbs6 <- summarize(final_sbs6,n=sum(n),
                        F_BTL_FILMS=sum(F_BTL_FILMS),
                        F_OVERIGE_NON_FICTIE=sum(F_OVERIGE_NON_FICTIE),
                        F_Overig_amusement=sum(F_Overig_amusement),
                        F_TAP=sum(F_TAP),
                        F_KINDERFILMS=sum(F_KINDERFILMS),
                        F_Actualiteiten=sum(F_Actualiteiten),
                        F_Nieuws=sum(F_Nieuws),
                        F_OM_Programa=sum(F_OM_Programa),
                        F_OM_Live_registratie=sum(F_OM_Live_registratie),
                        F_SPEL_QUIZ=sum(F_SPEL_QUIZ),
                        F_NLD_FILMS=sum(F_NLD_FILMS),
                        F_NLD_SERIES=sum(F_NLD_SERIES),
                        F_BTL_SERIES=sum(F_BTL_SERIES),
                        F_Nld_series=sum(F_Nld_series),
                        F_Voetbalreportage=sum(F_Voetbalreportage),
                        F_OM_overig=sum(F_OM_overig),
                        F_Satire=sum(F_Satire),
                        F_PM_Live_registratie=sum(F_PM_Live_registratie),
                        F_PM_Videoclips=sum(F_PM_Videoclips),
                        F_Actuele_sportinformatie=sum(F_Actuele_sportinformatie),
                        F_Cabaret_kleinkunst=sum(F_Cabaret_kleinkunst),
                        F_Overige_sportreportage=sum(F_Overige_sportreportage),
                        F_Overige_sportinformatie=sum(F_Overige_sportinformatie),
                        F_Tekstuele_informatie=sum(F_Tekstuele_informatie),
                        F_Weerbericht=sum(F_Weerbericht),
                        Voetball=sum(Voetball)
                        )
#### sbs6 ####
for(i in nrow(final_sbs6):2){
  #BTL FILMS
  final_sbs6$F_BTL_FILMS[i]=final_sbs6$F_BTL_FILMS[i-1]
  #OVERIGE NON FICTIE
  final_sbs6$F_OVERIGE_NON_FICTIE[i]=final_sbs6$F_OVERIGE_NON_FICTIE[i-1]
  # #OVERIG AMUSEMENT
  final_sbs6$F_Overig_amusement[i]=final_sbs6$F_Overig_amusement[i-1]
  # #Talentenjacht/Auditie/Programma
  final_sbs6$F_TAP[i]=final_sbs6$F_TAP[i-1]
  # #KINDERFILMS
  final_sbs6$F_KINDERFILMS[i]=final_sbs6$F_KINDERFILMS[i-1]
  # #Actualiteiten
  final_sbs6$F_Actualiteiten[i]=final_sbs6$F_Actualiteiten[i-1]
  # #Nieuws
  final_sbs6$F_Nieuws[i]=final_sbs6$F_Nieuws[i-1]
  # #Overige muziek: Programa
  final_sbs6$F_OM_Programa[i]=final_sbs6$F_OM_Programa[i-1]
  # #Overige muziek: Live registratie
  final_sbs6$F_OM_Live_registratie[i]=final_sbs6$F_OM_Live_registratie[i-1]
  # #SPEL & QUIZ
  final_sbs6$F_SPEL_QUIZ[i]=final_sbs6$F_SPEL_QUIZ[i-1]
  # #NLD FILMS
  final_sbs6$F_NLD_FILMS[i]=final_sbs6$F_NLD_FILMS[i-1]
  # #NLD SERIES
  final_sbs6$F_NLD_SERIES[i]=final_sbs6$F_NLD_SERIES[i-1]
  # #BTL SERIES
  final_sbs6$F_BTL_SERIES[i]=final_sbs6$F_BTL_SERIES[i-1]
  # #Nld Series
  final_sbs6$F_Nld_series[i]=final_sbs6$F_Nld_series[i-1]
  # #Voetbalreportage
  final_sbs6$F_Voetbalreportage[i]=final_sbs6$F_Voetbalreportage[i-1]
  # #Overige muziek: overig
  final_sbs6$F_OM_overig[i]=final_sbs6$F_OM_overig[i-1]
  # #Satire
  final_sbs6$F_Satire[i]=final_sbs6$F_Satire[i-1]
  # #Populaire muziek: Live registratie
  final_sbs6$F_PM_Live_registratie[i]=final_sbs6$F_PM_Live_registratie[i-1]
  # #Populaire muziek: Videoclips
  final_sbs6$F_PM_Videoclips[i]=final_sbs6$F_PM_Videoclips[i-1]
  # #Actuele sportinformatie
  final_sbs6$F_Actuele_sportinformatie[i]=final_sbs6$F_Actuele_sportinformatie[i-1]
  # #Cabaret/kleinkunst
  final_sbs6$F_Cabaret_kleinkunst[i]=final_sbs6$F_Cabaret_kleinkunst[i-1]
  # #Overige sportreportage
  final_sbs6$F_Overige_sportreportage[i]=final_sbs6$F_Overige_sportreportage[i-1]
  # #Overige sportinformatie
  final_sbs6$F_Overige_sportinformatie[i]=final_sbs6$F_Overige_sportinformatie[i-1]
  # #Tekstuele informatie
  final_sbs6$F_Tekstuele_informatie[i]=final_sbs6$F_Tekstuele_informatie[i-1]
  # #Weerbericht
  final_sbs6$F_Weerbericht[i]=final_sbs6$F_Weerbericht[i-1]
  # #Voetball
  final_sbs6$Voetball[i]=final_sbs6$Voetball[i-1]
  
}
final_sbs6$F_BTL_FILMS[final_sbs6$yday==1] <- 0
final_sbs6$F_OVERIGE_NON_FICTIE[final_sbs6$yday==1] <- 0
final_sbs6$F_Overig_amusement[final_sbs6$yday==1] <- 0
final_sbs6$F_TAP[final_sbs6$yday==1] <- 0
final_sbs6$F_KINDERFILMS[final_sbs6$yday==1] <- 0
final_sbs6$F_Actualiteiten[final_sbs6$yday==1] <- 0
final_sbs6$F_Nieuws[final_sbs6$yday==1] <- 0
final_sbs6$F_OM_Programa[final_sbs6$yday==1] <- 0
final_sbs6$F_OM_Live_registratie[final_sbs6$yday==1] <- 0
final_sbs6$F_SPEL_QUIZ[final_sbs6$yday==1] <- 0
final_sbs6$F_NLD_FILMS[final_sbs6$yday==1] <- 0
final_sbs6$F_NLD_SERIES[final_sbs6$yday==1] <- 0
final_sbs6$F_BTL_SERIES[final_sbs6$yday==1] <- 0
final_sbs6$F_Nld_series[final_sbs6$yday==1] <- 0
final_sbs6$F_Voetbalreportage[final_sbs6$yday==1] <- 0
final_sbs6$F_OM_overig[final_sbs6$yday==1] <- 0
final_sbs6$F_Satire[final_sbs6$yday==1] <- 0
final_sbs6$F_PM_Live_registratie[final_sbs6$yday==1] <- 0
final_sbs6$F_PM_Videoclips[final_sbs6$yday==1] <- 0
final_sbs6$F_Actuele_sportinformatie[final_sbs6$yday==1] <- 0
final_sbs6$F_Cabaret_kleinkunst[final_sbs6$yday==1] <- 0
final_sbs6$F_Overige_sportreportage[final_sbs6$yday==1] <- 0
final_sbs6$F_Overige_sportinformatie[final_sbs6$yday==1] <- 0
final_sbs6$F_Tekstuele_informatie[final_sbs6$yday==1] <- 0
final_sbs6$F_Weerbericht[final_sbs6$yday==1] <- 0
final_sbs6$Voetball[final_sbs6$yday==1] <- 0
#### sbs6 after #####
final_sbs6$Date <- ymd(final_sbs6$Date)
final_sbs6_2 <- left_join(final_sbs6,tw,by='Date')
final_sbs6_2 <- final_sbs6_2[,-c(8)]
final_sbs6_2 <- mutate(final_sbs6_2,Temp=ifelse(is.na(Temp)==TRUE,0,Temp))
final_sbs6_2 <- mutate(final_sbs6_2,Rain=ifelse(is.na(Rain)==TRUE,0,Rain))
final_sbs6_2 <- unique(final_sbs6_2)
#-----------------------------------------------------------------------------------------#

####net5 pre ####
final_net5 <- subset(OnlineTotal,(OnlineTotal$sourcesite == "net5" & OnlineTotal$sbs_partner == "n/a"), select = c(1,5:13,17:42)) 
final_net5 <- mutate(final_net5,Type..Subject.3..SKO.=ifelse(is.na(Type..Subject.3..SKO.)==TRUE|Type..Subject.3..SKO.=='',"NA",Type..Subject.3..SKO.))
final_net5 <- mutate(final_net5,Holiday.name.x=ifelse(Holiday.name.x==""|is.na(Holiday.name.x)==TRUE,"NH",Holiday.name.x))

final_net5 <- group_by(final_net5,Date,Holiday.name.x,mday,wday,yday,month,year,`day+1`)
final_net5 <- summarize(final_net5,n=sum(n),
                        F_BTL_FILMS=sum(F_BTL_FILMS),
                        F_OVERIGE_NON_FICTIE=sum(F_OVERIGE_NON_FICTIE),
                        F_Overig_amusement=sum(F_Overig_amusement),
                        F_TAP=sum(F_TAP),
                        F_KINDERFILMS=sum(F_KINDERFILMS),
                        F_Actualiteiten=sum(F_Actualiteiten),
                        F_Nieuws=sum(F_Nieuws),
                        F_OM_Programa=sum(F_OM_Programa),
                        F_OM_Live_registratie=sum(F_OM_Live_registratie),
                        F_SPEL_QUIZ=sum(F_SPEL_QUIZ),
                        F_NLD_FILMS=sum(F_NLD_FILMS),
                        F_NLD_SERIES=sum(F_NLD_SERIES),
                        F_BTL_SERIES=sum(F_BTL_SERIES),
                        F_Nld_series=sum(F_Nld_series),
                        F_Voetbalreportage=sum(F_Voetbalreportage),
                        F_OM_overig=sum(F_OM_overig),
                        F_Satire=sum(F_Satire),
                        F_PM_Live_registratie=sum(F_PM_Live_registratie),
                        F_PM_Videoclips=sum(F_PM_Videoclips),
                        F_Actuele_sportinformatie=sum(F_Actuele_sportinformatie),
                        F_Cabaret_kleinkunst=sum(F_Cabaret_kleinkunst),
                        F_Overige_sportreportage=sum(F_Overige_sportreportage),
                        F_Overige_sportinformatie=sum(F_Overige_sportinformatie),
                        F_Tekstuele_informatie=sum(F_Tekstuele_informatie),
                        F_Weerbericht=sum(F_Weerbericht),
                        Voetball=sum(Voetball)
)
#### net5 ####
for(i in nrow(final_net5):2){
  #BTL FILMS
  final_net5$F_BTL_FILMS[i]=final_net5$F_BTL_FILMS[i-1]
  #OVERIGE NON FICTIE
  final_net5$F_OVERIGE_NON_FICTIE[i]=final_net5$F_OVERIGE_NON_FICTIE[i-1]
  # #OVERIG AMUSEMENT
  final_net5$F_Overig_amusement[i]=final_net5$F_Overig_amusement[i-1]
  # #Talentenjacht/Auditie/Programma
  final_net5$F_TAP[i]=final_net5$F_TAP[i-1]
  # #KINDERFILMS
  final_net5$F_KINDERFILMS[i]=final_net5$F_KINDERFILMS[i-1]
  # #Actualiteiten
  final_net5$F_Actualiteiten[i]=final_net5$F_Actualiteiten[i-1]
  # #Nieuws
  final_net5$F_Nieuws[i]=final_net5$F_Nieuws[i-1]
  # #Overige muziek: Programa
  final_net5$F_OM_Programa[i]=final_net5$F_OM_Programa[i-1]
  # #Overige muziek: Live registratie
  final_net5$F_OM_Live_registratie[i]=final_net5$F_OM_Live_registratie[i-1]
  # #SPEL & QUIZ
  final_net5$F_SPEL_QUIZ[i]=final_net5$F_SPEL_QUIZ[i-1]
  # #NLD FILMS
  final_net5$F_NLD_FILMS[i]=final_net5$F_NLD_FILMS[i-1]
  # #NLD SERIES
  final_net5$F_NLD_SERIES[i]=final_net5$F_NLD_SERIES[i-1]
  # #BTL SERIES
  final_net5$F_BTL_SERIES[i]=final_net5$F_BTL_SERIES[i-1]
  # #Nld Series
  final_net5$F_Nld_series[i]=final_net5$F_Nld_series[i-1]
  # #Voetbalreportage
  final_net5$F_Voetbalreportage[i]=final_net5$F_Voetbalreportage[i-1]
  # #Overige muziek: overig
  final_net5$F_OM_overig[i]=final_net5$F_OM_overig[i-1]
  # #Satire
  final_net5$F_Satire[i]=final_net5$F_Satire[i-1]
  # #Populaire muziek: Live registratie
  final_net5$F_PM_Live_registratie[i]=final_net5$F_PM_Live_registratie[i-1]
  # #Populaire muziek: Videoclips
  final_net5$F_PM_Videoclips[i]=final_net5$F_PM_Videoclips[i-1]
  # #Actuele sportinformatie
  final_net5$F_Actuele_sportinformatie[i]=final_net5$F_Actuele_sportinformatie[i-1]
  # #Cabaret/kleinkunst
  final_net5$F_Cabaret_kleinkunst[i]=final_net5$F_Cabaret_kleinkunst[i-1]
  # #Overige sportreportage
  final_net5$F_Overige_sportreportage[i]=final_net5$F_Overige_sportreportage[i-1]
  # #Overige sportinformatie
  final_net5$F_Overige_sportinformatie[i]=final_net5$F_Overige_sportinformatie[i-1]
  # #Tekstuele informatie
  final_net5$F_Tekstuele_informatie[i]=final_net5$F_Tekstuele_informatie[i-1]
  # #Weerbericht
  final_net5$F_Weerbericht[i]=final_net5$F_Weerbericht[i-1]
  # #Voetball
  final_net5$Voetball[i]=final_net5$Voetball[i-1]
  
}
final_net5$F_BTL_FILMS[final_net5$yday==1] <- 0
final_net5$F_OVERIGE_NON_FICTIE[final_net5$yday==1] <- 0
final_net5$F_Overig_amusement[final_net5$yday==1] <- 0
final_net5$F_TAP[final_net5$yday==1] <- 0
final_net5$F_KINDERFILMS[final_net5$yday==1] <- 0
final_net5$F_Actualiteiten[final_net5$yday==1] <- 0
final_net5$F_Nieuws[final_net5$yday==1] <- 0
final_net5$F_OM_Programa[final_net5$yday==1] <- 0
final_net5$F_OM_Live_registratie[final_net5$yday==1] <- 0
final_net5$F_SPEL_QUIZ[final_net5$yday==1] <- 0
final_net5$F_NLD_FILMS[final_net5$yday==1] <- 0
final_net5$F_NLD_SERIES[final_net5$yday==1] <- 0
final_net5$F_BTL_SERIES[final_net5$yday==1] <- 0
final_net5$F_Nld_series[final_net5$yday==1] <- 0
final_net5$F_Voetbalreportage[final_net5$yday==1] <- 0
final_net5$F_OM_overig[final_net5$yday==1] <- 0
final_net5$F_Satire[final_net5$yday==1] <- 0
final_net5$F_PM_Live_registratie[final_net5$yday==1] <- 0
final_net5$F_PM_Videoclips[final_net5$yday==1] <- 0
final_net5$F_Actuele_sportinformatie[final_net5$yday==1] <- 0
final_net5$F_Cabaret_kleinkunst[final_net5$yday==1] <- 0
final_net5$F_Overige_sportreportage[final_net5$yday==1] <- 0
final_net5$F_Overige_sportinformatie[final_net5$yday==1] <- 0
final_net5$F_Tekstuele_informatie[final_net5$yday==1] <- 0
final_net5$F_Weerbericht[final_net5$yday==1] <- 0
final_net5$Voetball[final_net5$yday==1] <- 0
#### net5 after #####
final_net5$Date <- ymd(final_net5$Date)
final_net5_2 <- left_join(final_net5,tw,by='Date')
final_net5_2 <- final_net5_2[,-c(8)]
final_net5_2 <- mutate(final_net5_2,Temp=ifelse(is.na(Temp)==TRUE,0,Temp))
final_net5_2 <- mutate(final_net5_2,Rain=ifelse(is.na(Rain)==TRUE,0,Rain))
final_net5_2 <- unique(final_net5_2)

#-----------------------------------------------------------------------------------------#

####veronicatv pre ####
final_veronicatv <- subset(OnlineTotal,(OnlineTotal$sourcesite == "veronicatv" & OnlineTotal$sbs_partner == "n/a"), select = c(1,5:13,17:42)) 
final_veronicatv <- mutate(final_veronicatv,Type..Subject.3..SKO.=ifelse(is.na(Type..Subject.3..SKO.)==TRUE|Type..Subject.3..SKO.=='',"NA",Type..Subject.3..SKO.))
final_veronicatv <- mutate(final_veronicatv,Holiday.name.x=ifelse(Holiday.name.x==""|is.na(Holiday.name.x)==TRUE,"NH",Holiday.name.x))

final_veronicatv <- group_by(final_veronicatv,Date,Holiday.name.x,mday,wday,yday,month,year,`day+1`)
final_veronicatv <- summarize(final_veronicatv,n=sum(n),
                        F_BTL_FILMS=sum(F_BTL_FILMS),
                        F_OVERIGE_NON_FICTIE=sum(F_OVERIGE_NON_FICTIE),
                        F_Overig_amusement=sum(F_Overig_amusement),
                        F_TAP=sum(F_TAP),
                        F_KINDERFILMS=sum(F_KINDERFILMS),
                        F_Actualiteiten=sum(F_Actualiteiten),
                        F_Nieuws=sum(F_Nieuws),
                        F_OM_Programa=sum(F_OM_Programa),
                        F_OM_Live_registratie=sum(F_OM_Live_registratie),
                        F_SPEL_QUIZ=sum(F_SPEL_QUIZ),
                        F_NLD_FILMS=sum(F_NLD_FILMS),
                        F_NLD_SERIES=sum(F_NLD_SERIES),
                        F_BTL_SERIES=sum(F_BTL_SERIES),
                        F_Nld_series=sum(F_Nld_series),
                        F_Voetbalreportage=sum(F_Voetbalreportage),
                        F_OM_overig=sum(F_OM_overig),
                        F_Satire=sum(F_Satire),
                        F_PM_Live_registratie=sum(F_PM_Live_registratie),
                        F_PM_Videoclips=sum(F_PM_Videoclips),
                        F_Actuele_sportinformatie=sum(F_Actuele_sportinformatie),
                        F_Cabaret_kleinkunst=sum(F_Cabaret_kleinkunst),
                        F_Overige_sportreportage=sum(F_Overige_sportreportage),
                        F_Overige_sportinformatie=sum(F_Overige_sportinformatie),
                        F_Tekstuele_informatie=sum(F_Tekstuele_informatie),
                        F_Weerbericht=sum(F_Weerbericht),
                        Voetball=sum(Voetball)
)
#### veronicatv ####
for(i in nrow(final_veronicatv):2){
  #BTL FILMS
  final_veronicatv$F_BTL_FILMS[i]=final_veronicatv$F_BTL_FILMS[i-1]
  #OVERIGE NON FICTIE
  final_veronicatv$F_OVERIGE_NON_FICTIE[i]=final_veronicatv$F_OVERIGE_NON_FICTIE[i-1]
  # #OVERIG AMUSEMENT
  final_veronicatv$F_Overig_amusement[i]=final_veronicatv$F_Overig_amusement[i-1]
  # #Talentenjacht/Auditie/Programma
  final_veronicatv$F_TAP[i]=final_veronicatv$F_TAP[i-1]
  # #KINDERFILMS
  final_veronicatv$F_KINDERFILMS[i]=final_veronicatv$F_KINDERFILMS[i-1]
  # #Actualiteiten
  final_veronicatv$F_Actualiteiten[i]=final_veronicatv$F_Actualiteiten[i-1]
  # #Nieuws
  final_veronicatv$F_Nieuws[i]=final_veronicatv$F_Nieuws[i-1]
  # #Overige muziek: Programa
  final_veronicatv$F_OM_Programa[i]=final_veronicatv$F_OM_Programa[i-1]
  # #Overige muziek: Live registratie
  final_veronicatv$F_OM_Live_registratie[i]=final_veronicatv$F_OM_Live_registratie[i-1]
  # #SPEL & QUIZ
  final_veronicatv$F_SPEL_QUIZ[i]=final_veronicatv$F_SPEL_QUIZ[i-1]
  # #NLD FILMS
  final_veronicatv$F_NLD_FILMS[i]=final_veronicatv$F_NLD_FILMS[i-1]
  # #NLD SERIES
  final_veronicatv$F_NLD_SERIES[i]=final_veronicatv$F_NLD_SERIES[i-1]
  # #BTL SERIES
  final_veronicatv$F_BTL_SERIES[i]=final_veronicatv$F_BTL_SERIES[i-1]
  # #Nld Series
  final_veronicatv$F_Nld_series[i]=final_veronicatv$F_Nld_series[i-1]
  # #Voetbalreportage
  final_veronicatv$F_Voetbalreportage[i]=final_veronicatv$F_Voetbalreportage[i-1]
  # #Overige muziek: overig
  final_veronicatv$F_OM_overig[i]=final_veronicatv$F_OM_overig[i-1]
  # #Satire
  final_veronicatv$F_Satire[i]=final_veronicatv$F_Satire[i-1]
  # #Populaire muziek: Live registratie
  final_veronicatv$F_PM_Live_registratie[i]=final_veronicatv$F_PM_Live_registratie[i-1]
  # #Populaire muziek: Videoclips
  final_veronicatv$F_PM_Videoclips[i]=final_veronicatv$F_PM_Videoclips[i-1]
  # #Actuele sportinformatie
  final_veronicatv$F_Actuele_sportinformatie[i]=final_veronicatv$F_Actuele_sportinformatie[i-1]
  # #Cabaret/kleinkunst
  final_veronicatv$F_Cabaret_kleinkunst[i]=final_veronicatv$F_Cabaret_kleinkunst[i-1]
  # #Overige sportreportage
  final_veronicatv$F_Overige_sportreportage[i]=final_veronicatv$F_Overige_sportreportage[i-1]
  # #Overige sportinformatie
  final_veronicatv$F_Overige_sportinformatie[i]=final_veronicatv$F_Overige_sportinformatie[i-1]
  # #Tekstuele informatie
  final_veronicatv$F_Tekstuele_informatie[i]=final_veronicatv$F_Tekstuele_informatie[i-1]
  # #Weerbericht
  final_veronicatv$F_Weerbericht[i]=final_veronicatv$F_Weerbericht[i-1]
  # #Voetball
  final_veronicatv$Voetball[i]=final_veronicatv$Voetball[i-1]
  
}
final_veronicatv$F_BTL_FILMS[final_veronicatv$yday==1] <- 0
final_veronicatv$F_OVERIGE_NON_FICTIE[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Overig_amusement[final_veronicatv$yday==1] <- 0
final_veronicatv$F_TAP[final_veronicatv$yday==1] <- 0
final_veronicatv$F_KINDERFILMS[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Actualiteiten[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Nieuws[final_veronicatv$yday==1] <- 0
final_veronicatv$F_OM_Programa[final_veronicatv$yday==1] <- 0
final_veronicatv$F_OM_Live_registratie[final_veronicatv$yday==1] <- 0
final_veronicatv$F_SPEL_QUIZ[final_veronicatv$yday==1] <- 0
final_veronicatv$F_NLD_FILMS[final_veronicatv$yday==1] <- 0
final_veronicatv$F_NLD_SERIES[final_veronicatv$yday==1] <- 0
final_veronicatv$F_BTL_SERIES[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Nld_series[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Voetbalreportage[final_veronicatv$yday==1] <- 0
final_veronicatv$F_OM_overig[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Satire[final_veronicatv$yday==1] <- 0
final_veronicatv$F_PM_Live_registratie[final_veronicatv$yday==1] <- 0
final_veronicatv$F_PM_Videoclips[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Actuele_sportinformatie[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Cabaret_kleinkunst[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Overige_sportreportage[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Overige_sportinformatie[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Tekstuele_informatie[final_veronicatv$yday==1] <- 0
final_veronicatv$F_Weerbericht[final_veronicatv$yday==1] <- 0
final_veronicatv$Voetball[final_veronicatv$yday==1] <- 0
#### veronicatv after #####
final_veronicatv$Date <- ymd(final_veronicatv$Date)
final_veronicatv_2 <- left_join(final_veronicatv,tw,by='Date')
final_veronicatv_2 <- final_veronicatv_2[,-c(8)]
final_veronicatv_2 <- mutate(final_veronicatv_2,Temp=ifelse(is.na(Temp)==TRUE,0,Temp))
final_veronicatv_2 <- mutate(final_veronicatv_2,Rain=ifelse(is.na(Rain)==TRUE,0,Rain))
final_veronicatv_2 <- unique(final_veronicatv_2)

#-----------------------------------------------------------------------------------------#

####sbs9 pre ####
final_sbs9 <- subset(OnlineTotal,(OnlineTotal$sourcesite == "sbs9" & OnlineTotal$sbs_partner == "n/a"), select = c(1,5:13,17:42)) 
final_sbs9 <- mutate(final_sbs9,Type..Subject.3..SKO.=ifelse(is.na(Type..Subject.3..SKO.)==TRUE|Type..Subject.3..SKO.=='',"NA",Type..Subject.3..SKO.))
final_sbs9 <- mutate(final_sbs9,Holiday.name.x=ifelse(Holiday.name.x==""|is.na(Holiday.name.x)==TRUE,"NH",Holiday.name.x))

final_sbs9 <- group_by(final_sbs9,Date,Holiday.name.x,mday,wday,yday,month,year,`day+1`)
final_sbs9 <- summarize(final_sbs9,n=sum(n),
                              F_BTL_FILMS=sum(F_BTL_FILMS),
                              F_OVERIGE_NON_FICTIE=sum(F_OVERIGE_NON_FICTIE),
                              F_Overig_amusement=sum(F_Overig_amusement),
                              F_TAP=sum(F_TAP),
                              F_KINDERFILMS=sum(F_KINDERFILMS),
                              F_Actualiteiten=sum(F_Actualiteiten),
                              F_Nieuws=sum(F_Nieuws),
                              F_OM_Programa=sum(F_OM_Programa),
                              F_OM_Live_registratie=sum(F_OM_Live_registratie),
                              F_SPEL_QUIZ=sum(F_SPEL_QUIZ),
                              F_NLD_FILMS=sum(F_NLD_FILMS),
                              F_NLD_SERIES=sum(F_NLD_SERIES),
                              F_BTL_SERIES=sum(F_BTL_SERIES),
                              F_Nld_series=sum(F_Nld_series),
                              F_Voetbalreportage=sum(F_Voetbalreportage),
                              F_OM_overig=sum(F_OM_overig),
                              F_Satire=sum(F_Satire),
                              F_PM_Live_registratie=sum(F_PM_Live_registratie),
                              F_PM_Videoclips=sum(F_PM_Videoclips),
                              F_Actuele_sportinformatie=sum(F_Actuele_sportinformatie),
                              F_Cabaret_kleinkunst=sum(F_Cabaret_kleinkunst),
                              F_Overige_sportreportage=sum(F_Overige_sportreportage),
                              F_Overige_sportinformatie=sum(F_Overige_sportinformatie),
                              F_Tekstuele_informatie=sum(F_Tekstuele_informatie),
                              F_Weerbericht=sum(F_Weerbericht),
                              Voetball=sum(Voetball)
)
#### sbs9 ####
for(i in nrow(final_sbs9):2){
  #BTL FILMS
  final_sbs9$F_BTL_FILMS[i]=final_sbs9$F_BTL_FILMS[i-1]
  #OVERIGE NON FICTIE
  final_sbs9$F_OVERIGE_NON_FICTIE[i]=final_sbs9$F_OVERIGE_NON_FICTIE[i-1]
  # #OVERIG AMUSEMENT
  final_sbs9$F_Overig_amusement[i]=final_sbs9$F_Overig_amusement[i-1]
  # #Talentenjacht/Auditie/Programma
  final_sbs9$F_TAP[i]=final_sbs9$F_TAP[i-1]
  # #KINDERFILMS
  final_sbs9$F_KINDERFILMS[i]=final_sbs9$F_KINDERFILMS[i-1]
  # #Actualiteiten
  final_sbs9$F_Actualiteiten[i]=final_sbs9$F_Actualiteiten[i-1]
  # #Nieuws
  final_sbs9$F_Nieuws[i]=final_sbs9$F_Nieuws[i-1]
  # #Overige muziek: Programa
  final_sbs9$F_OM_Programa[i]=final_sbs9$F_OM_Programa[i-1]
  # #Overige muziek: Live registratie
  final_sbs9$F_OM_Live_registratie[i]=final_sbs9$F_OM_Live_registratie[i-1]
  # #SPEL & QUIZ
  final_sbs9$F_SPEL_QUIZ[i]=final_sbs9$F_SPEL_QUIZ[i-1]
  # #NLD FILMS
  final_sbs9$F_NLD_FILMS[i]=final_sbs9$F_NLD_FILMS[i-1]
  # #NLD SERIES
  final_sbs9$F_NLD_SERIES[i]=final_sbs9$F_NLD_SERIES[i-1]
  # #BTL SERIES
  final_sbs9$F_BTL_SERIES[i]=final_sbs9$F_BTL_SERIES[i-1]
  # #Nld Series
  final_sbs9$F_Nld_series[i]=final_sbs9$F_Nld_series[i-1]
  # #Voetbalreportage
  final_sbs9$F_Voetbalreportage[i]=final_sbs9$F_Voetbalreportage[i-1]
  # #Overige muziek: overig
  final_sbs9$F_OM_overig[i]=final_sbs9$F_OM_overig[i-1]
  # #Satire
  final_sbs9$F_Satire[i]=final_sbs9$F_Satire[i-1]
  # #Populaire muziek: Live registratie
  final_sbs9$F_PM_Live_registratie[i]=final_sbs9$F_PM_Live_registratie[i-1]
  # #Populaire muziek: Videoclips
  final_sbs9$F_PM_Videoclips[i]=final_sbs9$F_PM_Videoclips[i-1]
  # #Actuele sportinformatie
  final_sbs9$F_Actuele_sportinformatie[i]=final_sbs9$F_Actuele_sportinformatie[i-1]
  # #Cabaret/kleinkunst
  final_sbs9$F_Cabaret_kleinkunst[i]=final_sbs9$F_Cabaret_kleinkunst[i-1]
  # #Overige sportreportage
  final_sbs9$F_Overige_sportreportage[i]=final_sbs9$F_Overige_sportreportage[i-1]
  # #Overige sportinformatie
  final_sbs9$F_Overige_sportinformatie[i]=final_sbs9$F_Overige_sportinformatie[i-1]
  # #Tekstuele informatie
  final_sbs9$F_Tekstuele_informatie[i]=final_sbs9$F_Tekstuele_informatie[i-1]
  # #Weerbericht
  final_sbs9$F_Weerbericht[i]=final_sbs9$F_Weerbericht[i-1]
  # #Voetball
  final_sbs9$Voetball[i]=final_sbs9$Voetball[i-1]
  
}
final_sbs9$F_BTL_FILMS[final_sbs9$yday==1] <- 0
final_sbs9$F_OVERIGE_NON_FICTIE[final_sbs9$yday==1] <- 0
final_sbs9$F_Overig_amusement[final_sbs9$yday==1] <- 0
final_sbs9$F_TAP[final_sbs9$yday==1] <- 0
final_sbs9$F_KINDERFILMS[final_sbs9$yday==1] <- 0
final_sbs9$F_Actualiteiten[final_sbs9$yday==1] <- 0
final_sbs9$F_Nieuws[final_sbs9$yday==1] <- 0
final_sbs9$F_OM_Programa[final_sbs9$yday==1] <- 0
final_sbs9$F_OM_Live_registratie[final_sbs9$yday==1] <- 0
final_sbs9$F_SPEL_QUIZ[final_sbs9$yday==1] <- 0
final_sbs9$F_NLD_FILMS[final_sbs9$yday==1] <- 0
final_sbs9$F_NLD_SERIES[final_sbs9$yday==1] <- 0
final_sbs9$F_BTL_SERIES[final_sbs9$yday==1] <- 0
final_sbs9$F_Nld_series[final_sbs9$yday==1] <- 0
final_sbs9$F_Voetbalreportage[final_sbs9$yday==1] <- 0
final_sbs9$F_OM_overig[final_sbs9$yday==1] <- 0
final_sbs9$F_Satire[final_sbs9$yday==1] <- 0
final_sbs9$F_PM_Live_registratie[final_sbs9$yday==1] <- 0
final_sbs9$F_PM_Videoclips[final_sbs9$yday==1] <- 0
final_sbs9$F_Actuele_sportinformatie[final_sbs9$yday==1] <- 0
final_sbs9$F_Cabaret_kleinkunst[final_sbs9$yday==1] <- 0
final_sbs9$F_Overige_sportreportage[final_sbs9$yday==1] <- 0
final_sbs9$F_Overige_sportinformatie[final_sbs9$yday==1] <- 0
final_sbs9$F_Tekstuele_informatie[final_sbs9$yday==1] <- 0
final_sbs9$F_Weerbericht[final_sbs9$yday==1] <- 0
final_sbs9$Voetball[final_sbs9$yday==1] <- 0
#### sbs9 after #####
final_sbs9$Date <- ymd(final_sbs9$Date)
final_sbs9_2 <- left_join(final_sbs9,tw,by='Date')
final_sbs9_2 <- final_sbs9_2[,-c(8)]
final_sbs9_2 <- mutate(final_sbs9_2,Temp=ifelse(is.na(Temp)==TRUE,0,Temp))
final_sbs9_2 <- mutate(final_sbs9_2,Rain=ifelse(is.na(Rain)==TRUE,0,Rain))
final_sbs9_2 <- unique(final_sbs9_2)

#-----------------------------------------------------------------------------------------#

####s1tv pre ####
final_s1tv <- subset(OnlineTotal,(OnlineTotal$sbs_partner == "s1tv"), select = c(1,5:13,17:42)) 
final_s1tv <- mutate(final_s1tv,Type..Subject.3..SKO.=ifelse(is.na(Type..Subject.3..SKO.)==TRUE|Type..Subject.3..SKO.=='',"NA",Type..Subject.3..SKO.))
final_s1tv <- mutate(final_s1tv,Holiday.name.x=ifelse(Holiday.name.x==""|is.na(Holiday.name.x)==TRUE,"NH",Holiday.name.x))

final_s1tv <- group_by(final_s1tv,Date,Holiday.name.x,mday,wday,yday,month,year,`day+1`)
final_s1tv <- summarize(final_s1tv,n=sum(n),
                        F_BTL_FILMS=sum(F_BTL_FILMS),
                        F_OVERIGE_NON_FICTIE=sum(F_OVERIGE_NON_FICTIE),
                        F_Overig_amusement=sum(F_Overig_amusement),
                        F_TAP=sum(F_TAP),
                        F_KINDERFILMS=sum(F_KINDERFILMS),
                        F_Actualiteiten=sum(F_Actualiteiten),
                        F_Nieuws=sum(F_Nieuws),
                        F_OM_Programa=sum(F_OM_Programa),
                        F_OM_Live_registratie=sum(F_OM_Live_registratie),
                        F_SPEL_QUIZ=sum(F_SPEL_QUIZ),
                        F_NLD_FILMS=sum(F_NLD_FILMS),
                        F_NLD_SERIES=sum(F_NLD_SERIES),
                        F_BTL_SERIES=sum(F_BTL_SERIES),
                        F_Nld_series=sum(F_Nld_series),
                        F_Voetbalreportage=sum(F_Voetbalreportage),
                        F_OM_overig=sum(F_OM_overig),
                        F_Satire=sum(F_Satire),
                        F_PM_Live_registratie=sum(F_PM_Live_registratie),
                        F_PM_Videoclips=sum(F_PM_Videoclips),
                        F_Actuele_sportinformatie=sum(F_Actuele_sportinformatie),
                        F_Cabaret_kleinkunst=sum(F_Cabaret_kleinkunst),
                        F_Overige_sportreportage=sum(F_Overige_sportreportage),
                        F_Overige_sportinformatie=sum(F_Overige_sportinformatie),
                        F_Tekstuele_informatie=sum(F_Tekstuele_informatie),
                        F_Weerbericht=sum(F_Weerbericht),
                        Voetball=sum(Voetball)
)
#### s1tv ####
for(i in nrow(final_s1tv):2){
  #BTL FILMS
  final_s1tv$F_BTL_FILMS[i]=final_s1tv$F_BTL_FILMS[i-1]
  #OVERIGE NON FICTIE
  final_s1tv$F_OVERIGE_NON_FICTIE[i]=final_s1tv$F_OVERIGE_NON_FICTIE[i-1]
  # #OVERIG AMUSEMENT
  final_s1tv$F_Overig_amusement[i]=final_s1tv$F_Overig_amusement[i-1]
  # #Talentenjacht/Auditie/Programma
  final_s1tv$F_TAP[i]=final_s1tv$F_TAP[i-1]
  # #KINDERFILMS
  final_s1tv$F_KINDERFILMS[i]=final_s1tv$F_KINDERFILMS[i-1]
  # #Actualiteiten
  final_s1tv$F_Actualiteiten[i]=final_s1tv$F_Actualiteiten[i-1]
  # #Nieuws
  final_s1tv$F_Nieuws[i]=final_s1tv$F_Nieuws[i-1]
  # #Overige muziek: Programa
  final_s1tv$F_OM_Programa[i]=final_s1tv$F_OM_Programa[i-1]
  # #Overige muziek: Live registratie
  final_s1tv$F_OM_Live_registratie[i]=final_s1tv$F_OM_Live_registratie[i-1]
  # #SPEL & QUIZ
  final_s1tv$F_SPEL_QUIZ[i]=final_s1tv$F_SPEL_QUIZ[i-1]
  # #NLD FILMS
  final_s1tv$F_NLD_FILMS[i]=final_s1tv$F_NLD_FILMS[i-1]
  # #NLD SERIES
  final_s1tv$F_NLD_SERIES[i]=final_s1tv$F_NLD_SERIES[i-1]
  # #BTL SERIES
  final_s1tv$F_BTL_SERIES[i]=final_s1tv$F_BTL_SERIES[i-1]
  # #Nld Series
  final_s1tv$F_Nld_series[i]=final_s1tv$F_Nld_series[i-1]
  # #Voetbalreportage
  final_s1tv$F_Voetbalreportage[i]=final_s1tv$F_Voetbalreportage[i-1]
  # #Overige muziek: overig
  final_s1tv$F_OM_overig[i]=final_s1tv$F_OM_overig[i-1]
  # #Satire
  final_s1tv$F_Satire[i]=final_s1tv$F_Satire[i-1]
  # #Populaire muziek: Live registratie
  final_s1tv$F_PM_Live_registratie[i]=final_s1tv$F_PM_Live_registratie[i-1]
  # #Populaire muziek: Videoclips
  final_s1tv$F_PM_Videoclips[i]=final_s1tv$F_PM_Videoclips[i-1]
  # #Actuele sportinformatie
  final_s1tv$F_Actuele_sportinformatie[i]=final_s1tv$F_Actuele_sportinformatie[i-1]
  # #Cabaret/kleinkunst
  final_s1tv$F_Cabaret_kleinkunst[i]=final_s1tv$F_Cabaret_kleinkunst[i-1]
  # #Overige sportreportage
  final_s1tv$F_Overige_sportreportage[i]=final_s1tv$F_Overige_sportreportage[i-1]
  # #Overige sportinformatie
  final_s1tv$F_Overige_sportinformatie[i]=final_s1tv$F_Overige_sportinformatie[i-1]
  # #Tekstuele informatie
  final_s1tv$F_Tekstuele_informatie[i]=final_s1tv$F_Tekstuele_informatie[i-1]
  # #Weerbericht
  final_s1tv$F_Weerbericht[i]=final_s1tv$F_Weerbericht[i-1]
  # #Voetball
  final_s1tv$Voetball[i]=final_s1tv$Voetball[i-1]
  
}
final_s1tv$F_BTL_FILMS[final_s1tv$yday==1] <- 0
final_s1tv$F_OVERIGE_NON_FICTIE[final_s1tv$yday==1] <- 0
final_s1tv$F_Overig_amusement[final_s1tv$yday==1] <- 0
final_s1tv$F_TAP[final_s1tv$yday==1] <- 0
final_s1tv$F_KINDERFILMS[final_s1tv$yday==1] <- 0
final_s1tv$F_Actualiteiten[final_s1tv$yday==1] <- 0
final_s1tv$F_Nieuws[final_s1tv$yday==1] <- 0
final_s1tv$F_OM_Programa[final_s1tv$yday==1] <- 0
final_s1tv$F_OM_Live_registratie[final_s1tv$yday==1] <- 0
final_s1tv$F_SPEL_QUIZ[final_s1tv$yday==1] <- 0
final_s1tv$F_NLD_FILMS[final_s1tv$yday==1] <- 0
final_s1tv$F_NLD_SERIES[final_s1tv$yday==1] <- 0
final_s1tv$F_BTL_SERIES[final_s1tv$yday==1] <- 0
final_s1tv$F_Nld_series[final_s1tv$yday==1] <- 0
final_s1tv$F_Voetbalreportage[final_s1tv$yday==1] <- 0
final_s1tv$F_OM_overig[final_s1tv$yday==1] <- 0
final_s1tv$F_Satire[final_s1tv$yday==1] <- 0
final_s1tv$F_PM_Live_registratie[final_s1tv$yday==1] <- 0
final_s1tv$F_PM_Videoclips[final_s1tv$yday==1] <- 0
final_s1tv$F_Actuele_sportinformatie[final_s1tv$yday==1] <- 0
final_s1tv$F_Cabaret_kleinkunst[final_s1tv$yday==1] <- 0
final_s1tv$F_Overige_sportreportage[final_s1tv$yday==1] <- 0
final_s1tv$F_Overige_sportinformatie[final_s1tv$yday==1] <- 0
final_s1tv$F_Tekstuele_informatie[final_s1tv$yday==1] <- 0
final_s1tv$F_Weerbericht[final_s1tv$yday==1] <- 0
final_s1tv$Voetball[final_s1tv$yday==1] <- 0
#### s1tv after #####
final_s1tv$Date <- ymd(final_s1tv$Date)
final_s1tv_2 <- left_join(final_s1tv,tw,by='Date')
final_s1tv_2 <- final_s1tv_2[,-c(8)]
final_s1tv_2 <- mutate(final_s1tv_2,Temp=ifelse(is.na(Temp)==TRUE,0,Temp))
final_s1tv_2 <- mutate(final_s1tv_2,Rain=ifelse(is.na(Rain)==TRUE,0,Rain))
final_s1tv_2 <- unique(final_s1tv_2)

#-----------------------------------------------------------------------------------------#

####kijk pre ####
final_kijk <- subset(OnlineTotal,(OnlineTotal$sourcesite == "kijk" & OnlineTotal$sbs_partner == "n/a"), select = c(1,5:13,17:42)) 
final_kijk <- mutate(final_kijk,Type..Subject.3..SKO.=ifelse(is.na(Type..Subject.3..SKO.)==TRUE|Type..Subject.3..SKO.=='',"NA",Type..Subject.3..SKO.))
final_kijk <- mutate(final_kijk,Holiday.name.x=ifelse(Holiday.name.x==""|is.na(Holiday.name.x)==TRUE,"NH",Holiday.name.x))

final_kijk <- group_by(final_kijk,Date,Holiday.name.x,mday,wday,yday,month,year,`day+1`)
final_kijk <- summarize(final_kijk,n=sum(n),
                        F_BTL_FILMS=sum(F_BTL_FILMS),
                        F_OVERIGE_NON_FICTIE=sum(F_OVERIGE_NON_FICTIE),
                        F_Overig_amusement=sum(F_Overig_amusement),
                        F_TAP=sum(F_TAP),
                        F_KINDERFILMS=sum(F_KINDERFILMS),
                        F_Actualiteiten=sum(F_Actualiteiten),
                        F_Nieuws=sum(F_Nieuws),
                        F_OM_Programa=sum(F_OM_Programa),
                        F_OM_Live_registratie=sum(F_OM_Live_registratie),
                        F_SPEL_QUIZ=sum(F_SPEL_QUIZ),
                        F_NLD_FILMS=sum(F_NLD_FILMS),
                        F_NLD_SERIES=sum(F_NLD_SERIES),
                        F_BTL_SERIES=sum(F_BTL_SERIES),
                        F_Nld_series=sum(F_Nld_series),
                        F_Voetbalreportage=sum(F_Voetbalreportage),
                        F_OM_overig=sum(F_OM_overig),
                        F_Satire=sum(F_Satire),
                        F_PM_Live_registratie=sum(F_PM_Live_registratie),
                        F_PM_Videoclips=sum(F_PM_Videoclips),
                        F_Actuele_sportinformatie=sum(F_Actuele_sportinformatie),
                        F_Cabaret_kleinkunst=sum(F_Cabaret_kleinkunst),
                        F_Overige_sportreportage=sum(F_Overige_sportreportage),
                        F_Overige_sportinformatie=sum(F_Overige_sportinformatie),
                        F_Tekstuele_informatie=sum(F_Tekstuele_informatie),
                        F_Weerbericht=sum(F_Weerbericht),
                        Voetball=sum(Voetball)
)
#### kijk ####
for(i in nrow(final_kijk):2){
  #BTL FILMS
  final_kijk$F_BTL_FILMS[i]=final_kijk$F_BTL_FILMS[i-1]
  #OVERIGE NON FICTIE
  final_kijk$F_OVERIGE_NON_FICTIE[i]=final_kijk$F_OVERIGE_NON_FICTIE[i-1]
  # #OVERIG AMUSEMENT
  final_kijk$F_Overig_amusement[i]=final_kijk$F_Overig_amusement[i-1]
  # #Talentenjacht/Auditie/Programma
  final_kijk$F_TAP[i]=final_kijk$F_TAP[i-1]
  # #KINDERFILMS
  final_kijk$F_KINDERFILMS[i]=final_kijk$F_KINDERFILMS[i-1]
  # #Actualiteiten
  final_kijk$F_Actualiteiten[i]=final_kijk$F_Actualiteiten[i-1]
  # #Nieuws
  final_kijk$F_Nieuws[i]=final_kijk$F_Nieuws[i-1]
  # #Overige muziek: Programa
  final_kijk$F_OM_Programa[i]=final_kijk$F_OM_Programa[i-1]
  # #Overige muziek: Live registratie
  final_kijk$F_OM_Live_registratie[i]=final_kijk$F_OM_Live_registratie[i-1]
  # #SPEL & QUIZ
  final_kijk$F_SPEL_QUIZ[i]=final_kijk$F_SPEL_QUIZ[i-1]
  # #NLD FILMS
  final_kijk$F_NLD_FILMS[i]=final_kijk$F_NLD_FILMS[i-1]
  # #NLD SERIES
  final_kijk$F_NLD_SERIES[i]=final_kijk$F_NLD_SERIES[i-1]
  # #BTL SERIES
  final_kijk$F_BTL_SERIES[i]=final_kijk$F_BTL_SERIES[i-1]
  # #Nld Series
  final_kijk$F_Nld_series[i]=final_kijk$F_Nld_series[i-1]
  # #Voetbalreportage
  final_kijk$F_Voetbalreportage[i]=final_kijk$F_Voetbalreportage[i-1]
  # #Overige muziek: overig
  final_kijk$F_OM_overig[i]=final_kijk$F_OM_overig[i-1]
  # #Satire
  final_kijk$F_Satire[i]=final_kijk$F_Satire[i-1]
  # #Populaire muziek: Live registratie
  final_kijk$F_PM_Live_registratie[i]=final_kijk$F_PM_Live_registratie[i-1]
  # #Populaire muziek: Videoclips
  final_kijk$F_PM_Videoclips[i]=final_kijk$F_PM_Videoclips[i-1]
  # #Actuele sportinformatie
  final_kijk$F_Actuele_sportinformatie[i]=final_kijk$F_Actuele_sportinformatie[i-1]
  # #Cabaret/kleinkunst
  final_kijk$F_Cabaret_kleinkunst[i]=final_kijk$F_Cabaret_kleinkunst[i-1]
  # #Overige sportreportage
  final_kijk$F_Overige_sportreportage[i]=final_kijk$F_Overige_sportreportage[i-1]
  # #Overige sportinformatie
  final_kijk$F_Overige_sportinformatie[i]=final_kijk$F_Overige_sportinformatie[i-1]
  # #Tekstuele informatie
  final_kijk$F_Tekstuele_informatie[i]=final_kijk$F_Tekstuele_informatie[i-1]
  # #Weerbericht
  final_kijk$F_Weerbericht[i]=final_kijk$F_Weerbericht[i-1]
  # #Voetball
  final_kijk$Voetball[i]=final_kijk$Voetball[i-1]
  
}
final_kijk$F_BTL_FILMS[final_kijk$yday==1] <- 0
final_kijk$F_OVERIGE_NON_FICTIE[final_kijk$yday==1] <- 0
final_kijk$F_Overig_amusement[final_kijk$yday==1] <- 0
final_kijk$F_TAP[final_kijk$yday==1] <- 0
final_kijk$F_KINDERFILMS[final_kijk$yday==1] <- 0
final_kijk$F_Actualiteiten[final_kijk$yday==1] <- 0
final_kijk$F_Nieuws[final_kijk$yday==1] <- 0
final_kijk$F_OM_Programa[final_kijk$yday==1] <- 0
final_kijk$F_OM_Live_registratie[final_kijk$yday==1] <- 0
final_kijk$F_SPEL_QUIZ[final_kijk$yday==1] <- 0
final_kijk$F_NLD_FILMS[final_kijk$yday==1] <- 0
final_kijk$F_NLD_SERIES[final_kijk$yday==1] <- 0
final_kijk$F_BTL_SERIES[final_kijk$yday==1] <- 0
final_kijk$F_Nld_series[final_kijk$yday==1] <- 0
final_kijk$F_Voetbalreportage[final_kijk$yday==1] <- 0
final_kijk$F_OM_overig[final_kijk$yday==1] <- 0
final_kijk$F_Satire[final_kijk$yday==1] <- 0
final_kijk$F_PM_Live_registratie[final_kijk$yday==1] <- 0
final_kijk$F_PM_Videoclips[final_kijk$yday==1] <- 0
final_kijk$F_Actuele_sportinformatie[final_kijk$yday==1] <- 0
final_kijk$F_Cabaret_kleinkunst[final_kijk$yday==1] <- 0
final_kijk$F_Overige_sportreportage[final_kijk$yday==1] <- 0
final_kijk$F_Overige_sportinformatie[final_kijk$yday==1] <- 0
final_kijk$F_Tekstuele_informatie[final_kijk$yday==1] <- 0
final_kijk$F_Weerbericht[final_kijk$yday==1] <- 0
final_kijk$Voetball[final_kijk$yday==1] <- 0
#### kijk after #####
final_kijk$Date <- ymd(final_kijk$Date)
final_kijk_2 <- left_join(final_kijk,tw,by='Date')
final_kijk_2 <- final_kijk_2[,-c(8)]
final_kijk_2 <- mutate(final_kijk_2,Temp=ifelse(is.na(Temp)==TRUE,0,Temp))
final_kijk_2 <- mutate(final_kijk_2,Rain=ifelse(is.na(Rain)==TRUE,0,Rain))
final_kijk_2 <- unique(final_kijk_2)

#-----------------------------------------------------------------------------------------#


#### import sanoma data SS####
#2015
sanoma150103 <- read.csv2("sanomaSS150103.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma150406 <- read.csv2("sanomaSS150406.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma150709 <- read.csv2("sanomaSS150709.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma151012 <- read.csv2("sanomaSS151012.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
#2016
sanoma160102 <- read.csv2("sanomaSS160102.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma160304 <- read.csv2("sanomaSS160304.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
sanoma160506 <- read.csv2("sanomaSS160506.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")
#sanomaSS1607.csv
sanoma1607 <- read.csv2("sanomaSS1607.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "")


totalSanoma <- rbind(sanoma150103,sanoma150406,sanoma150709,sanoma151012,sanoma160102,sanoma160304,sanoma160506,sanoma1607)
totalSanoma$Day <- as.character(totalSanoma$Day)
totalSanoma$ns_st_cu <- as.character(totalSanoma$ns_st_cu)
totalSanoma$Stream.starts <- as.character(totalSanoma$Stream.starts)
totalSanoma$Stream.starts <- gsub("\\.","",totalSanoma$Stream.starts)
totalSanoma$Stream.starts <- as.numeric(totalSanoma$Stream.starts)

#### join sanoma with weather and holiday - generate date vars #####
final_sanoma <- totalSanoma
final_sanoma$Day <- dmy(final_sanoma$Day)
colnames(final_sanoma) <- c("Date","Site","StreamStarts")
final_sanoma <- left_join(final_sanoma,tw,by='Date')
final_sanoma <- mutate(final_sanoma,Temp=ifelse(is.na(Temp)==TRUE,0,Temp))
final_sanoma <- mutate(final_sanoma,Rain=ifelse(is.na(Rain)==TRUE,0,Rain))
#summarize all sanoma sites 
final_sanoma <- group_by(final_sanoma,Date,Temp,Rain)
final_sanoma <- summarize(final_sanoma,StreamStarts=sum(StreamStarts))
final_sanoma <- left_join(final_sanoma,totalHol,by='Date')
final_sanoma <- final_sanoma[,c(1:4,6)]
for(i in 1:nrow(final_sanoma)){
  if(is.na(final_sanoma$Holiday.name[i])==TRUE){
    final_sanoma$Holiday.name[i] <- "NH"
  }
}
final_sanoma$wday <- wday(final_sanoma$Date)
final_sanoma$yday <- yday(final_sanoma$Date)
final_sanoma$month <- month(final_sanoma$Date)
final_sanoma$mday <- mday(final_sanoma$Date)
colnames(final_sanoma) <- c("Date","Temp","Rain","n","Holiday.name.x","wday","yday","month","mday")

#-----------------------------------------------------------------------------------------#



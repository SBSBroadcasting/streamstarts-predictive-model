library(lubridate)
library(dplyr)
library(data.table)

#this script contains the functionality of getting the programme name list from the optimizer and retrieving a subset based on those programmes from the dataset
#### online data ss ####
dataset$V2 <- tolower(dataset$V2)
dataset$V2 <- gsub(" - seizoen \\d{1,2}$", "", dataset$V2)
dataset$V2 <- gsub(" aflevering \\d{1,3}$", "", dataset$V2)
dataset$V2 <- gsub(" aflevering \\d{1,3} en \\d{1,3}$", "", dataset$V2)
dataset$V2 <- gsub(": aflevering \\d{1,3}$", "", dataset$V2)
dataset <- dataset[(dataset$V2!=" "),]
#### import tv freq for historical NE and Freq ####
ProgHistorical <- read.csv2("TVFreqs.csv", header = TRUE, sep = ",", dec = ",", quote = "\"", stringsAsFactors = FALSE)
genres <- ProgHistorical[,c(1,3:4,7)]
ProgHistorical$Date <- dmy(ProgHistorical$Date)
ProgHistorical$Main.Title <- tolower(ProgHistorical$Main.Title)
ProgHistorical <- ProgHistorical[,c(1,3,4,5)]
ProgHistorical <- unique(ProgHistorical)
ProgHistorical$Freq <- 0
ProgHistorical$NE <- 0
ProgHistorical <- mutate(ProgHistorical, Freq=ifelse((Type..Frequency=="EERSTE UITZENDING"),1,Freq))
ProgHistorical <- ProgHistorical[,c(1,2,5,6)]
#### Import FUTURE SCHEDULE ####
temp = list.files(pattern="List_20160905_0930.csv")
futureSchedule = as.data.frame(lapply(temp, read.csv2))
#futureSchedule <- read.csv2(, header = TRUE, sep = ";", dec = ",", quote = "\"", stringsAsFactors = FALSE )
futureSchedule$Product <- as.character(futureSchedule$Product)
futureSchedule$Start.date <- as.character(futureSchedule$Start.date)
futureSchedule$Platforms <- as.character(futureSchedule$Platforms)
futureSchedule <- futureSchedule[,c(1,3,4)]
futureSchedule$Product <- tolower(futureSchedule$Product)
futureSchedule$Platforms <- tolower(futureSchedule$Platforms)
futureSchedule$Platforms <- gsub("sbs 6","sbs6",futureSchedule$Platforms)
futureSchedule$Platforms <- gsub("sbs 9","sbs9",futureSchedule$Platforms)
futureSchedule$Platforms <- gsub("net 5","net5",futureSchedule$Platforms)
futureSchedule$Start.date <- dmy(futureSchedule$Start.date)
futureSchedule$Product <- gsub(".*@","",futureSchedule$Product)
futureSchedule$Product <- gsub(" - .*","",futureSchedule$Product)
futureSchedule$Product <- gsub("\\)","",futureSchedule$Product)
#### Import genres ####
genres$Date <- dmy(genres$Date)
genres$Main.Title <- tolower(genres$Main.Title)
genres$Prog.Share <- as.numeric(genres$Prog.Share)
colnames(genres) <- c('Date','programme','genre','share')
genres <- group_by(genres, Date, programme, genre) %>% summarize(share=mean(share))

genreShareMeans <- genres[,c(3,4)]
genreShareMeans <- group_by(genreShareMeans, genre) %>% summarize(share=mean(share))
#ADD COLUMN OF MEAN VALUES TO LIST
programmeList2 <- left_join(programmeList2, genreShareMeans, by='genre')

##### import file of holidays #####
total_hol <- rbind(read.csv2("holidays15.csv", header = TRUE, sep = ";", dec =",", quote = "\"", stringsAsFactors = FALSE), read.csv2("holidays16.csv", header = TRUE, sep = ";", dec =",", quote = "\"", stringsAsFactors = FALSE))
total_hol$Date <- ymd(total_hol$Date)
total_hol <- total_hol[,c(1,3)]
colnames(total_hol) <- c("Day","Holiday")

#### FILL here programme name or keyword of title #####
programme <- programmeList1 
dailyResults1 <- as.list(NA)
#check in scheduling data for this keyword and take subset
for(i in 1:length(programme)){
  futuresubset <- futureSchedule[grepl(programme[i], futureSchedule$Product),]
  if(length(futuresubset$Product)!=0){
    futuresubset$n <- 1
    futuresubset <- group_by(futuresubset, Start.date) %>% summarize(n=sum(n))
    colnames(futuresubset) <- c('Day','Freq')
#### check in online data for this keyword and take subset ####
    subsetdata <- dataset[grepl(programme[i], dataset$V2), c(1,4)] %>% group_by(V1) %>% summarize(V4=sum(V4))
    colnames(subsetdata) <- c('Day','Stream.starts')
#### create dates columns ####
    subsetdata$wday <- wday(subsetdata$Day)
    subsetdata$mday <- mday(subsetdata$Day)
    subsetdata$yday <- yday(subsetdata$Day)
    subsetdata$week <- week(subsetdata$Day)
    subsetdata$month <- month(subsetdata$Day)
    subsetdata$year <- year(subsetdata$Day)
#### Join with genre table ####
    genresubset <- genres[grepl(programme[i], genres$programme), c(1,4)] %>% group_by(Date) %>% summarize(share=sum(share))
    colnames(genresubset) <- c('Day','share')
    subsetdata <- left_join(subsetdata, genresubset, by=c('Day'))
    subsetdata <- mutate(subsetdata, share=ifelse(is.na(share)==TRUE, 0, share))
#### bind holidays ####
    subsetdata <- left_join(subsetdata, total_hol, by="Day")
    subsetdata <- mutate(subsetdata, Holiday=ifelse(is.na(Holiday)==TRUE, "NO", Holiday))
    subsetdata <- unique(subsetdata)
#### bind table with ProgHistorical ####
    subsetProgHistorical <- unique(ProgHistorical[grepl(programme[i], ProgHistorical$Main.Title), c(1,3,4)])
    subsetProgHistorical <- group_by(subsetProgHistorical, Date, NE) %>% summarize(Freq=sum(Freq))
    colnames(subsetProgHistorical) <- c('Day','NE','Freq')
    subsetdata <- left_join(subsetdata, subsetProgHistorical, by=c('Day'))
    subsetdata <- mutate(subsetdata, Freq=ifelse(is.na(Freq)==TRUE, 0, Freq))
    subsetdata <- mutate(subsetdata, NE=ifelse(is.na(NE)==TRUE, 0, NE))
#### create column NE for days from NewEpisodes ####
    pos <- 1
    for(j in 2:nrow(subsetdata)){
      if(subsetdata$Freq[j] == 0 & subsetdata$NE[j] == 0 & subsetdata$NE[j-1] > 0){
        pos <- 1 + subsetdata$NE[j-1]
        subsetdata$NE[j] <- pos
      }else if(subsetdata$Freq[j] != 0){
        subsetdata$NE[j] <- 1
      }
    }
#### create future table ####
    futureTbl <- as.data.frame(seq.Date(Sys.Date(), Sys.Date() + 60, by='day'))
    colnames(futureTbl) <- c('Day')
    futureTbl$Day <- as.POSIXct(futureTbl$Day)
    futureTbl$Freq <- 0
    futureTbl$NE <- 0
    # bind table with futuredataset 
    futureTbl <- rbind(futureTbl,futuresubset[,c(1,3,2)])
    futureTbl <- right_join(total_hol,futureTbl)
    for(j in 1:nrow(futureTbl)){
      if(is.na(futureTbl$Holiday[j])==TRUE){
        futureTbl$Holiday[j] <- "NO"
      }
    }
    futureTbl$Stream.starts <- 0
    futureTbl$wday <- wday(futureTbl$Day)
    futureTbl$mday <- mday(futureTbl$Day)
    futureTbl$yday <- yday(futureTbl$Day)
    futureTbl$week <- week(futureTbl$Day)
    futureTbl$month <- month(futureTbl$Day)
    futureTbl$year <- year(futureTbl$Day)
    futureTbl$share <- 0
    futureTbl <- unique(futureTbl)
    futureTbl <- futureTbl[,c(1,5,6:11,12,2:4)]
    
    #transform them
    pos <- 1
    for(j in 2:nrow(futureTbl)){
      if(futureTbl$Freq[j] == 0 & futureTbl$NE[j] == 0 & futureTbl$NE[j-1] > 0){
        pos <- 1 + subsetdata$NE[j-1]
        futureTbl$NE[j] <- pos
      }else if(futureTbl$Freq[j] != 0){
        futureTbl$NE[j] <- 1
      }
    }

#### Predict future ####
    library(randomForest)
    library(plotly)
    
    SRC_PATH <- 'W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-programmes/StreamStarts-predictive-model-programmes'
    train <- subsetdata[subsetdata$Day <='2016-08-31',]
    test <- futureTbl
    train$Holiday <- as.factor(as.numeric(as.factor(train$Holiday)))
    test$Holiday <- as.factor(as.numeric(as.factor(test$Holiday)))
    levels(test$Holiday)=levels(train$Holiday)
    #### create a linear model using the training partition ####
    pmodel <- lm(Stream.starts ~ wday
                 + mday
                 + yday
                 + week
                 + month
                 + year
                 + Freq
                 + share
                 + Holiday
                 + NE
                 ,train)
    
    summary(pmodel)
    # save the model to disk
    save(pmodel, file=paste(SRC_PATH,'pmodel.rds', sep=","))
    # load the model back from disk (prior variable name is restored)
    load(paste(SRC_PATH,'pmodel.rds', sep=","))
    # score the test data and append it as a new column (for later use)
    
    
    train$p_n <- predict(pmodel, train)
    #train$LiftLM <- train$p_n - train$Stream.starts
    test <- cbind(test,'p_n' = predict(pmodel, test))
    #test$LiftLM <- test$p_n - test$Stream.starts
    
    #### RF ####
    #train model
    prfmodel.rf <- randomForest(Stream.starts ~ wday
                                + mday
                                + yday
                                + week
                                + month
                                + year
                                + Holiday
                                + Freq
                                + share
                                + NE
                                , data=train,
                                importance=TRUE,
                                ntree=1000)
    
    prfmodel.pred <- predict(prfmodel.rf, train)
    train$preds <- prfmodel.pred 
    #train$LiftRF <- train$preds - train$Stream.starts
    
    prfmodel.pred <- predict(prfmodel.rf, test)
    test$preds <- prfmodel.pred 
    #test$LiftRF <- test$preds - test$Stream.starts
    
    
    #rbind all results
    # totalResults <- train[,c(1,2,15:18)]
    # totalResults <- rbind(totalResults, test[,c(1,2,15:18)])
    # 
    # #write date & preds in a csv
    # write.csv2(totalResults,paste0(programme, "_predictions_", Sys.Date(), ".csv"))
    # 
    # 
    # library(DAAG)
    # #### plot ####
    # p <- plot_ly(totalResults,x=totalResults$Day, y=totalResults$Stream.starts, name="Actuals") %>% 
    #   add_trace(x=totalResults$Day, y=totalResults$p_n, name="lm"
    #   ) %>% add_trace(x=totalResults$Day, y=totalResults$preds, name="rf"
    #                   # ) %>% add_trace(x=totalResults$Day, y=totalResults$LiftLM, name="Lift LM"
    #                   # ) %>% add_trace(x=totalResults$Day, y=totalResults$LiftRF, name="Lift RF"
    #   ) %>% layout(title=capstring(programme), xaxis=list(title='Day'), yaxis=list(title='Stream starts'))
    # p
    
    
    # r2_lm <- sqrt(mean((test$LiftLM)^2)) 
    # r2_rf <- sqrt(mean((test$LiftRF)^2)) 
    
    #plot random forest
    # plot(prfmodel.rf, log="y")
    # varImpPlot(prfmodel.rf)
    
    dailyResults1[[i]] <- test[,c(6,13,14)] %>% group_by(week) %>% summarize(p_n=sum(p_n), preds=sum(preds))
  }else if(length(futuresubset$Product)==0){
   print('to be fixed') 
  }
}


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

#### FILL here programme name or keyword of title #####
programme <- programmeList 
dailyResults1 <- as.list(NA)
#check in scheduling data for this keyword and take subset
for(i in 1:length(programme)){
  
#### check in online data for this keyword and take subset ####
  subsetdata <- dataset[grepl(programme[i], dataset$V2), c(1,4)] %>% group_by(V1) %>% summarize(V4=sum(V4))
  colnames(subsetdata) <- c('Day','Stream.starts')
  
  if(nrow(subsetdata)!=0){
    
#### create dates columns ####
    subsetdata$wday <- wday(subsetdata$Day)
    subsetdata$week <- week(subsetdata$Day)

#### Join with genre table ####
    genresubset <- genres[grepl(programme[i], genres$programme), c(1,4)] %>% group_by(Date) %>% summarize(share=sum(share))
    colnames(genresubset) <- c('Day','share')
    subsetdata <- left_join(subsetdata, genresubset, by=c('Day'))
    subsetdata <- mutate(subsetdata, share=ifelse(is.na(share)==TRUE, 0, share))
    
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
    
#### check which is the day-week period of the programmes in the list & create future schedule ####
    schedule <- Optiondf[grepl(programme[2], Optiondf$Program.y),c(14,16)]
    colnames(schedule) <- c('wday','week')

#### create future table ####
    futureTbl <- as.data.frame(seq.Date(Sys.Date(), Sys.Date() + 60, by='day'))
    colnames(futureTbl) <- c('Day')
    futureTbl$Day <- as.POSIXct(futureTbl$Day)
    futureTbl$Freq <- 0
    futureTbl$NE <- 0
    futureTbl$Stream.starts <- 0
    futureTbl$wday <- wday(futureTbl$Day)
    futureTbl$week <- week(futureTbl$Day)
    futureTbl$month <- month(futureTbl$Day)
    futureTbl$share <- 0
    futureTbl <- unique(futureTbl)

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
                 + week
                 + month
                 + year
                 + Freq
                 + share
                 + NE
                 ,train)
    
    summary(pmodel)
    # save the model to disk
    save(pmodel, file=paste(SRC_PATH,'pmodel.rds', sep=","))
    # load the model back from disk (prior variable name is restored)
    load(paste(SRC_PATH,'pmodel.rds', sep=","))
    train$p_n <- predict(pmodel, train)
    test <- cbind(test,'p_n' = predict(pmodel, test))

#### RF ####
    #train model
    prfmodel.rf <- randomForest(Stream.starts ~ wday
                                + week
                                + month
                                + year
                                + Freq
                                + share
                                + NE
                                , data=train,
                                importance=TRUE,
                                ntree=1000)
    
    prfmodel.pred <- predict(prfmodel.rf, train)
    train$preds <- prfmodel.pred 

    prfmodel.pred <- predict(prfmodel.rf, test)
    test$preds <- prfmodel.pred 
    #gather the results of predictions of every programme
    dailyResults1[[i]] <- test[,c(6,13,14)] %>% group_by(week) %>% summarize(p_n=sum(p_n), preds=sum(preds))
    
  }else if(nrow(subsetdata)==0){
    print('to be fixed') 
  }
}


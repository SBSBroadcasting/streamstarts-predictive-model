library(lubridate)
library(dplyr)
#### Predict future ####
SRC_PATH <- 'W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/tryout_prediction_v1/'
futureR <- read.csv2('W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/tryout_prediction_v1/bindTable.csv', header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
futureR <- futureR[futureR$channel=='net5',-c(1)]
futureR$V1 <- as.character(futureR$V1)
futureR$V1 <- ymd(futureR$V1)
futureR$channel <- as.character(futureR$channel)

futureR$c1 <- 0
futureR$c2 <- 0
futureR$c3 <- 0
futureR$c4 <- 0
futureR$c5 <- 0
futureR$c6 <- 0
futureR$c7 <- 0
futureR$c8 <- 0
futureR$c9 <- 0
futureR$c10 <- 0
futureR$c11 <- 0

futureR <- mutate(futureR, c1=ifelse(futureR$type=='1',1,c1),
                  c2=ifelse(futureR$type=='2',1,c2),
                  c3=ifelse(futureR$type=='3',1,c3),
                  c4=ifelse(futureR$type=='4',1,c4),
                  c5=ifelse(futureR$type=='5',1,c5),
                  c6=ifelse(futureR$type=='6',1,c6),
                  c7=ifelse(futureR$type=='7',1,c7),
                  c8=ifelse(futureR$type=='8',1,c8),
                  c9=ifelse(futureR$type=='9',1,c9),
                  c10=ifelse(futureR$type=='10',1,c10),
                  c11=ifelse(futureR$type=='11',1,c11)
)
futureR <- group_by(futureR, V1, mday, week, month, yday, year, channel)
futureR <- summarize(futureR, n=sum(V4),
                     c1=sum(c1),
                     c2=sum(c2),
                     c3=sum(c3),
                     c4=sum(c4),
                     c5=sum(c5),
                     c6=sum(c6),
                     c7=sum(c7),
                     c8=sum(c8),
                     c9=sum(c9),
                     c10=sum(c10),
                     c11=sum(c11)
)
#join with weather
colnames(tw) <- c('V1',"Temp","Rain")
futureR <- right_join(futureR, tw, by='V1')
futureR$Temp <- as.numeric(futureR$Temp)
futureR$Rain <- as.numeric(futureR$Rain)
trainnet5 <- futureR[(futureR$year=='2015')|(futureR$year=='2016'&futureR$month<4),]
testnet5 <- futureR[futureR$year=='2016'&futureR$month>3,]
#### create a linear model using the training partition ####
pmodelnet5 <- lm(n ~ mday
                 + week
                 + month
                 + yday
                 + c1
                 #+ c2
                 + c3 
                 + c4
                 + c5
                 + c6 
                 + c7
                 #+ c8
                 + c9 
                 + c10
                 + c11
                 + Temp
                 + Rain
                 ,trainnet5)

summary(pmodelnet5)

# save the model to disk
save(pmodelnet5, file=paste(SRC_PATH,'pmodelnet5.rds', sep=","))
# load the model back from disk (prior variable name is restored)
load(paste(SRC_PATH,'pmodelnet5.rds', sep=","))
# score the test data and plot pred vs. obs 
RESULT <- data.frame('Predicted'=predict(pmodelnet5, testnet5), 'Observed' = testnet5$n)
#plot(RESULT)
# score the test data and append it as a new column (for later use)
testnet5 <- cbind(testnet5,'p_n' = predict(pmodelnet5, testnet5))
testnet5$Lift_lm <- testnet5$p_n - testnet5$n
#### RF ####
#library(randomForest)
#train model
#prfmodelnet5.rf <- randomForest(n ~ mday
#                                + month
#                                + week
#                                + yday
#                                + c1
#                                + c2
#                                + c3 
#                                + c4
#                                + c5
                                # + c6 
                                # + c7
                                # + c8
                                # + c9 
                                # + c10
                                # + c11
                                # + Temp
                                # + Rain
                                # , data=trainnet5,
                                # importance=TRUE,
                                # ntree=1000)

# prfmodelnet5.pred <- predict(prfmodelnet5.rf, testnet5)
# 
# testnet5$preds <- prfmodelnet5.pred 
# testnet5$Lift_rf <- testnet5$preds - testnet5$n
# 
# library(plotly)
# prfor <- plot_ly(testnet5,x=testnet5$V1, y=testnet5$n, name="Actuals") %>% 
#   add_trace(testnet5,x=testnet5$V1, y=testnet5$preds, name="predicted") #%>%
# add_trace(testnet5,x=testnet5$V1, y=testnet5$Lift_rf, name="Lift_rf")
# prfor
# 

####Inventory model####
ssresults <- testnet5[,c(1,8,22)]
ssresults <- mutate(ssresults,n=ifelse(is.na(n)==TRUE,p_n,n))
ssresults <- ssresults[,c(1,2)]
sshist <- trainnet5[,c(1,8)]
#merge ss data
ssdata <- rbind(ssresults,sshist)
#import inventory
#total#
PrInvnet5 <- ssdata
PrInvnet5 <- left_join(PrInvnet5,totalInvnet5[,c(1,3:5)], by="V1")
PrInvnet5 <- group_by(PrInvnet5,V1,n, Temp, Rain)
PrInvnet5 <- summarize(PrInvnet5,inventory=sum(inventory))
PrInvnet5$mday <- mday(PrInvnet5$V1)
PrInvnet5$week <- week(PrInvnet5$V1)
PrInvnet5$month <- month(PrInvnet5$V1)
PrInvnet5$yday <- yday(PrInvnet5$V1)
PrInvnet5$year <- year(PrInvnet5$V1)
PrInvnet5$Temp <- as.numeric(PrInvnet5$Temp)
PrInvnet5$Rain <- as.numeric(PrInvnet5$Rain)

invTrain_net5 <- PrInvnet5[(PrInvnet5$year=='2014')|(PrInvnet5$year=='2015')|(PrInvnet5$year=='2016'&PrInvnet5$month<4),]
invTest_net5  <- PrInvnet5[PrInvnet5$year=='2016'&PrInvnet5$month>3,]
#preroll
PrInvnet5_proll <- ssdata
PrInvnet5_proll <- right_join(PrInvnet5_proll,tw, by="V1")
PrInvnet5_proll <- left_join(PrInvnet5_proll,totalInvnet5[totalInvnet5$Ftype=="Preroll",c(1,3)], by="V1")
PrInvnet5_proll <- group_by(PrInvnet5_proll,V1,n, Temp, Rain)
PrInvnet5_proll <- summarize(PrInvnet5_proll,inventory=sum(inventory))
PrInvnet5_proll$mday <- mday(PrInvnet5_proll$V1)
PrInvnet5_proll$week <- week(PrInvnet5_proll$V1)
PrInvnet5_proll$month <- month(PrInvnet5_proll$V1)
PrInvnet5_proll$yday <- yday(PrInvnet5_proll$V1)
PrInvnet5_proll$year <- year(PrInvnet5_proll$V1)
PrInvnet5_proll$Temp <- as.numeric(PrInvnet5_proll$Temp)
PrInvnet5_proll$Rain <- as.numeric(PrInvnet5_proll$Rain)

for(i in 1:nrow(PrInvnet5_proll)){
  PrInvnet5_proll$ind[i] <- mean(PrInvnet5_proll$inventory[PrInvnet5_proll$week==PrInvnet5_proll$week[i]-1])
  if(is.na(PrInvnet5_proll$ind[i])==TRUE){
    PrInvnet5_proll$ind[i] <- mean(PrInvnet5_proll$inventory[PrInvnet5_proll$week==PrInvnet5_proll$week[i]-2])
    if(is.na(PrInvnet5_proll$ind[i])==TRUE){
      PrInvnet5_proll$ind[i] <- mean(PrInvnet5_proll$inventory[PrInvnet5_proll$week==PrInvnet5_proll$week[i]-3])
      if(is.na(PrInvnet5_proll$ind[i])==TRUE){
        PrInvnet5_proll$ind[i] <- mean(PrInvnet5_proll$inventory[PrInvnet5_proll$week==PrInvnet5_proll$week[i]-4])
        if(is.na(PrInvnet5_proll$ind[i])==TRUE){
          PrInvnet5_proll$ind[i] <- mean(PrInvnet5_proll$ind[PrInvnet5_proll$month==PrInvnet5_proll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvnet5_proll$ind[i])==TRUE){
    PrInvnet5_proll$ind[i] <- mean(PrInvnet5_proll$inventory[PrInvnet5_proll$week==PrInvnet5_proll$week[i]])
  }
}

invTrain_net5_proll <- PrInvnet5_proll[(PrInvnet5_proll$year=='2014')|(PrInvnet5_proll$year=='2015')|(PrInvnet5_proll$year=='2016'&PrInvnet5_proll$month<4),]
invTest_net5_proll <- PrInvnet5_proll[PrInvnet5_proll$year=='2016'&PrInvnet5_proll$month>3,]
#midroll
PrInvnet5_mroll <- ssdata
PrInvnet5_mroll <- right_join(PrInvnet5_mroll,tw, by="V1")
PrInvnet5_mroll <- left_join(PrInvnet5_mroll,totalInvnet5[totalInvnet5$Ftype=="Midroll",c(1,3)], by="V1")
PrInvnet5_mroll <- group_by(PrInvnet5_mroll,V1,n, Temp, Rain)
PrInvnet5_mroll <- summarize(PrInvnet5_mroll,inventory=sum(inventory))
PrInvnet5_mroll$mday <- mday(PrInvnet5_mroll$V1)
PrInvnet5_mroll$week <- week(PrInvnet5_mroll$V1)
PrInvnet5_mroll$month <- month(PrInvnet5_mroll$V1)
PrInvnet5_mroll$yday <- yday(PrInvnet5_mroll$V1)
PrInvnet5_mroll$year <- year(PrInvnet5_mroll$V1)
PrInvnet5_mroll$Temp <- as.numeric(PrInvnet5_mroll$Temp)
PrInvnet5_mroll$Rain <- as.numeric(PrInvnet5_mroll$Rain)

for(i in 1:nrow(PrInvnet5_mroll)){
  PrInvnet5_mroll$ind[i] <- mean(PrInvnet5_mroll$inventory[PrInvnet5_mroll$week==PrInvnet5_mroll$week[i]-1])
  if(is.na(PrInvnet5_mroll$ind[i])==TRUE){
    PrInvnet5_mroll$ind[i] <- mean(PrInvnet5_mroll$inventory[PrInvnet5_mroll$week==PrInvnet5_mroll$week[i]-2])
    if(is.na(PrInvnet5_mroll$ind[i])==TRUE){
      PrInvnet5_mroll$ind[i] <- mean(PrInvnet5_mroll$inventory[PrInvnet5_mroll$week==PrInvnet5_mroll$week[i]-3])
      if(is.na(PrInvnet5_mroll$ind[i])==TRUE){
        PrInvnet5_mroll$ind[i] <- mean(PrInvnet5_mroll$inventory[PrInvnet5_mroll$week==PrInvnet5_mroll$week[i]-4])
        if(is.na(PrInvnet5_mroll$ind[i])==TRUE){
          PrInvnet5_mroll$ind[i] <- mean(PrInvnet5_mroll$ind[PrInvnet5_mroll$month==PrInvnet5_mroll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvnet5_mroll$ind[i])==TRUE){
    PrInvnet5_mroll$ind[i] <- mean(PrInvnet5_mroll$inventory[PrInvnet5_mroll$week==PrInvnet5_mroll$week[i]])
  }
}

invTrain_net5_mroll <- PrInvnet5_mroll[(PrInvnet5_mroll$year=='2014')|(PrInvnet5_mroll$year=='2015')|(PrInvnet5_mroll$year=='2016'&PrInvnet5_mroll$month<4),]
invTest_net5_mroll <- PrInvnet5_mroll[PrInvnet5_mroll$year=='2016'&PrInvnet5_mroll$month>3,]
#postroll
PrInvnet5_postroll <- ssdata
PrInvnet5_postroll <- right_join(PrInvnet5_postroll,tw, by="V1")
PrInvnet5_postroll <- left_join(PrInvnet5_postroll,totalInvnet5[totalInvnet5$Ftype=="Postroll",c(1,3)], by="V1")
PrInvnet5_postroll <- group_by(PrInvnet5_postroll,V1,n, Temp, Rain)
PrInvnet5_postroll <- summarize(PrInvnet5_postroll,inventory=sum(inventory))
PrInvnet5_postroll$mday <- mday(PrInvnet5_postroll$V1)
PrInvnet5_postroll$week <- week(PrInvnet5_postroll$V1)
PrInvnet5_postroll$month <- month(PrInvnet5_postroll$V1)
PrInvnet5_postroll$yday <- yday(PrInvnet5_postroll$V1)
PrInvnet5_postroll$year <- year(PrInvnet5_postroll$V1)
PrInvnet5_postroll$Temp <- as.numeric(PrInvnet5_postroll$Temp)
PrInvnet5_postroll$Rain <- as.numeric(PrInvnet5_postroll$Rain)

for(i in 1:nrow(PrInvnet5_postroll)){
  PrInvnet5_postroll$ind[i] <- mean(PrInvnet5_postroll$inventory[PrInvnet5_postroll$week==PrInvnet5_postroll$week[i]-1])
  if(is.na(PrInvnet5_postroll$ind[i])==TRUE){
    PrInvnet5_postroll$ind[i] <- mean(PrInvnet5_postroll$inventory[PrInvnet5_postroll$week==PrInvnet5_postroll$week[i]-2])
    if(is.na(PrInvnet5_postroll$ind[i])==TRUE){
      PrInvnet5_postroll$ind[i] <- mean(PrInvnet5_postroll$inventory[PrInvnet5_postroll$week==PrInvnet5_postroll$week[i]-3])
      if(is.na(PrInvnet5_postroll$ind[i])==TRUE){
        PrInvnet5_postroll$ind[i] <- mean(PrInvnet5_postroll$inventory[PrInvnet5_postroll$week==PrInvnet5_postroll$week[i]-4])
        if(is.na(PrInvnet5_postroll$ind[i])==TRUE){
          PrInvnet5_postroll$ind[i] <- mean(PrInvnet5_postroll$ind[PrInvnet5_postroll$month==PrInvnet5_postroll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvnet5_postroll$ind[i])==TRUE){
    PrInvnet5_postroll$ind[i] <- mean(PrInvnet5_postroll$inventory[PrInvnet5_postroll$week==PrInvnet5_postroll$week[i]])
  }
}
invTrain_net5_postroll <- PrInvnet5_postroll[(PrInvnet5_postroll$year=='2014')|(PrInvnet5_postroll$year=='2015')|(PrInvnet5_postroll$year=='2016'&PrInvnet5_postroll$month<4),]
invTest_net5_postroll <- PrInvnet5_postroll[PrInvnet5_postroll$year=='2016'&PrInvnet5_postroll$month>3,]

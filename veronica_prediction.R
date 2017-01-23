####prediction#####
predver <- rbind(vertvSSSum, futureDates)
predver <- right_join(predver, tw, by='V1')

predver$mday <- mday(predver$V1)
predver$week <- week(predver$V1)
predver$month <- month(predver$V1)
predver$yday <- yday(predver$V1)
predver$year <- year(predver$V1)
predver$Temp <- as.numeric(predver$Temp)
predver$Rain <- as.numeric(predver$Rain)

trainver <- predver[(predver$year=='2014')|(predver$year=='2015')|(predver$year=='2016'&predver$month<4),]
testver <- predver[predver$year=='2016'&predver$month>3,]

#### create a linear model using the training partition ####
# pmodelver <- lm(n ~ mday
#                 + week
#                 + month
#                 + yday
#                 + Temp
#                 + Rain
#                 ,trainver)
# 
# summary(pmodelver)
# 
# # save the model to disk
# save(pmodelver, file=paste(SRC_PATH,'pmodelver.rds', sep=","))
# # load the model back from disk (prior variable name is restored)
# load(paste(SRC_PATH,'pmodelver.rds', sep=","))
# # score the test data and plot pred vs. obs 
# RESULT <- data.frame('Predicted'=predict(pmodelver, testver), 'Observed' = testver$n)
# #plot(RESULT)
# # score the test data and append it as a new column (for later use)
# testver <- cbind(testver,'p_n' = predict(pmodelver, testver))
# testver$Lift_lm <- testver$p_n - testver$n
#### RF ####
library(randomForest)
#train model
prfmodelver.rf <- randomForest(n ~ mday
                               + month
                               + week
                               + yday
                               + Temp
                               + Rain
                               , data=trainver,
                               importance=TRUE,
                               ntree=1000)

prfmodelver.pred <- predict(prfmodelver.rf, testver)

testver$preds <- prfmodelver.pred 

####Inventory model####
ssresults <- testver[,c(1,2,10)]
ssresults <- mutate(ssresults,n=ifelse(is.na(n)==TRUE,preds,n))
ssresults <- ssresults[,c(1,2)]
sshist <- trainver[,c(1,2)]
#merge ss data
ssdata <- rbind(ssresults,sshist)
#import inventory
#total#
PrInvveronica <- ssdata
PrInvveronica <- left_join(PrInvveronica,totalInvveronica[,c(1,3:5)], by="V1")
PrInvveronica <- group_by(PrInvveronica,V1,n, Temp, Rain)
PrInvveronica <- summarize(PrInvveronica,inventory=sum(inventory))
PrInvveronica$mday <- mday(PrInvveronica$V1)
PrInvveronica$week <- week(PrInvveronica$V1)
PrInvveronica$month <- month(PrInvveronica$V1)
PrInvveronica$yday <- yday(PrInvveronica$V1)
PrInvveronica$year <- year(PrInvveronica$V1)
PrInvveronica$Temp <- as.numeric(PrInvveronica$Temp)
PrInvveronica$Rain <- as.numeric(PrInvveronica$Rain)

invTrain_veronica <- PrInvveronica[(PrInvveronica$year=='2014')|(PrInvveronica$year=='2015')|(PrInvveronica$year=='2016'&PrInvveronica$month<4),]
invTest_veronica  <- PrInvveronica[PrInvveronica$year=='2016'&PrInvveronica$month>3,]
#preroll
PrInvveronica_proll <- ssdata
PrInvveronica_proll <- right_join(PrInvveronica_proll,tw, by="V1")
PrInvveronica_proll <- left_join(PrInvveronica_proll,totalInvveronica[totalInvveronica$Ftype=="Preroll",c(1,3)], by="V1")
PrInvveronica_proll <- group_by(PrInvveronica_proll,V1,n, Temp, Rain)
PrInvveronica_proll <- summarize(PrInvveronica_proll,inventory=sum(inventory))
PrInvveronica_proll$mday <- mday(PrInvveronica_proll$V1)
PrInvveronica_proll$week <- week(PrInvveronica_proll$V1)
PrInvveronica_proll$month <- month(PrInvveronica_proll$V1)
PrInvveronica_proll$yday <- yday(PrInvveronica_proll$V1)
PrInvveronica_proll$year <- year(PrInvveronica_proll$V1)
PrInvveronica_proll$Temp <- as.numeric(PrInvveronica_proll$Temp)
PrInvveronica_proll$Rain <- as.numeric(PrInvveronica_proll$Rain)

for(i in 1:nrow(PrInvveronica_proll)){
  PrInvveronica_proll$ind[i] <- mean(PrInvveronica_proll$inventory[PrInvveronica_proll$week==PrInvveronica_proll$week[i]-1])
  if(is.na(PrInvveronica_proll$ind[i])==TRUE){
    PrInvveronica_proll$ind[i] <- mean(PrInvveronica_proll$inventory[PrInvveronica_proll$week==PrInvveronica_proll$week[i]-2])
    if(is.na(PrInvveronica_proll$ind[i])==TRUE){
      PrInvveronica_proll$ind[i] <- mean(PrInvveronica_proll$inventory[PrInvveronica_proll$week==PrInvveronica_proll$week[i]-3])
      if(is.na(PrInvveronica_proll$ind[i])==TRUE){
        PrInvveronica_proll$ind[i] <- mean(PrInvveronica_proll$inventory[PrInvveronica_proll$week==PrInvveronica_proll$week[i]-4])
        if(is.na(PrInvveronica_proll$ind[i])==TRUE){
          PrInvveronica_proll$ind[i] <- mean(PrInvveronica_proll$ind[PrInvveronica_proll$month==PrInvveronica_proll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvveronica_proll$ind[i])==TRUE){
    PrInvveronica_proll$ind[i] <- mean(PrInvveronica_proll$inventory[PrInvveronica_proll$week==PrInvveronica_proll$week[i]])
  }
}

invTrain_veronica_proll <- PrInvveronica_proll[(PrInvveronica_proll$year=='2014')|(PrInvveronica_proll$year=='2015')|(PrInvveronica_proll$year=='2016'&PrInvveronica_proll$month<4),]
invTest_veronica_proll <- PrInvveronica_proll[PrInvveronica_proll$year=='2016'&PrInvveronica_proll$month>3,]
#midroll
PrInvveronica_mroll <- ssdata
PrInvveronica_mroll <- right_join(PrInvveronica_mroll,tw, by="V1")
PrInvveronica_mroll <- left_join(PrInvveronica_mroll,totalInvveronica[totalInvveronica$Ftype=="Midroll",c(1,3)], by="V1")
PrInvveronica_mroll <- group_by(PrInvveronica_mroll,V1,n, Temp, Rain)
PrInvveronica_mroll <- summarize(PrInvveronica_mroll,inventory=sum(inventory))
PrInvveronica_mroll$mday <- mday(PrInvveronica_mroll$V1)
PrInvveronica_mroll$week <- week(PrInvveronica_mroll$V1)
PrInvveronica_mroll$month <- month(PrInvveronica_mroll$V1)
PrInvveronica_mroll$yday <- yday(PrInvveronica_mroll$V1)
PrInvveronica_mroll$year <- year(PrInvveronica_mroll$V1)
PrInvveronica_mroll$Temp <- as.numeric(PrInvveronica_mroll$Temp)
PrInvveronica_mroll$Rain <- as.numeric(PrInvveronica_mroll$Rain)

for(i in 1:nrow(PrInvveronica_mroll)){
  PrInvveronica_mroll$ind[i] <- mean(PrInvveronica_mroll$inventory[PrInvveronica_mroll$week==PrInvveronica_mroll$week[i]-1])
  if(is.na(PrInvveronica_mroll$ind[i])==TRUE){
    PrInvveronica_mroll$ind[i] <- mean(PrInvveronica_mroll$inventory[PrInvveronica_mroll$week==PrInvveronica_mroll$week[i]-2])
    if(is.na(PrInvveronica_mroll$ind[i])==TRUE){
      PrInvveronica_mroll$ind[i] <- mean(PrInvveronica_mroll$inventory[PrInvveronica_mroll$week==PrInvveronica_mroll$week[i]-3])
      if(is.na(PrInvveronica_mroll$ind[i])==TRUE){
        PrInvveronica_mroll$ind[i] <- mean(PrInvveronica_mroll$inventory[PrInvveronica_mroll$week==PrInvveronica_mroll$week[i]-4])
        if(is.na(PrInvveronica_mroll$ind[i])==TRUE){
          PrInvveronica_mroll$ind[i] <- mean(PrInvveronica_mroll$ind[PrInvveronica_mroll$month==PrInvveronica_mroll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvveronica_mroll$ind[i])==TRUE){
    PrInvveronica_mroll$ind[i] <- mean(PrInvveronica_mroll$inventory[PrInvveronica_mroll$week==PrInvveronica_mroll$week[i]])
  }
}

invTrain_veronica_mroll <- PrInvveronica_mroll[(PrInvveronica_mroll$year=='2014')|(PrInvveronica_mroll$year=='2015')|(PrInvveronica_mroll$year=='2016'&PrInvveronica_mroll$month<4),]
invTest_veronica_mroll <- PrInvveronica_mroll[PrInvveronica_mroll$year=='2016'&PrInvveronica_mroll$month>3,]
#postroll
PrInvveronica_postroll <- ssdata
PrInvveronica_postroll <- right_join(PrInvveronica_postroll,tw, by="V1")
PrInvveronica_postroll <- left_join(PrInvveronica_postroll,totalInvveronica[totalInvveronica$Ftype=="Postroll",c(1,3)], by="V1")
PrInvveronica_postroll <- group_by(PrInvveronica_postroll,V1,n, Temp, Rain)
PrInvveronica_postroll <- summarize(PrInvveronica_postroll,inventory=sum(inventory))
PrInvveronica_postroll$mday <- mday(PrInvveronica_postroll$V1)
PrInvveronica_postroll$week <- week(PrInvveronica_postroll$V1)
PrInvveronica_postroll$month <- month(PrInvveronica_postroll$V1)
PrInvveronica_postroll$yday <- yday(PrInvveronica_postroll$V1)
PrInvveronica_postroll$year <- year(PrInvveronica_postroll$V1)
PrInvveronica_postroll$Temp <- as.numeric(PrInvveronica_postroll$Temp)
PrInvveronica_postroll$Rain <- as.numeric(PrInvveronica_postroll$Rain)

for(i in 1:nrow(PrInvveronica_postroll)){
  PrInvveronica_postroll$ind[i] <- mean(PrInvveronica_postroll$inventory[PrInvveronica_postroll$week==PrInvveronica_postroll$week[i]-1])
  if(is.na(PrInvveronica_postroll$ind[i])==TRUE){
    PrInvveronica_postroll$ind[i] <- mean(PrInvveronica_postroll$inventory[PrInvveronica_postroll$week==PrInvveronica_postroll$week[i]-2])
    if(is.na(PrInvveronica_postroll$ind[i])==TRUE){
      PrInvveronica_postroll$ind[i] <- mean(PrInvveronica_postroll$inventory[PrInvveronica_postroll$week==PrInvveronica_postroll$week[i]-3])
      if(is.na(PrInvveronica_postroll$ind[i])==TRUE){
        PrInvveronica_postroll$ind[i] <- mean(PrInvveronica_postroll$inventory[PrInvveronica_postroll$week==PrInvveronica_postroll$week[i]-4])
        if(is.na(PrInvveronica_postroll$ind[i])==TRUE){
          PrInvveronica_postroll$ind[i] <- mean(PrInvveronica_postroll$ind[PrInvveronica_postroll$month==PrInvveronica_postroll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvveronica_postroll$ind[i])==TRUE){
    PrInvveronica_postroll$ind[i] <- mean(PrInvveronica_postroll$inventory[PrInvveronica_postroll$week==PrInvveronica_postroll$week[i]])
  }
}
invTrain_veronica_postroll <- PrInvveronica_postroll[(PrInvveronica_postroll$year=='2014')|(PrInvveronica_postroll$year=='2015')|(PrInvveronica_postroll$year=='2016'&PrInvveronica_postroll$month<4),]
invTest_veronica_postroll <- PrInvveronica_postroll[PrInvveronica_postroll$year=='2016'&PrInvveronica_postroll$month>3,]

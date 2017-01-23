####prediction#####
predsbs9 <- rbind(sbs9SSSum, futureDates)
predsbs9 <- left_join(predsbs9, tw, by='V1')
predsbs9$mday <- mday(predsbs9$V1)
predsbs9$week <- week(predsbs9$V1)
predsbs9$month <- month(predsbs9$V1)
predsbs9$yday <- yday(predsbs9$V1)
predsbs9$year <- year(predsbs9$V1)
predsbs9$Temp <- as.numeric(predsbs9$Temp)
predsbs9$Rain <- as.numeric(predsbs9$Rain)

trainsbs9 <- predsbs9[(predsbs9$year=='2014')|(predsbs9$year=='2015')|(predsbs9$year=='2016'&predsbs9$month<4),]
testsbs9 <- predsbs9[predsbs9$year=='2016'&predsbs9$month>3,]

#### create a linear model using the training partition ####
# pmodelsbs9 <- lm(n ~ mday
#                  + week
#                  + month
#                  + yday
#                  + Temp
#                  + Rain
#                  ,trainsbs9)
# 
# summary(pmodelsbs9)
# 
# # save the model to disk
# save(pmodelsbs9, file=paste(SRC_PATH,'pmodelsbs9.rds', sep=","))
# # load the model back from disk (prior variable name is restored)
# load(paste(SRC_PATH,'pmodelsbs9.rds', sep=","))
# # score the test data and plot pred vs. obs 
# RESULT <- data.frame('Predicted'=predict(pmodelsbs9, testsbs9), 'Observed' = testsbs9$n)
# #plot(RESULT)
# # score the test data and append it as a new column (for later use)
# testsbs9 <- cbind(testsbs9,'p_n' = predict(pmodelsbs9, testsbs9))
# testsbs9$Lift_lm <- testsbs9$p_n - testsbs9$n
#### RF ####
library(randomForest)
#train model
prfmodelsbs9.rf <- randomForest(n ~ mday+ month+ week+ yday+ Temp+ Rain, data=trainsbs9,importance=TRUE,ntree=1000)

prfmodelsbs9.pred <- predict(prfmodelsbs9.rf, testsbs9)
testsbs9$preds <- prfmodelsbs9.pred 

####Inventory model####
ssresults <- testsbs9
ssresults <- mutate(ssresults,n=ifelse(is.na(n)==TRUE,preds,n))
ssresults <- ssresults[,c(1,2)]
sshist <- trainsbs9[,c(1,2)]
#merge ss data
ssdata <- rbind(ssresults,sshist)
#import inventory
#total#
PrInvsbs9 <- ssdata
PrInvsbs9 <- left_join(PrInvsbs9,totalInvsbs9[,c(1,3:5)], by="V1")
PrInvsbs9 <- group_by(PrInvsbs9,V1,n, Temp, Rain)
PrInvsbs9 <- summarize(PrInvsbs9,inventory=sum(inventory))
PrInvsbs9$mday <- mday(PrInvsbs9$V1)
PrInvsbs9$week <- week(PrInvsbs9$V1)
PrInvsbs9$month <- month(PrInvsbs9$V1)
PrInvsbs9$yday <- yday(PrInvsbs9$V1)
PrInvsbs9$year <- year(PrInvsbs9$V1)
PrInvsbs9$Temp <- as.numeric(PrInvsbs9$Temp)
PrInvsbs9$Rain <- as.numeric(PrInvsbs9$Rain)

invTrain_sbs9 <- PrInvsbs9[(PrInvsbs9$year=='2014')|(PrInvsbs9$year=='2015')|(PrInvsbs9$year=='2016'&PrInvsbs9$month<4),]
invTest_sbs9  <- PrInvsbs9[PrInvsbs9$year=='2016'&PrInvsbs9$month>3,]
#preroll
PrInvsbs9_proll <- ssdata
PrInvsbs9_proll <- right_join(PrInvsbs9_proll,tw, by="V1")
PrInvsbs9_proll <- left_join(PrInvsbs9_proll,totalInvsbs9[totalInvsbs9$Ftype=="Preroll",c(1,3)], by="V1")
PrInvsbs9_proll <- group_by(PrInvsbs9_proll,V1,n, Temp, Rain)
PrInvsbs9_proll <- summarize(PrInvsbs9_proll,inventory=sum(inventory))
PrInvsbs9_proll$mday <- mday(PrInvsbs9_proll$V1)
PrInvsbs9_proll$week <- week(PrInvsbs9_proll$V1)
PrInvsbs9_proll$month <- month(PrInvsbs9_proll$V1)
PrInvsbs9_proll$yday <- yday(PrInvsbs9_proll$V1)
PrInvsbs9_proll$year <- year(PrInvsbs9_proll$V1)
PrInvsbs9_proll$Temp <- as.numeric(PrInvsbs9_proll$Temp)
PrInvsbs9_proll$Rain <- as.numeric(PrInvsbs9_proll$Rain)

for(i in 1:nrow(PrInvsbs9_proll)){
  PrInvsbs9_proll$ind[i] <- mean(PrInvsbs9_proll$inventory[PrInvsbs9_proll$week==PrInvsbs9_proll$week[i]-1])
  if(is.na(PrInvsbs9_proll$ind[i])==TRUE){
    PrInvsbs9_proll$ind[i] <- mean(PrInvsbs9_proll$inventory[PrInvsbs9_proll$week==PrInvsbs9_proll$week[i]-2])
    if(is.na(PrInvsbs9_proll$ind[i])==TRUE){
      PrInvsbs9_proll$ind[i] <- mean(PrInvsbs9_proll$inventory[PrInvsbs9_proll$week==PrInvsbs9_proll$week[i]-3])
      if(is.na(PrInvsbs9_proll$ind[i])==TRUE){
        PrInvsbs9_proll$ind[i] <- mean(PrInvsbs9_proll$inventory[PrInvsbs9_proll$week==PrInvsbs9_proll$week[i]-4])
        if(is.na(PrInvsbs9_proll$ind[i])==TRUE){
          PrInvsbs9_proll$ind[i] <- mean(PrInvsbs9_proll$ind[PrInvsbs9_proll$month==PrInvsbs9_proll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvsbs9_proll$ind[i])==TRUE){
    PrInvsbs9_proll$ind[i] <- mean(PrInvsbs9_proll$inventory[PrInvsbs9_proll$week==PrInvsbs9_proll$week[i]])
  }
}

invTrain_sbs9_proll <- PrInvsbs9_proll[(PrInvsbs9_proll$year=='2014')|(PrInvsbs9_proll$year=='2015')|(PrInvsbs9_proll$year=='2016'&PrInvsbs9_proll$month<4),]
invTest_sbs9_proll <- PrInvsbs9_proll[PrInvsbs9_proll$year=='2016'&PrInvsbs9_proll$month>3,]
#midroll
PrInvsbs9_mroll <- ssdata
PrInvsbs9_mroll <- right_join(PrInvsbs9_mroll,tw, by="V1")
PrInvsbs9_mroll <- left_join(PrInvsbs9_mroll,totalInvsbs9[totalInvsbs9$Ftype=="Midroll",c(1,3)], by="V1")
PrInvsbs9_mroll <- group_by(PrInvsbs9_mroll,V1,n, Temp, Rain)
PrInvsbs9_mroll <- summarize(PrInvsbs9_mroll,inventory=sum(inventory))
PrInvsbs9_mroll$mday <- mday(PrInvsbs9_mroll$V1)
PrInvsbs9_mroll$week <- week(PrInvsbs9_mroll$V1)
PrInvsbs9_mroll$month <- month(PrInvsbs9_mroll$V1)
PrInvsbs9_mroll$yday <- yday(PrInvsbs9_mroll$V1)
PrInvsbs9_mroll$year <- year(PrInvsbs9_mroll$V1)
PrInvsbs9_mroll$Temp <- as.numeric(PrInvsbs9_mroll$Temp)
PrInvsbs9_mroll$Rain <- as.numeric(PrInvsbs9_mroll$Rain)

for(i in 1:nrow(PrInvsbs9_mroll)){
  PrInvsbs9_mroll$ind[i] <- mean(PrInvsbs9_mroll$inventory[PrInvsbs9_mroll$week==PrInvsbs9_mroll$week[i]-1])
  if(is.na(PrInvsbs9_mroll$ind[i])==TRUE){
    PrInvsbs9_mroll$ind[i] <- mean(PrInvsbs9_mroll$inventory[PrInvsbs9_mroll$week==PrInvsbs9_mroll$week[i]-2])
    if(is.na(PrInvsbs9_mroll$ind[i])==TRUE){
      PrInvsbs9_mroll$ind[i] <- mean(PrInvsbs9_mroll$inventory[PrInvsbs9_mroll$week==PrInvsbs9_mroll$week[i]-3])
      if(is.na(PrInvsbs9_mroll$ind[i])==TRUE){
        PrInvsbs9_mroll$ind[i] <- mean(PrInvsbs9_mroll$inventory[PrInvsbs9_mroll$week==PrInvsbs9_mroll$week[i]-4])
        if(is.na(PrInvsbs9_mroll$ind[i])==TRUE){
          PrInvsbs9_mroll$ind[i] <- mean(PrInvsbs9_mroll$ind[PrInvsbs9_mroll$month==PrInvsbs9_mroll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvsbs9_mroll$ind[i])==TRUE){
    PrInvsbs9_mroll$ind[i] <- mean(PrInvsbs9_mroll$inventory[PrInvsbs9_mroll$week==PrInvsbs9_mroll$week[i]])
  }
}

invTrain_sbs9_mroll <- PrInvsbs9_mroll[(PrInvsbs9_mroll$year=='2014')|(PrInvsbs9_mroll$year=='2015')|(PrInvsbs9_mroll$year=='2016'&PrInvsbs9_mroll$month<4),]
invTest_sbs9_mroll <- PrInvsbs9_mroll[PrInvsbs9_mroll$year=='2016'&PrInvsbs9_mroll$month>3,]
#postroll
PrInvsbs9_postroll <- ssdata
PrInvsbs9_postroll <- right_join(PrInvsbs9_postroll,tw, by="V1")
PrInvsbs9_postroll <- left_join(PrInvsbs9_postroll,totalInvsbs9[totalInvsbs9$Ftype=="Postroll",c(1,3)], by="V1")
PrInvsbs9_postroll <- group_by(PrInvsbs9_postroll,V1,n, Temp, Rain)
PrInvsbs9_postroll <- summarize(PrInvsbs9_postroll,inventory=sum(inventory))
PrInvsbs9_postroll$mday <- mday(PrInvsbs9_postroll$V1)
PrInvsbs9_postroll$week <- week(PrInvsbs9_postroll$V1)
PrInvsbs9_postroll$month <- month(PrInvsbs9_postroll$V1)
PrInvsbs9_postroll$yday <- yday(PrInvsbs9_postroll$V1)
PrInvsbs9_postroll$year <- year(PrInvsbs9_postroll$V1)
PrInvsbs9_postroll$Temp <- as.numeric(PrInvsbs9_postroll$Temp)
PrInvsbs9_postroll$Rain <- as.numeric(PrInvsbs9_postroll$Rain)

for(i in 1:nrow(PrInvsbs9_postroll)){
  PrInvsbs9_postroll$ind[i] <- mean(PrInvsbs9_postroll$inventory[PrInvsbs9_postroll$week==PrInvsbs9_postroll$week[i]-1])
  if(is.na(PrInvsbs9_postroll$ind[i])==TRUE){
    PrInvsbs9_postroll$ind[i] <- mean(PrInvsbs9_postroll$inventory[PrInvsbs9_postroll$week==PrInvsbs9_postroll$week[i]-2])
    if(is.na(PrInvsbs9_postroll$ind[i])==TRUE){
      PrInvsbs9_postroll$ind[i] <- mean(PrInvsbs9_postroll$inventory[PrInvsbs9_postroll$week==PrInvsbs9_postroll$week[i]-3])
      if(is.na(PrInvsbs9_postroll$ind[i])==TRUE){
        PrInvsbs9_postroll$ind[i] <- mean(PrInvsbs9_postroll$inventory[PrInvsbs9_postroll$week==PrInvsbs9_postroll$week[i]-4])
        if(is.na(PrInvsbs9_postroll$ind[i])==TRUE){
          PrInvsbs9_postroll$ind[i] <- mean(PrInvsbs9_postroll$ind[PrInvsbs9_postroll$month==PrInvsbs9_postroll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvsbs9_postroll$ind[i])==TRUE){
    PrInvsbs9_postroll$ind[i] <- mean(PrInvsbs9_postroll$inventory[PrInvsbs9_postroll$week==PrInvsbs9_postroll$week[i]])
  }
}
invTrain_sbs9_postroll <- PrInvsbs9_postroll[(PrInvsbs9_postroll$year=='2014')|(PrInvsbs9_postroll$year=='2015')|(PrInvsbs9_postroll$year=='2016'&PrInvsbs9_postroll$month<4),]
invTest_sbs9_postroll <- PrInvsbs9_postroll[PrInvsbs9_postroll$year=='2016'&PrInvsbs9_postroll$month>3,]

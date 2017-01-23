####prediction#####
preds1tv <- rbind(s1tvSSSum, futureDates)
preds1tv <- left_join(preds1tv, tw, by='V1')

preds1tv$mday <- mday(preds1tv$V1)
preds1tv$week <- week(preds1tv$V1)
preds1tv$month <- month(preds1tv$V1)
preds1tv$yday <- yday(preds1tv$V1)
preds1tv$year <- year(preds1tv$V1)
preds1tv$Temp <- as.numeric(preds1tv$Temp)
preds1tv$Rain <- as.numeric(preds1tv$Rain)

trains1tv <- preds1tv[(preds1tv$year=='2014')|(preds1tv$year=='2015')|(preds1tv$year=='2016'&preds1tv$month<4),]
tests1tv <- preds1tv[preds1tv$year=='2016'&preds1tv$month>3,]

#### create a linear model using the training partition ####
# pmodels1tv <- lm(n ~ mday
#                  + week
#                  + month
#                  + yday
#                  + Temp
#                  + Rain
#                  ,trains1tv)
# 
# summary(pmodels1tv)
# 
# # save the model to disk
# save(pmodels1tv, file=paste(SRC_PATH,'pmodels1tv.rds', sep=","))
# # load the model back from disk (prior variable name is restored)
# load(paste(SRC_PATH,'pmodels1tv.rds', sep=","))
# # score the test data and plot pred vs. obs 
# RESULT <- data.frame('Predicted'=predict(pmodels1tv, tests1tv), 'Observed' = tests1tv$n)
# #plot(RESULT)
# # score the test data and append it as a new column (for later use)
# tests1tv <- cbind(tests1tv,'p_n' = predict(pmodels1tv, tests1tv))
# tests1tv$Lift_lm <- tests1tv$p_n - tests1tv$n

#### RF ####
library(randomForest)
#train model
prfmodels1tv.rf <- randomForest(n ~ mday
                                + month
                                + week
                                + yday
                                + Temp
                                + Rain
                                , data=trains1tv,
                                importance=TRUE,
                                ntree=1000)

prfmodels1tv.pred <- predict(prfmodels1tv.rf, tests1tv)
tests1tv$preds <- prfmodels1tv.pred 
####Inventory model####
ssresults <- tests1tv[,c(1,2,10)]
ssresults <- mutate(ssresults,n=ifelse(is.na(n)==TRUE,preds,n))
ssresults <- ssresults[,c(1,2)]
sshist <- trains1tv[,c(1,2)]
#merge ss data
ssdata <- rbind(ssresults,sshist)
#total#
PrInvs1tv <- ssdata
PrInvs1tv <- left_join(PrInvs1tv,totalInvs1[,c(1,3:5)], by="V1")
PrInvs1tv <- group_by(PrInvs1tv,V1,n, Temp, Rain)
PrInvs1tv <- summarize(PrInvs1tv,inventory=sum(inventory))
PrInvs1tv$mday <- mday(PrInvs1tv$V1)
PrInvs1tv$week <- week(PrInvs1tv$V1)
PrInvs1tv$month <- month(PrInvs1tv$V1)
PrInvs1tv$yday <- yday(PrInvs1tv$V1)
PrInvs1tv$year <- year(PrInvs1tv$V1)
PrInvs1tv$Temp <- as.numeric(PrInvs1tv$Temp)
PrInvs1tv$Rain <- as.numeric(PrInvs1tv$Rain)

invTrain_s1tv <- PrInvs1tv[(PrInvs1tv$year=='2014')|(PrInvs1tv$year=='2015')|(PrInvs1tv$year=='2016'&PrInvs1tv$month<4),]
invTest_s1tv  <- PrInvs1tv[PrInvs1tv$year=='2016'&PrInvs1tv$month>3,]
#preroll
PrInvs1tv_proll <- ssdata
PrInvs1tv_proll <- right_join(PrInvs1tv_proll,tw, by="V1")
PrInvs1tv_proll <- left_join(PrInvs1tv_proll,totalInvs1[totalInvs1$Ftype=="Preroll",c(1,3)], by="V1")
PrInvs1tv_proll <- group_by(PrInvs1tv_proll,V1,n, Temp, Rain)
PrInvs1tv_proll <- summarize(PrInvs1tv_proll,inventory=sum(inventory))
PrInvs1tv_proll$mday <- mday(PrInvs1tv_proll$V1)
PrInvs1tv_proll$week <- week(PrInvs1tv_proll$V1)
PrInvs1tv_proll$month <- month(PrInvs1tv_proll$V1)
PrInvs1tv_proll$yday <- yday(PrInvs1tv_proll$V1)
PrInvs1tv_proll$year <- year(PrInvs1tv_proll$V1)
PrInvs1tv_proll$Temp <- as.numeric(PrInvs1tv_proll$Temp)
PrInvs1tv_proll$Rain <- as.numeric(PrInvs1tv_proll$Rain)

for(i in 1:nrow(PrInvs1tv_proll)){
  PrInvs1tv_proll$ind[i] <- mean(PrInvs1tv_proll$inventory[PrInvs1tv_proll$week==PrInvs1tv_proll$week[i]-1])
  if(is.na(PrInvs1tv_proll$ind[i])==TRUE){
    PrInvs1tv_proll$ind[i] <- mean(PrInvs1tv_proll$inventory[PrInvs1tv_proll$week==PrInvs1tv_proll$week[i]-2])
    if(is.na(PrInvs1tv_proll$ind[i])==TRUE){
      PrInvs1tv_proll$ind[i] <- mean(PrInvs1tv_proll$inventory[PrInvs1tv_proll$week==PrInvs1tv_proll$week[i]-3])
      if(is.na(PrInvs1tv_proll$ind[i])==TRUE){
        PrInvs1tv_proll$ind[i] <- mean(PrInvs1tv_proll$inventory[PrInvs1tv_proll$week==PrInvs1tv_proll$week[i]-4])
        if(is.na(PrInvs1tv_proll$ind[i])==TRUE){
          PrInvs1tv_proll$ind[i] <- mean(PrInvs1tv_proll$ind[PrInvs1tv_proll$month==PrInvs1tv_proll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvs1tv_proll$ind[i])==TRUE){
    PrInvs1tv_proll$ind[i] <- mean(PrInvs1tv_proll$inventory[PrInvs1tv_proll$week==PrInvs1tv_proll$week[i]])
  }
}

invTrain_s1tv_proll <- PrInvs1tv_proll[(PrInvs1tv_proll$year=='2014')|(PrInvs1tv_proll$year=='2015')|(PrInvs1tv_proll$year=='2016'&PrInvs1tv_proll$month<4),]
invTest_s1tv_proll <- PrInvs1tv_proll[PrInvs1tv_proll$year=='2016'&PrInvs1tv_proll$month>3,]
# #midroll
# PrInvs1tv_mroll <- ssdata
# PrInvs1tv_mroll <- right_join(PrInvs1tv_mroll,tw, by="V1")
# PrInvs1tv_mroll <- left_join(PrInvs1tv_mroll,totalInvs1[totalInvs1$Ftype=="Midroll",c(1,3)], by="V1")
# PrInvs1tv_mroll <- group_by(PrInvs1tv_mroll,V1,n, Temp, Rain)
# PrInvs1tv_mroll <- summarize(PrInvs1tv_mroll,inventory=sum(inventory))
# PrInvs1tv_mroll$mday <- mday(PrInvs1tv_mroll$V1)
# PrInvs1tv_mroll$week <- week(PrInvs1tv_mroll$V1)
# PrInvs1tv_mroll$month <- month(PrInvs1tv_mroll$V1)
# PrInvs1tv_mroll$yday <- yday(PrInvs1tv_mroll$V1)
# PrInvs1tv_mroll$year <- year(PrInvs1tv_mroll$V1)
# PrInvs1tv_mroll$Temp <- as.numeric(PrInvs1tv_mroll$Temp)
# PrInvs1tv_mroll$Rain <- as.numeric(PrInvs1tv_mroll$Rain)
# 
# for(i in 1:nrow(PrInvs1tv_mroll)){
#   PrInvs1tv_mroll$ind[i] <- mean(PrInvs1tv_mroll$inventory[PrInvs1tv_mroll$week==PrInvs1tv_mroll$week[i]-1])
#   if(is.na(PrInvs1tv_mroll$ind[i])==TRUE){
#     PrInvs1tv_mroll$ind[i] <- mean(PrInvs1tv_mroll$inventory[PrInvs1tv_mroll$week==PrInvs1tv_mroll$week[i]-2])
#     if(is.na(PrInvs1tv_mroll$ind[i])==TRUE){
#       PrInvs1tv_mroll$ind[i] <- mean(PrInvs1tv_mroll$inventory[PrInvs1tv_mroll$week==PrInvs1tv_mroll$week[i]-3])
#       if(is.na(PrInvs1tv_mroll$ind[i])==TRUE){
#         PrInvs1tv_mroll$ind[i] <- mean(PrInvs1tv_mroll$inventory[PrInvs1tv_mroll$week==PrInvs1tv_mroll$week[i]-4])
#         if(is.na(PrInvs1tv_mroll$ind[i])==TRUE){
#           PrInvs1tv_mroll$ind[i] <- mean(PrInvs1tv_mroll$ind[PrInvs1tv_mroll$month==PrInvs1tv_mroll$month[i]-1])
#         }
#       }
#     }
#   }
#   if(is.nan(PrInvs1tv_mroll$ind[i])==TRUE){
#     PrInvs1tv_mroll$ind[i] <- mean(PrInvs1tv_mroll$inventory[PrInvs1tv_mroll$week==PrInvs1tv_mroll$week[i]])
#   }
# }
# 
# invTrain_s1tv_mroll <- PrInvs1tv_mroll[(PrInvs1tv_mroll$year=='2015')|(PrInvs1tv_mroll$year=='2016'&PrInvs1tv_mroll$month<4),]
# invTest_s1tv_mroll <- PrInvs1tv_mroll[PrInvs1tv_mroll$year=='2016'&PrInvs1tv_mroll$month>3,]
# #postroll
PrInvs1tv_postroll <- ssdata
PrInvs1tv_postroll <- right_join(PrInvs1tv_postroll,tw, by="V1")
PrInvs1tv_postroll <- left_join(PrInvs1tv_postroll,totalInvs1[totalInvs1$Ftype=="Postroll",c(1,3)], by="V1")
PrInvs1tv_postroll <- group_by(PrInvs1tv_postroll,V1,n, Temp, Rain)
PrInvs1tv_postroll <- summarize(PrInvs1tv_postroll,inventory=sum(inventory))
PrInvs1tv_postroll$mday <- mday(PrInvs1tv_postroll$V1)
PrInvs1tv_postroll$week <- week(PrInvs1tv_postroll$V1)
PrInvs1tv_postroll$month <- month(PrInvs1tv_postroll$V1)
PrInvs1tv_postroll$yday <- yday(PrInvs1tv_postroll$V1)
PrInvs1tv_postroll$year <- year(PrInvs1tv_postroll$V1)
PrInvs1tv_postroll$Temp <- as.numeric(PrInvs1tv_postroll$Temp)
PrInvs1tv_postroll$Rain <- as.numeric(PrInvs1tv_postroll$Rain)

for(i in 1:nrow(PrInvs1tv_postroll)){
  PrInvs1tv_postroll$ind[i] <- mean(PrInvs1tv_postroll$inventory[PrInvs1tv_postroll$week==PrInvs1tv_postroll$week[i]-1])
  if(is.na(PrInvs1tv_postroll$ind[i])==TRUE){
    PrInvs1tv_postroll$ind[i] <- mean(PrInvs1tv_postroll$inventory[PrInvs1tv_postroll$week==PrInvs1tv_postroll$week[i]-2])
    if(is.na(PrInvs1tv_postroll$ind[i])==TRUE){
      PrInvs1tv_postroll$ind[i] <- mean(PrInvs1tv_postroll$inventory[PrInvs1tv_postroll$week==PrInvs1tv_postroll$week[i]-3])
      if(is.na(PrInvs1tv_postroll$ind[i])==TRUE){
        PrInvs1tv_postroll$ind[i] <- mean(PrInvs1tv_postroll$inventory[PrInvs1tv_postroll$week==PrInvs1tv_postroll$week[i]-4])
        if(is.na(PrInvs1tv_postroll$ind[i])==TRUE){
          PrInvs1tv_postroll$ind[i] <- mean(PrInvs1tv_postroll$ind[PrInvs1tv_postroll$month==PrInvs1tv_postroll$month[i]-1])
        }
      }
    }
  }
  if(is.nan(PrInvs1tv_postroll$ind[i])==TRUE){
    PrInvs1tv_postroll$ind[i] <- mean(PrInvs1tv_postroll$inventory[PrInvs1tv_postroll$week==PrInvs1tv_postroll$week[i]])
  }
}
invTrain_s1tv_postroll <- PrInvs1tv_postroll[(PrInvs1tv_postroll$year=='2014')|(PrInvs1tv_postroll$year=='2015')|(PrInvs1tv_postroll$year=='2016'&PrInvs1tv_postroll$month<4),]
invTest_s1tv_postroll <- PrInvs1tv_postroll[PrInvs1tv_postroll$year=='2016'&PrInvs1tv_postroll$month>3,]

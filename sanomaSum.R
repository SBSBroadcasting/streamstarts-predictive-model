#read sanoma data 15-16 SS and summarize
library(lubridate)
library(dplyr)
sanomaSS <- rbind(read.csv2('sanomaSS150103.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE),
                   read.csv2('sanomaSS150406.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE),
                   read.csv2('sanomaSS150709.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE),
                   read.csv2('sanomaSS151012.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE),
                   read.csv2('sanomaSS160102.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE),
                   read.csv2('sanomaSS160304.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE),
                   read.csv2('sanomaSS1605.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)
)

sanomaSS$Day <- dmy(sanomaSS$Day)
sanomaSS$Stream.starts <- gsub("\\.","",sanomaSS$Stream.starts)
sanomaSS$Stream.starts <- as.numeric(sanomaSS$Stream.starts)

sumSanomaSS <- group_by(sanomaSS, Day)
sumSanomaSS <- summarize(sumSanomaSS, n=sum(Stream.starts))

colnames(sumSanomaSS) <- c('V1','n')
#In the future we do per brand totals/day#

####prediction#####
predSanoma <- rbind(sumSanomaSS, futureDates)
predSanoma <- right_join(predSanoma, tw, by='V1')
predSanoma$mday <- mday(predSanoma$V1)
predSanoma$week <- week(predSanoma$V1)
predSanoma$month <- month(predSanoma$V1)
predSanoma$yday <- yday(predSanoma$V1)
predSanoma$year <- year(predSanoma$V1)
predSanoma$Temp <- as.numeric(predSanoma$Temp)
predSanoma$Rain <- as.numeric(predSanoma$Rain)

trainSan <- predSanoma[(predSanoma$year=='2015')|(predSanoma$year=='2016'&predSanoma$month<4),]
testSan <- predSanoma[predSanoma$year=='2016'&predSanoma$month>3,]

#### create a linear model using the training partition ####
pmodelSan <- lm(n ~ mday+ week+ month+ yday+ Temp+ Rain,trainSan)
summary(pmodelSan)
# save the model to disk
save(pmodelSan, file=paste(SRC_PATH,'pmodelSan.rds', sep=","))
# load the model back from disk (prior variable name is restored)
load(paste(SRC_PATH,'pmodelSan.rds', sep=","))
# score the test data and plot pred vs. obs 
RESULT <- data.frame('Predicted'=predict(pmodelSan, testSan), 'Observed' = testSan$n)
#plot(RESULT)
# score the test data and append it as a new column (for later use)
testSan <- cbind(testSan,'p_n' = predict(pmodelSan, testSan))
testSan$Lift_lm <- testSan$p_n - testSan$n

#### RF ####
# library(randomForest)
# #train model
# prfmodelSan.rf <- randomForest(n ~ mday
#                               + month
#                               + week
#                               + yday
#                               + Temp
#                               + Rain
#                               , data=trainSan,
#                               importance=TRUE,
#                               ntree=1000)
# 
# prfmodelSan.pred <- predict(prfmodelSan.rf, testSan)
# 
# testSan$preds <- prfmodelSan.pred 
# testSan$Lift_rf <- testSan$preds - testSan$n
# 
# library(plotly)
# prfor <- plot_ly(testSan,x=testSan$V1, y=testSan$n, name="Actuals") %>% 
#   add_trace(testSan,x=testSan$V1, y=testSan$preds, name="predicted") #%>%
# add_trace(testSan,x=testSan$V1, y=testSan$Lift_rf, name="Lift_rf")
# prfor
# 
p <- plot_ly(testSan, x=testSan$V1, y=testSan$Lift_lm, name="Lift_lm") %>% 
  add_trace(p,x=testSan$V1, y=testSan$n, name="Actuals") %>% 
  add_trace(p,x=testSan$V1, y=testSan$p_n, name="predicted")
p

#### Predict future ####
library(lubridate)
library(dplyr)
library(randomForest)
library(plotly)

SRC_PATH <- 'W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-programmes/StreamStarts-predictive-model-programmes'
train <- subsetdata[subsetdata$Day <='2016-08-05',]
test <- futureTbl
train$Holiday <- as.numeric(as.factor(train$Holiday))
test$Holiday <- as.numeric(as.factor(test$Holiday))

#### create a linear model using the training partition ####
pmodel <- lm(Stream.starts ~ wday
              + mday
              + yday
              + week
              + month
              + year
              + Temp 
              + Rain
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
train$LiftLM <- train$p_n - train$Stream.starts
test <- cbind(test,'p_n' = predict(pmodel, test))
test$LiftLM <- test$p_n - test$Stream.starts

#### RF ####
#train model
prfmodel.rf <- randomForest(Stream.starts ~ wday
                             #+ mday
                             + yday
                             + week
                             + month
                             #+ year
                             + Holiday
                             + Temp 
                             + Rain
                             + Freq
                             + share
                             + NE
                            , data=train,
                            importance=TRUE,
                            ntree=1000)

prfmodel.pred <- predict(prfmodel.rf, train)
train$preds <- prfmodel.pred 
train$LiftRF <- train$preds - train$Stream.starts

prfmodel.pred <- predict(prfmodel.rf, test)
test$preds <- prfmodel.pred 
test$LiftRF <- test$preds - test$Stream.starts


#rbind all results
totalResults <- train[,c(1,2,15:18)]
totalResults <- rbind(totalResults, test[,c(1,2,15:18)])

#write date & preds in a csv
write.csv2(totalResults,paste0(programme, "_predictions_", Sys.Date(), ".csv"))


library(DAAG)
#### plot ####
p <- plot_ly(totalResults,x=totalResults$Day, y=totalResults$Stream.starts, name="Actuals") %>% 
  add_trace(x=totalResults$Day, y=totalResults$p_n, name="lm"
            ) %>% add_trace(x=totalResults$Day, y=totalResults$preds, name="rf"
            # ) %>% add_trace(x=totalResults$Day, y=totalResults$LiftLM, name="Lift LM"
            # ) %>% add_trace(x=totalResults$Day, y=totalResults$LiftRF, name="Lift RF"
            ) %>% layout(title=capstring(programme), xaxis=list(title='Day'), yaxis=list(title='Stream starts'))
p


r2_lm <- sqrt(mean((test$LiftLM)^2)) 
r2_rf <- sqrt(mean((test$LiftRF)^2)) 

#plot random forest
plot(prfmodel.rf, log="y")
varImpPlot(prfmodel.rf)


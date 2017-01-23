#### Predict future ####
library(lubridate)
library(dplyr)
library(randomForest)
library(plotly)

SRC_PATH <- 'W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-kopie-NEW/StreamStarts-predictive-model-kopie/'
train <- final_sanoma
test <- futureTbl
train$Holiday.name.x <- as.factor(train$Holiday.name.x)
test$Holiday.name.x <- as.factor(test$Holiday.name.x)
levels(test$Holiday.name.x) <- levels(train$Holiday.name.x)

#### create a linear model using the training partition ####
pmodel <- lm(n ~ wday
             + month
             + yday
             + mday
             + Holiday.name.x
             + Temp 
             + Rain 
             ,train)

summary(pmodel)
# save the model to disk
save(pmodel, file=paste(SRC_PATH,'pmodel.rds', sep=","))
# load the model back from disk (prior variable name is restored)
load(paste(SRC_PATH,'pmodel.rds', sep=","))
# score the test data and append it as a new column (for later use)
train$p_n <- predict(pmodel, train)
train$LiftLM <- train$p_n - train$n

test <- cbind(test,'p_n' = predict(pmodel, test))
test$LiftLM <- test$p_n - test$n

#### RF ####
#train model
prfmodel.rf <- randomForest(n ~ wday
                            + month
                            + yday
                            + mday
                            + Holiday.name.x
                            + Temp 
                            + Rain 
                            , data=train,
                            importance=TRUE,
                            ntree=1000)

prfmodel.pred <- predict(prfmodel.rf, train)
train$preds <- prfmodel.pred 
train$LiftRF <- train$preds - train$n

prfmodel.pred <- predict(prfmodel.rf, test)
test$preds <- prfmodel.pred 
test$LiftRF <- test$preds - test$n


#rbind all results
totalResults_SANOMA <- train[,c(1,4,10:13)]
totalResults_SANOMA <- rbind(totalResults_SANOMA, test[,c(1,10,37:40)])

#write date & preds in a csv
write.csv2(totalResults_SANOMA,"totalPredictions_SANOMA.csv")


#### plot ####
p <- plot_ly(totalResults_SANOMA,x=totalResults_SANOMA$Date, y=totalResults_SANOMA$n, name="Actuals") %>% 
  add_trace(x=totalResults_SANOMA$Date, y=totalResults_SANOMA$preds, name="RandomForest") %>%
  add_trace(x=totalResults_SANOMA$Date, y=totalResults_SANOMA$p_n, name="Linear Regression") %>% layout(title="SANOMA STREAMSTARTS PREDICTION",xaxis=list(title='Date'),yaxis=list(title='StreamStarts'))
p


r2_lm <- sqrt(mean((test$LiftLM)^2)) 
r2_rf <- sqrt(mean((test$LiftRF)^2)) 

#plot random forest
plot(prfmodel.rf, log="y")
varImpPlot(prfmodel.rf)


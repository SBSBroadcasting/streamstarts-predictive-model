#### Predict future ####

#### load the data from a CSV ####
SRC_PATH <- 'W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/'
futureR <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/olddates.csv", header = TRUE, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "", row.names=NULL)
#futureR$mday <- as.factor(futureR$mday)
#futureR$wday <- as.factor(futureR$wday)
#futureR$month <- as.factor(futureR$month)
#futureR$Holiday.name <- as.character(futureR$Holiday.name)

library(lubridate)
library(dplyr)

data_train_f <- futureR[,1:38]
data_train_f$Date <- as.character(data_train_f$Date)
data_train_f$Date <- dmy(data_train_f$Date)
data_train_f$Prog.Share <- as.character(data_train_f$Prog.Share)
data_train_f$Prog.Share <- gsub(",",".",data_train_f$Prog.Share)
data_train_f$Prog.Share <- as.numeric(data_train_f$Prog.Share)
#data_train_f$Holiday.type <- as.character(data_train_f$Holiday.type)

data_test_h <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/futuredates2.csv", header = TRUE, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "", row.names=NULL)
data_test_h$Date <- as.character(data_test_h$Date)
data_test_h$Date <- dmy(data_test_h$Date)
data_test_h$Prog.Share <- as.character(data_test_h$Prog.Share)
data_test_h$Prog.Share <- gsub(",",".",data_test_h$Prog.Share)
data_test_h$Prog.Share <- as.numeric(data_test_h$Prog.Share)
data_test_h$Holiday.type <- as.character(data_test_h$Holiday.type)

#data_test_h$mday <- as.factor(data_test_h$mday)
#data_test_h$wday <- as.factor(data_test_h$wday)
#data_test_h$month <- as.factor(data_test_h$month)
data_test_h$Holiday.type <- as.factor(data_test_h$Holiday.type)

avgprsTR <- mean(!is.na(data_train_f$Prog.Share))
for(i in 1:nrow(data_train_f)){
  if(is.na(data_train_f$Prog.Share[i])|is.infinite(data_train_f$Prog.Share[i])){
    data_train_f$Prog.Share[i] <- avgprsTR
  }
}
for(i in 1:nrow(data_test_h)){
  if(is.na(data_test_h$Prog.Share[i])|is.infinite(data_test_h$Prog.Share[i])){
    data_test_h$Prog.Share[i] <- avgprsTR
  }
}

#GIve all the levels
#levels(data_test_h$mday) <- levels(data_train_f$mday)
#levels(data_test_h$wday) <- levels(data_train_f$wday)
#levels(data_test_h$month) <- levels(data_train_f$month)
levels(data_test_h$Holiday.type) <- levels(data_train_f$Holiday.type)

#### create a linear model using the training partition ####
pmodel_f <- lm(n ~mday
             + month
             + wday
             #+ yday
             + Prog.Share
             + Holiday.type
             + Temp 
             + SDur 
             + S. 
             ,data_train_f)

summary(pmodel_f)

# save the model to disk
save(pmodel_f, file=paste(SRC_PATH,'pmodel_f.rds', sep=","))

# load the model back from disk (prior variable name is restored)
load(paste(SRC_PATH,'pmodel_f.rds', sep=","))

# score the test data and plot pred vs. obs 
RESULT <- data.frame('Predicted'=predict(pmodel_f, data_test_h), 'Observed' = data_test_h$n)
plot(RESULT)

# score the test data and append it as a new column (for later use)
new_data_future <- cbind(data_test_h,'p_n' = predict(pmodel_f, data_test_h))
new_data_future$Lift <- new_data_future$p_n - new_data_future$n
# score an individual row

#### RF ####

library(randomForest)
#train model
prfmodel_f.rf <- randomForest(n ~ wday
                            #+ month
                            #+ yday
                            #+ mday
                            + Prog.Share
                            + F_BTL_FILMS 
                            + F_OVERIGE_NON_FICTIE 
                            + F_Overig_amusement
                            + F_TAP 
                            + F_OM_Programa 
                            + F_OM_Live_registratie 
                            + F_SPEL_QUIZ 
                            + F_NLD_FILMS 
                            + F_ONF 
                            + F_NLD_SERIES 
                            + F_BTL_SERIES 
                            + F_Nld_series 
                            + F_Voetbalreportage 
                            + F_BTL_OM_overig 
                            + F_Satire 
                            + F_Weerbericht
                            + Voetball
                            + F_PM_Live_registratie 
                            + F_PM_Videoclips 
                            + F_Actuele_sportinformatie 
                            + F_Cabaret_kleinkunst 
                            + F_Overige_sportreportage 
                            + F_Overige_sportinformatie 
                            + F_Tekstuele_informatie 
                            + Holiday.type
                            + Temp 
                            + SDur 
                            + S. 
                            , data=data_train_f,
                            importance=TRUE,
                            ntree=1000)

prfmodel_f.pred <- predict(prfmodel_f.rf, data_test_h)


data_test_h$preds <- prfmodel_f.pred 
data_test_h$Lift <- data_test_h$preds - data_test_h$n

#write date & preds in a csv
write.csv2(data_test_h[,c(1,39)],"ssToInv.csv")

prfor <- plot_ly(data_test_h,x=data_test_h$Date, y=data_test_h$n, name="Actuals") %>% 
  add_trace(data_test_h,x=data_test_h$Date, y=data_test_h$preds, name="predicted") %>%
  add_trace(data_test_h,x=data_test_h$Date, y=data_test_h$Lift, name="Lift")
prfor


###comparison of lifts lm_rf####
lifts <- plot_ly(new_data_future,x=new_data_future$Date, y=new_data_future$Lift, name="Lift_lm") %>% 
  add_trace(data_test_h,x=data_test_h$Date, y=data_test_h$Lift, name="Lift_rf")
lifts

r2_lm <- sqrt(mean((new_data_future$Lift)^2)) 
r2_rf <- sqrt(mean((data_test_h$Lift)^2)) 


#### plot results ####
library(plotly)
# Plot using Plotly
# Fitted vs Residuals
# For scatter plot smoother

plt1 <- pmodel_f %>% 
  plot_ly(x = fitted.values, y = residuals, 
          type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
          marker = list(size = 10, opacity = 0.5), showlegend = F) 

# For scatter plot smoother

plt4 <- pmodel_f %>% 
  plot_ly(x = effects, y = residuals, 
          type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
          marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
  
  layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
plt1
plt4


#plot random forest
plot(prfmodel_f.rf, log="y")
varImpPlot(prfmodel_f.rf)

p <- plot_ly(new_data_future, x=new_data_future$Date, y=new_data_future$Lift, name="Lift") %>% 
  add_trace(p,x=new_data_future$Date, y=new_data_future$n, name="Actuals") %>% 
  add_trace(p,x=new_data_future$Date, y=new_data_future$p_n, name="predicted")
p

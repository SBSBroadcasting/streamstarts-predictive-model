#### load the data from a CSV ####
SRC_PATH <- 'W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/'
dataR <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/final_sbs6_2.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
dataR <- dataR[,-c(1)]
#merge dates
dataR$Date <- as.character(dataR$Date)
dataR$Date <- ymd(dataR$Date)

#future <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/futureDates.csv", header = TRUE, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
library(lubridate)
#future$mday <- mday(future$Date)
#future$wday <- wday(future$Date)
#future$yday <- yday(future$Date)
#future$month <- month(future$Date)

# split the data 80% train/20% test
sample_idx <- sample(1:nrow(dataR), nrow(dataR)*0.8)
data_train <- dataR[sample_idx, ]
data_test <- dataR[-sample_idx, ]

#### create a linear model using the training partition ####
pmodel <- lm(n ~  mday
             + month
             + yday
             + wday
             + Prog.Share
              #+ F_BTL_FILMS 
              # + F_OVERIGE_NON_FICTIE 
              # + F_Overig_amusement
              # + F_TAP 
              #+ F_KINDERFILMS 
              #+ F_Nieuws 
              #+ F_OM_Programa 
              #+ F_OM_Live_registratie 
              #+ F_SPEL_QUIZ 
              #+ F_NLD_FILMS 
              #+ F_ONF 
              #+ F_NLD_SERIES 
              #+ F_BTL_SERIES 
              #+ F_Nld_series 
              #+ F_Voetbalreportage 
              #+ F_BTL_OM_overig 
              #+ F_Satire 
              #+ F_Weerbericht 
              #+ F_PM_Live_registratie 
              #+ F_PM_Videoclips 
              #+ F_Actuele_sportinformatie 
              #+ F_Cabaret_kleinkunst 
              #+ F_Overige_sportreportage 
              #+ F_Overige_sportinformatie 
              #+ F_Tekstuele_informatie  
             #+ Holiday.name
             #+ Voetball
             + Holiday.type
             + Temp 
             + SDur 
             + S. 
             , data_train)
summary(pmodel)
# save the model to disk
saveRDS(pmodel.pred, file=paste(SRC_PATH,'pmodel.rds', sep=","))

# load the model back from disk (prior variable name is restored)
load(paste(SRC_PATH,'pmodel.rds', sep=","))

# score the test data and plot pred vs. obs 
RESULT <- data.frame('Predicted'=predict(pmodel, data_test), 'Observed' = data_test$n)
#plot(RESULT)

# score the test data and append it as a new column (for later use)
new_data <- cbind(data_test,'p_n' = predict(pmodel, data_test))
new_data$Lift <- new_data$p_n - new_data$n

pllm <- plot_ly(new_data,x=new_data$Date, y=new_data$n, name="Actuals") %>% 
  add_trace(new_data,x=new_data$Date, y=new_data$p_n, name="predicted") %>%
  add_trace(new_data,x=new_data$Date, y=new_data$Lift, name="Lift")
pllm
#F_BTL_FILMS 
#F_OVERIGE_NON_FICTIE 
#F_Overig_amusement
#F_TAP 
#F_KINDERFILMS 
#F_Nieuws 
#F_OM_Programa 
#F_OM_Live_registratie 
#F_SPEL_QUIZ 
#F_NLD_FILMS 
#F_ONF 
#F_NLD_SERIES 
#F_BTL_SERIES 
#F_Nld_series 
#F_Voetbalreportage 
#F_BTL_OM_overig 
#F_Satire 
#F_Weerbericht 
#F_PM_Live_registratie 
#F_PM_Videoclips 
#F_Actuele_sportinformatie 
#F_Cabaret_kleinkunst 
#F_Overige_sportreportage 
#F_Overige_sportinformatie 
#F_Tekstuele_informatie 

#### RF ####

library(randomForest)
#train model
prfmodel.rf <- randomForest(n ~ mday
                            + month
                            + yday
                            + wday
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
                          , data=data_train,
                          importance=TRUE,
                          ntree=1000)

prfmodel.pred <- predict(prfmodel.rf, data_test)
prfmodel.rf

data_test$preds <- prfmodel.pred 
data_test$Lift <- data_test$preds - data_test$n

prfor <- plot_ly(data_test,x=data_test$Date, y=data_test$n, name="Actuals") %>% 
  add_trace(data_test,x=data_test$Date, y=data_test$preds, name="predicted") %>%
  add_trace(data_test,x=data_test$Date, y=data_test$Lift, name="Lift")
prfor

###comparison of lifts lm_rf####
lifts <- plot_ly(new_data,x=new_data$Date, y=new_data$Lift, name="Lift_lm") %>% 
  add_trace(data_test,x=data_test$Date, y=data_test$Lift, name="Lift_rf")
lifts

r2_lm <- sqrt(mean((new_data$Lift)^2)) 
r2_rf <- sqrt(mean((data_test$Lift)^2)) 

#### plot results ####
library(plotly)
# Plot using Plotly
# Fitted vs Residuals
# For scatter plot smoother

plt1 <- pmodel %>% 
  plot_ly(x = fitted.values, y = residuals, 
          type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
          marker = list(size = 10, opacity = 0.5), showlegend = F) 

# For scatter plot smoother

plt4 <- pmodel %>% 
  plot_ly(x = effects, y = residuals, 
          type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
          marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
  
  layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
plt1
plt4

#plot random forest
plot(prfmodel.rf, log="y")
varImpPlot(prfmodel.rf)

p <- plot_ly(new_data, x=new_data$Date, y=new_data$Lift, name="Lift") %>% 
  add_trace(p,x=new_data$Date, y=new_data$n, name="Actuals") %>% 
  add_trace(p,x=new_data$Date, y=new_data$p_n, name="predicted")
p

#plot random forest 1
#plot(prfmodel1.rf, log="y")
#varImpPlot(prfmodel1.rf)

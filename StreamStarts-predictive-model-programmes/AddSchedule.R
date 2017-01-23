#Manual scheduling

MSsbs6a <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-programmes/StreamStarts-predictive-model-programmes/ManualSchedule/SBS60716.csv",header = FALSE, sep = ";", dec = ".", quote = "\"", stringsAsFactors = FALSE)
MSsbs6b <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-programmes/StreamStarts-predictive-model-programmes/ManualSchedule/SBS60816.csv",header = FALSE, sep = ";", dec = ".", quote = "\"", stringsAsFactors = FALSE)
MSsbs6c <- read.csv2("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-programmes/StreamStarts-predictive-model-programmes/ManualSchedule/SBS60916.csv",header = FALSE, sep = ";", dec = ".", quote = "\"", stringsAsFactors = FALSE)

MSsbs6a <- MSsbs6a[!(is.na(MSsbs6a$V1)==TRUE | MSsbs6a$V1==""),c(1,3,4)]
MSsbs6b <- MSsbs6b[!(is.na(MSsbs6b$V1)==TRUE | MSsbs6b$V1==""),c(1,3,4)]
MSsbs6c <- MSsbs6c[!(is.na(MSsbs6c$V1)==TRUE | MSsbs6c$V1==""),c(1,3,4)]

MSsbs6 <- rbind(MSsbs6a, MSsbs6b, MSsbs6c)
MSsbs6$V3 <- tolower(MSsbs6$V3)
MSsbs6 <- MSsbs6[!(grepl('blok',MSsbs6$V3)|grepl('verv.',MSsbs6$V4)|grepl('t.b.a',MSsbs6$V3)),c(1,2)]
MSsbs6 <- unique(MSsbs6)
MSsbs6$V1 <- dmy(MSsbs6$V1)
colnames(MSsbs6) <- c('Day','prog')
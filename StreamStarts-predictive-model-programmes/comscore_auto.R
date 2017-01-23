library(httr)
onlineSS_15 <- rbind(read.csv2("online_150103.csv", header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                     read.csv2("online_150406.csv", header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                     read.csv2("online_150709.csv", header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                     read.csv2("online_151012.csv", header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
               )
#onlineSS_15 <- onlineSS_15[,-c(1)]

#read 2016 historical data
onlineSS_16 <- rbind(read.csv2("sbs_kijk+kijk-app+kijk-embed+6-more-site(s)_20160101-20160131_12391.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                     read.csv2("sbs_kijk+kijk-app+kijk-embed+6-more-site(s)_20160201-20160430_12391.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                     read.csv2("sbs_kijk+kijk-app+kijk-embed+6-more-site(s)_20160501-20160828_12391.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")                
                )
#get online ss comscore data
date <- Sys.Date()-7
filename <- GET("https://dax-rest.comscore.eu/v1/datadownloadreportitem.csv?itemid=12391&startdate=20160501&enddate=20160828&site=sbs6|kijk|kijk-embed|kijk-app|net5|sbs9|veronicatv|supersite-kijk|supersite&client=sbs&user=mgilissen&password=xem46ULjh-z5g8Sp")
#filename <- GET(paste0("https://dax-rest.comscore.eu/v1/datadownloadreportitem.csv?itemid=12391&startdate=", gsub("-","",date),"&enddate=today&site=sbs6|utopia-app|kijk|kijk-embed|kijk-app|net5|sbs9|veronicatv&client=sbs&user=mgilissen&password=xem46ULjh-z5g8Sp"))
filename <- content(filename, 'text')
filename <- gsub(" ","",filename)
filename <- gsub("url","",filename)
filename <- gsub("\n","",filename)
filename <- gsub("[|]","",filename)
file <- filename

#credits for sbs
#GET("https://dax-rest.comscore.eu/v1/credits.pretty_xml?client=sbs&user=mgilissen&password=xem46ULjh-z5g8Sp")

#### Unzip *.gz files ####
library('R.utils')
for(i in 1:length(file)){
  filename <- file[[i]]
  fname <- strsplit(strsplit(filename, "\\.")[[1]][3],"/")[[1]][[7]]
  temp <- paste("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-programmes/StreamStarts-predictive-model-programmes/",fname,".tsv.gz",sep="") 
  download.file(filename, destfile = temp)
  unzipped <- gunzip(temp)
}

library(lubridate)
onlineSS_16 <- rbind(onlineSS_16,read.delim(unzipped, header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""))

dataset <- rbind(onlineSS_15, onlineSS_16)
dataset$V1 <- as.character(dataset$V1)
dataset$V2 <- as.character(dataset$V2)
dataset$V3 <- as.character(dataset$V3)
dataset$V4 <- as.character(dataset$V4)
dataset$V1 <- dmy(dataset$V1)
dataset$V4 <- gsub("\\,","",dataset$V4)
dataset$V4 <- as.numeric(dataset$V4)
dataset <- dataset[!(dataset$V2=='Advertentie'|dataset$V2=="nunl"|grepl("linda",dataset$V2)),]
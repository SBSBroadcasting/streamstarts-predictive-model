library(httr)
onlineSS_15 <- read.csv2("onlineSs_15.csv", header = TRUE, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
onlineSS_15 <- onlineSS_15[,-c(1)]
#read 2015 historical data
# onlineSS_15 <- rbind(read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150101-20150131_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150201-20150228_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150301-20150331_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150401-20150430_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150501-20150531_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150601-20150630_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150701-20150731_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150801-20150831_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20150901-20150930_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20151001-20151031_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20151101-20151130_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
#                       read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20151201-20151231_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
#                       )
# write.csv2(onlineSS_15, "onlineSS_15.csv")
#read 2016 historical data
onlineSS_16 <- rbind(read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160101-20160131_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                      read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160201-20160229_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                      read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160301-20160331_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                      read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160401-20160430_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                      read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160501-20160531_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                      read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160601-20160630_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""),
                      read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160701-20160730_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
                      ,read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20160801-20160817_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
                      # ,read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20161001-20161031_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
                      # ,read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20161101-20161130_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
                      # read.delim("sbs_kijk+kijk-app+kijk-embed+5-more-site(s)_20161201-20161231_11944.tsv", header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
                      )
#get online ss comscore data
date <- Sys.Date()-7
filename <- GET("https://dax-rest.comscore.eu/v1/datadownloadreportitem.csv?itemid=11944&startdate=20160801&enddate=20160817&site=sbs6|utopia-app|kijk|kijk-embed|kijk-app|net5|sbs9|veronicatv&client=sbs&user=mgilissen&password=xem46ULjh-z5g8Sp")
#filename <- GET(paste0("https://dax-rest.comscore.eu/v1/datadownloadreportitem.csv?itemid=11944&startdate=", gsub("-","",date),"&enddate=today&site=sbs6|utopia-app|kijk|kijk-embed|kijk-app|net5|sbs9|veronicatv&client=sbs&user=mgilissen&password=xem46ULjh-z5g8Sp"))
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
  temp <- paste("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/StreamStarts-predictive-model-kopie-NEW/StreamStarts-predictive-model-kopie/",fname,".tsv.gz",sep="") 
  download.file(filename, destfile = temp)
  unzipped <- gunzip(temp)
}

onlineSS_16 <- rbind(onlineSS_16,read.delim(unzipped, header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = ""))

OnlineTotal <- rbind(onlineSS_15, onlineSS_16)
OnlineTotal$V1 <- as.character(OnlineTotal$V1)
OnlineTotal$V2 <- as.character(OnlineTotal$V2)
OnlineTotal$V3 <- as.character(OnlineTotal$V3)
OnlineTotal$V4 <- as.character(OnlineTotal$V4)
OnlineTotal$V5 <- as.character(OnlineTotal$V5)
library(lubridate)
OnlineTotal$V1 <- dmy(OnlineTotal$V1)
OnlineTotal$V5 <- gsub("\\,0","",OnlineTotal$V5)
OnlineTotal$V5 <- as.numeric(OnlineTotal$V5)
OnlineTotal <- OnlineTotal[!(OnlineTotal$V4=='Advertentie'|OnlineTotal$V2=='utopia-app'|OnlineTotal$V3=="nunl"|grepl("linda",OnlineTotal$V3)),]
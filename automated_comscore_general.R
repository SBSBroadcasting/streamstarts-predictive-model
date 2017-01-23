#download daily comscore data for prediction
####Get data from Comscore API####
library('httr')
date <- Sys.Date()
#sbs6 data
filename_sbs6 <- GET( paste0( paste0("https://dax-rest.comscore.eu/v1/datadownloadreportitem.csv?itemid=12065&startdate=", gsub("-","",date)),
                              "&enddate=today&site=sbs6|utopia-app|kijk|kijk-embed|kijk-app|net5|sbs9|veronicatv&client=sbs&user=mgilissen&password=xem46ULjh-z5g8Sp"
                            )
                    )
filename_sbs6 <- content(filename_sbs6, 'text')
filename_sbs6 <- gsub(" ","",filename_sbs6)
filename_sbs6 <- gsub("url","",filename_sbs6)
filename_sbs6 <- gsub("\n","",filename_sbs6)
filename_sbs6 <- gsub("[|]","",filename_sbs6)
file <- filename_sbs6
#credits for sbs
#GET("https://dax-rest.comscore.eu/v1/credits.pretty_xml?client=sbs&user=mgilissen&password=xem46ULjh-z5g8Sp")

#### Unzip *.gz files ####
library('R.utils')
for(i in 1:length(file)){
  filename <- file[[i]]
  fname <- strsplit(strsplit(filename, "\\.")[[1]][3],"/")[[1]][[7]]
  temp <- paste( 
    paste("W:/R scripts/R_scripts_Konstantina/streamstarts_prediction/datasets_comscore/",fname,sep = ""), 
    ".tsv.gz", 
    sep=""
  ) 
  download.file(filename, destfile = temp)
  unzipped <- gunzip(temp)
}





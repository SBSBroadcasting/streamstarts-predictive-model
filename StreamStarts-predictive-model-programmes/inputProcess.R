##### rEAD THE INPUT // get info from Optiondf #####
inputData <- load('exampletable')

###### MANIPULATE THE INFO ######
Optiondf$Program.y <- tolower(Optiondf$Program.y)
programmeList <-  Optiondf$Program.y

library(dplyr)

programmeList <- as.list(programmeList)
for(i in 1:length(programmeList)){
  if(grepl(' \\+ ',programmeList[[i]])){
    plusPos[[i]] <- i 
    programmeList[[i]] <-  strsplit(programmeList[[i]], ' \\+ ', fixed=FALSE)[[1]][1]
    programmeList[[length(programmeList)+1]] <-  strsplit(programmeList[[i]], ' \\+ ', fixed=FALSE)[[1]][2]
    if(length(strsplit(programmeList[[i]][1], ' \\+ ', fixed=FALSE)[[1]])==3){
      programmeList[[length(programmeList)+1]] <-  strsplit(programmeList[[i]], ' \\+ ', fixed=FALSE)[[1]][3]
    }
  }else if(grepl(' / ',programmeList[[i]])){
    slashPos[[i]] <- i 
    programmeList[[i]] <-  strsplit(programmeList[[i]], ' [/] ', fixed=FALSE)[[1]][1]
    programmeList[[length(programmeList)+1]] <-  strsplit(programmeList[[i]], ' [/] ', fixed=FALSE)[[1]][2]
    if(length(strsplit(programmeList[[i]][1], ' [/] ', fixed=FALSE)[[1]])==3){
      programmeList[[length(programmeList)+1]] <-  strsplit(programmeList[[i]], ' [/] ', fixed=FALSE)[[1]][3]
    }
  }else if(grepl('NCIS',programmeList[[i]])){ #temp case for now
    programmeList[[i]] <-  strtrim(programmeList[[i]], nchar(programmeList[[i]])-1)
  }
}

programmeList <- unlist(unique(programmeList[!is.na(programmeList)]))
plusPos <- unique(plusPos[!is.na(plusPos)])
slashPos <- unique(slashPos[!is.na(slashPos)])


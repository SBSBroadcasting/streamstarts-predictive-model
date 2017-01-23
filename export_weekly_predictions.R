#make weekly predictions
#sbs6
w_totalResults_SBS6 <- totalResults_SBS6
w_totalResults_SBS6$week <- week(w_totalResults_SBS6$Date)
w_totalResults_SBS6$year <- year(w_totalResults_SBS6$Date)
w_totalResults_SBS6 <- w_totalResults_SBS6[,c(1,2,5,7,8)]
w_totalResults_SBS6 <- w_totalResults_SBS6[,-c(1)]
w_totalResults_SBS6 <- group_by(w_totalResults_SBS6,week,year)
w_totalResults_SBS6 <- summarize(w_totalResults_SBS6,n=sum(n),preds=sum(preds))

#NET5
w_totalResults_NET5 <- totalResults_NET5
w_totalResults_NET5$week <- week(w_totalResults_NET5$Date)
w_totalResults_NET5$year <- year(w_totalResults_NET5$Date)
w_totalResults_NET5 <- w_totalResults_NET5[,c(1,2,5,7,8)]
w_totalResults_NET5 <- w_totalResults_NET5[,-c(1)]
w_totalResults_NET5 <- group_by(w_totalResults_NET5,week,year)
w_totalResults_NET5 <- summarize(w_totalResults_NET5,n=sum(n),preds=sum(preds))

#SBS9
w_totalResults_SBS9 <- totalResults_SBS9
w_totalResults_SBS9$week <- week(w_totalResults_SBS9$Date)
w_totalResults_SBS9$year <- year(w_totalResults_SBS9$Date)
w_totalResults_SBS9 <- w_totalResults_SBS9[,c(1,2,5,7,8)]
w_totalResults_SBS9 <- w_totalResults_SBS9[,-c(1)]
w_totalResults_SBS9 <- group_by(w_totalResults_SBS9,week,year)
w_totalResults_SBS9 <- summarize(w_totalResults_SBS9,n=sum(n),preds=sum(preds))

#VERONICATV
w_totalResults_VERONICATV <- totalResults_VERONICATV
w_totalResults_VERONICATV$week <- week(w_totalResults_VERONICATV$Date)
w_totalResults_VERONICATV$year <- year(w_totalResults_VERONICATV$Date)
w_totalResults_VERONICATV <- w_totalResults_VERONICATV[,c(1,2,5,7,8)]
w_totalResults_VERONICATV <- w_totalResults_VERONICATV[,-c(1)]
w_totalResults_VERONICATV <- group_by(w_totalResults_VERONICATV,week,year)
w_totalResults_VERONICATV <- summarize(w_totalResults_VERONICATV,n=sum(n),preds=sum(preds))

#S1TV
w_totalResults_S1TV <- totalResults_S1TV
w_totalResults_S1TV$week <- week(w_totalResults_S1TV$Date)
w_totalResults_S1TV$year <- year(w_totalResults_S1TV$Date)
w_totalResults_S1TV <- w_totalResults_S1TV[,c(1,2,5,7,8)]
w_totalResults_S1TV <- w_totalResults_S1TV[,-c(1)]
w_totalResults_S1TV <- group_by(w_totalResults_S1TV,week,year)
w_totalResults_S1TV <- summarize(w_totalResults_S1TV,n=sum(n),preds=sum(preds))

#SANOMA
w_totalResults_SANOMA <- totalResults_SANOMA
w_totalResults_SANOMA$week <- week(w_totalResults_SANOMA$Date)
w_totalResults_SANOMA$year <- year(w_totalResults_SANOMA$Date)
w_totalResults_SANOMA <- w_totalResults_SANOMA[,c(1,2,5,7,8)]
w_totalResults_SANOMA <- w_totalResults_SANOMA[,-c(1)]
w_totalResults_SANOMA <- group_by(w_totalResults_SANOMA,week,year)
w_totalResults_SANOMA <- summarize(w_totalResults_SANOMA,n=sum(n),preds=sum(preds))

#KIJK
w_totalResults_KIJK <- totalResults_KIJK
w_totalResults_KIJK$week <- week(w_totalResults_KIJK$Date)
w_totalResults_KIJK$year <- year(w_totalResults_KIJK$Date)
w_totalResults_KIJK <- w_totalResults_KIJK[,c(1,2,5,7,8)]
w_totalResults_KIJK <- w_totalResults_KIJK[,-c(1)]
w_totalResults_KIJK <- group_by(w_totalResults_KIJK,week,year)
w_totalResults_KIJK <- summarize(w_totalResults_KIJK,n=sum(n),preds=sum(preds))

#SAVE ON DRIVE WEEKLY PREDS
write.csv2(w_totalResults_KIJK,'w_totalResults_KIJK.csv')
write.csv2(w_totalResults_NET5,'w_totalResults_NET5.csv')
write.csv2(w_totalResults_SBS6,'w_totalResults_SBS6.csv')
write.csv2(w_totalResults_S1TV,'w_totalResults_S1TV.csv')
write.csv2(w_totalResults_SBS9,'w_totalResults_SBS9.csv')
write.csv2(w_totalResults_VERONICATV,'w_totalResults_VERONICATV.csv')
write.csv2(w_totalResults_SANOMA,'w_totalResults_SANOMA.csv')

#setwd("C:/Users/Won/Dropbox/Won/Data_Food intake pattern/duration_data/rawdata")
library(tidyverse)
library(purrr)

#1. duration data reference ===============================================
df<-readRDS("durationdata.RDS")
str(df)

df1x<-readRDS("durationdata_recodingdata.RDS")
str(df1x)

df2x<-readRDS("durationdata_recodingdata_2.RDS")
str(df2x)

df3x<-readRDS("durationdata_recodingdata_3.RDS")
str(df3x)

#easy replacement
df$c36.9<-df1x$c36.9
df$c46.20<-df2x$c46.20

#halfway fuckup
df$c83.1[14:40,]<-df1x$c83.1[14:40,]
df$c83.1<-df$c83.1[1:40,] #I can't do NULL???

#double fuckup
df2x$c69.1<-df2x$c69.1[1:35,]
df2x$c69.1[36:44,]<-df3x$c69.1[34:42,]
df$c69.1<-df2x$c69.1

print(df$c36.9)
print(df$c46.20)
print(df$c69.1)
print(df$c83.1)
#all good so far



#2. Duration data from Observer=================================================
#cleanup first
temp = list.files(pattern="*.csv")
files = lapply(temp, read.csv, stringsAsFactors=F) #N=15 because cohort 37_1 is already in duration!
str(files)


dl1<-lapply(files,function(x) x %>% filter(Event_Type=="State start") %>% select(Behavior,Side,Duration_sf))


#now I want to find any rows have the same Hour/Minute information
#dl2<-lapply(dl1,function(x) x%>% mutate(Hour=substr(x$Time,1,2),
#                                        Min=substr(x$Time,4,5)))
#saveRDS(dl2,"inprogress.RDS")
#notuniq<-lapply(dl2,function(x) x%>% select(Behavior,Side,Hour,Min))
#lapply(notuniq,function(x) x[duplicated(x),])#doens't give me that good info  


#3. Merge!=============================================================
str(df)
str(dl1)
df$c36.9[,5:7]<-dl1[[1]]
df$c37.6[,5:7]<-dl1[[2]]
df$c38.17[,5:7]<-dl1[[3]]#
df$c39.22[,5:7]<-dl1[[4]]
df$c44.8[,5:7]<-dl1[[5]]#
df$c45.14[,5:7]<-dl1[[6]]
df$c46.20[,5:7]<-dl1[[7]]
df$c69.1[,5:7]<-dl1[[8]]#
df$c80.1[,5:7]<-dl1[[9]]
df$c81.7[,5:7]<-dl1[[10]]
df$c83.1[,5:7]<-dl1[[11]]#
df$c83.12[,5:7]<-dl1[[12]]
df$c85.8[,5:7]<-dl1[[13]]
df$c86.1[,5:7]<-dl1[[14]]
df$c86.7[,5:7]<-dl1[[15]]

df->safety

df$c36.9$day<-9
df$c37.6$day<-6
df$c38.17$day<-17
df$c39.22$day<-22
df$c44.8$day<-8
df$c45.14$day<-14
df$c46.20$day<-20
df$c69.1$day<-1
df$c80.1$day<-1
df$c81.7$day<-7
df$c83.1$day<-1
df$c83.12$day<-12
df$c85.8$day<-8
df$c86.1$day<-1
df$c86.7$day<-7



df$c36.9$cohort<-36
df$c37.6$cohort<-37
df$c38.17$cohort<-38
df$c39.22$cohort<-39
df$c44.8$cohort<-44
df$c45.14$cohort<-45
df$c46.20$cohort<-46
df$c69.1$cohort<-69
df$c80.1$cohort<-80
df$c81.7$cohort<-81
df$c83.1$cohort<-83
df$c83.12$cohort<-83
df$c85.8$cohort<-85
df$c86.1$cohort<-86
df$c86.7$cohort<-86

almost<-list() #if I were starter my hands would work less uhhh

almost[1]<-df[1]

almost[2]<-df[3]
almost[3]<-df[4]
almost[4]<-df[5]
almost[5]<-df[6]
almost[6]<-df[7]
almost[7]<-df[8]
almost[8]<-df[9]
almost[9]<-df[10]
almost[10]<-df[11]
almost[11]<-df[12]
almost[12]<-df[13]
almost[13]<-df[14]
almost[14]<-df[15]
almost[15]<-df[16]



final<-lapply(almost,function(x) x%>%select(cohort,day,id,side,behav,times,Duration_sf))

#saveRDS(final,"final_duration.RDS")

str(final)

onedata<-bind_rows(final) %>% as.data.frame()
write.csv(onedata,"final_duaration.csv")

##setwd("C:/Users/Won/Dropbox/Won/Data_Food intake pattern/cleandata")

#### FUNCTIONS

library(tidyverse)
library(data.table)

getdf <- function(l,r){
  
  # check there is a first column called X.
  # if first column is "X" (comes from saving csv and not including rownames=F)
  
  times<-list(
    left_on = l[which(l$Light=="on"),"realtime"],
    right_on = r[which(r$Light=="on"),"realtime"],
    left_off = l[which(l$Light=="off"),"realtime"],
    right_off = r[which(r$Light=="off"),"realtime"]
  )
  
  if(colnames(l)[1]=="X"){
    df1<-l[,2:14] 
    df2<-r[,2:14]
    df3<-df1%>%gather(subject,behavior,2:13)
    df4<-df2%>%gather(subject,behavior,2:13)
  }
  else
  {
    df1<-l[,1:13] 
    
    df2<-r[,1:13]
    df3<-df1%>%gather(subject,behavior,1:12)
    df4<-df2%>%gather(subject,behavior,1:12)
  }
  
  
  df3<-cbind(side="left",df3)
  df4<-cbind(side="right",df4)
  df5<-rbind(df3,df4)%>%arrange(realtime)%>%mutate(endtime=realtime+1)
  df5<-as.data.frame(df5)
  df<-sapply(df5,as.character)
  df[is.na(df)] <- " "
  df<-as.data.frame(df) #cbind sucks
  df$eating <- ifelse(grepl("e", df$behavior), 1, 0) 
  df$drinking <- ifelse(grepl("d", df$behavior), 1, 0) 
  df <- df %>% group_by(subject,realtime, endtime) %>% summarise(eating=sum(eating), drinking=sum(drinking))
  df<-as.data.frame(df) #
  df$realtime <- as.numeric(as.character(df$realtime))
  df$endtime <- as.numeric(as.character(df$endtime))
  df <- df[order(df$realtime),]
  return(list(df,times))
}





gettimes <- function(myseed=211,alphaid="M8",totaldrink =20,totaleat =20,propalpha =0.5,leftdf = l36.9,rightdf = r36.9) {
  
  ### set up dataframes
  bothdf <- getdf(leftdf,rightdf)[[1]]
  bothdf_D <- bothdf[bothdf$drinking>=1,]
  bothdf_E <- bothdf[bothdf$eating>=1,]
  bothdf_D_alpha <- bothdf_D[bothdf_D$subject==alphaid,]
  bothdf_D_others <- bothdf_D[bothdf_D$subject!=alphaid,]
  bothdf_E_alpha <- bothdf_E[bothdf_E$subject==alphaid,]
  bothdf_E_others <- bothdf_E[bothdf_E$subject!=alphaid,]
  
  
  
  #### get total drink/eat for alpha vs others
  
  alphadrink <- floor(totaldrink * propalpha)
  othersdrink <- totaldrink - alphadrink 
  
  alphaeat <- floor(totaleat * propalpha)
  otherseat <- totaleat - alphaeat 
  
  
  
  
  #### drinking
  set.seed(myseed)
  alpharows <- bothdf_D_alpha[sample(nrow(bothdf_D_alpha), alphadrink), ]
  
  alphaleft <- leftdf[leftdf$realtime %in% alpharows$realtime, ]
  alphaleft.times <- alphaleft$realtime[ (grepl("d",alphaleft[,alphaid])==T) ]
  alphaleft.times <- c(NA, alphaleft.times)
  
  alpharight <- rightdf[rightdf$realtime %in% alpharows$realtime, ]
  alpharight.times <- alpharight$realtime[ (grepl("d",alpharight[,alphaid])==T) ]
  alpharight.times <- c(NA, alpharight.times)
  
  alphadf<-rbind(
    data.frame(id=alphaid,side="left", times=alphaleft.times),
    data.frame(id=alphaid,side="right", times=alpharight.times)
  )
  
  
  
  
  ### others
  set.seed(myseed)
  othersrows <- bothdf_D_others[sample(nrow(bothdf_D_others), othersdrink), ]
  
  othersrows.list <- Filter(function(x) dim(x)[1] > 0, split(othersrows, othersrows$subject) )
  
  otherdf<-NULL
  for(i in 1:length(othersrows.list)){
    
    xdf<-  othersrows.list[[i]]
    subjectid <- xdf$subject %>% unique %>% as.character()
    
    otherleft <- leftdf[leftdf$realtime %in% xdf$realtime, ]
    otherleft.times <- otherleft$realtime[ (grepl("d",otherleft[,subjectid])==T) ]
    otherleft.times <- c(NA, otherleft.times)
    
    otherright <- rightdf[rightdf$realtime %in% xdf$realtime, ]
    otherright.times <- otherright$realtime[ (grepl("d",otherright[,subjectid])==T) ]
    otherright.times <- c(NA, otherright.times)
    
    otherdf[[i]]<-rbind(
      data.frame(id=subjectid,side="left", times=otherleft.times),
      data.frame(id=subjectid,side="right", times=otherright.times)
    )
    
    
  }
  
  timesdf <- rbind(alphadf,data.table::rbindlist(otherdf))
  timesdf <- timesdf[!is.na(timesdf$times),]
  timesdf$behav <- "Drink"
  
  
  
  
  #### EATING
  
  set.seed(myseed)
  alpharows_E <- bothdf_E_alpha[sample(nrow(bothdf_E_alpha), alphaeat), ]
  alpharows_E
  alphaleft_E <- leftdf[leftdf$realtime %in% alpharows_E$realtime, ]
  alphaleft_E.times <- alphaleft_E$realtime[ (grepl("e",alphaleft_E[,alphaid])==T) ]
  alphaleft_E.times <- c(NA, alphaleft_E.times)
  
  alpharight_E <- rightdf[rightdf$realtime %in% alpharows_E$realtime, ]
  alpharight_E.times <- alpharight_E$realtime[ (grepl("e",alpharight_E[,alphaid])==T) ]
  alpharight_E.times <- c(NA, alpharight_E.times)
  
  alphadf_E<-rbind(
    data.frame(id=alphaid,side="left", times=alphaleft_E.times),
    data.frame(id=alphaid,side="right", times=alpharight_E.times)
  )
  
  
  
  
  ### others
  set.seed(myseed)
  othersrows_E <- bothdf_E_others[sample(nrow(bothdf_E_others), otherseat), ]
  othersrows_E.list <- Filter(function(x) dim(x)[1] > 0, split(othersrows_E, othersrows_E$subject) )
  
  otherdf_E<-NULL
  for(i in 1:length(othersrows_E.list)){
    
    xdf_E<-  othersrows_E.list[[i]]
    subjectid_E <- xdf_E$subject %>% unique %>% as.character()
    
    otherleft_E <- leftdf[leftdf$realtime %in% xdf_E$realtime, ]
    otherleft_E.times <- otherleft_E$realtime[ (grepl("e",otherleft_E[,subjectid_E])==T) ]
    otherleft_E.times <- c(NA, otherleft_E.times)
    
    otherright_E <- rightdf[rightdf$realtime %in% xdf_E$realtime, ]
    otherright_E.times <- otherright_E$realtime[ (grepl("e",otherright_E[,subjectid_E])==T) ]
    otherright_E.times <- c(NA, otherright_E.times)
    
    otherdf_E[[i]]<-rbind(
      data.frame(id=subjectid_E,side="left", times=otherleft_E.times),
      data.frame(id=subjectid_E,side="right", times=otherright_E.times)
    )
    
    
  }
  
  timesdf_E <- rbind(alphadf_E,data.table::rbindlist(otherdf_E))
  timesdf_E <- timesdf_E[!is.na(timesdf_E$times),]
  timesdf_E$behav <- "Eat"
  
  
  timesdf_all <- rbind(timesdf,timesdf_E) %>% arrange(times)
  
  
  return(timesdf_all)
}



#alpha information
alpha<-read.csv("glicko_ontheday.csv")%>%filter(grank==1)%>%select(cohort,mouse,ontheday,grank)
#alpha
#     cohort mouse ontheday grank
#1      36     8        9     1
#2      37     2        1     1
#3      37     2        6     1
#4      38     1       17     1
#5      39    12       22     1
#6      44     8        8     1
#7      45     3       14     1
#8      46    10       20     1
#9      69    10        1     1
#10     80     5        1     1
#11     81     3        6     1
#12     83     1        1     1
#13     83     1       12     1
#14     85     1        8     1
#15     86     2        1     1
#16     86    11        7     1


#########now let's get the times ----
tomjerry<-list()

#1.Cohort36_Day9=================================================================
l36.9 <- read.csv("c36_left_082515.csv", stringsAsFactors=FALSE)
r36.9 <- read.csv("c36_right_082515.csv", stringsAsFactors=FALSE)
c36<-gettimes(myseed=928,alphaid="M8",totaldrink =20,totaleat =20,propalpha =0.5,
              leftdf = l36.9,
              rightdf = r36.9)
arrange(c36,side)
tomjerry$c36.9<-arrange(c36,side)


#2.Cohort 37_Day1 -->already in duration -yas------!=============================
tomjerry$c37.1<-"Already in duration"


#3.Cohort 37_Day6================================================================ 
l37.6 <- read.csv("c37_left_082215.csv", stringsAsFactors=FALSE)
r37.6 <- read.csv("c37_right_082215.csv", stringsAsFactors=FALSE)
c37<-gettimes(myseed=928,alphaid="M2",totaldrink =20,totaleat =20,propalpha =0.5,
         leftdf = l37.6,
         rightdf = r37.6)
arrange(c37,side)
tomjerry$c37.6<-arrange(c37,side)


#4.Cohort 38_Day17================================================================ 
l38.17 <- read.csv("c38_left_090215.csv", stringsAsFactors=FALSE)
r38.17 <- read.csv("c38_right_090215.csv", stringsAsFactors=FALSE)
c38<-gettimes(myseed=928,alphaid="M1",totaldrink =20,totaleat =20,propalpha =0.5,
              leftdf = l38.17,
              rightdf = r38.17)%>%arrange(side)

tomjerry$c38.17<-c38


#5.Cohort 39_Day22================================================================ 
leftcurrent<- read.csv("c39_left_090715.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c39_right_090715.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=928,alphaid="M12",totaldrink =20,totaleat =20,propalpha =0.5,
              leftdf = leftcurrent,
              rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c39.22<-both


#6.Cohort 44_Day8================================================================ 
leftcurrent<- read.csv("c44_left_101315.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c44_right_101315.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=489,alphaid="M8",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c44.8<-both


#7.Cohort 45_Day14================================================================ 
leftcurrent<- read.csv("c45_left_101915.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c45_right_101915.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=211,alphaid="M3",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c45.14<-both


#8.Cohort 46_Day20================================================================ 
leftcurrent<- read.csv("c46_left_102515.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c46_right_102515.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=134,alphaid="M10",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c46.20<-both


#9.Cohort 69_Day1================================================================ 
leftcurrent<- read.csv("c69_left_042416.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c69_right_042416.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=764,alphaid="M10",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c69.1<-both


#10.Cohort 80_Day1================================================================ 
leftcurrent<- read.csv("c80_left_071316.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c80_right_071316.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=928,alphaid="M5",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c80.1<-both


#11.Cohort 81_Day7================================================================ 
leftcurrent<- read.csv("c81_left_071916.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c81_right_071916.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=928,alphaid="M3",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c81.7<-both


#12.Cohort 83_Day1================================================================ 
leftcurrent<- read.csv("c83_left_102416.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c83_right_102416.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=111,alphaid="M1",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c83.1<-both


#13.Cohort 83_Day12================================================================ 
leftcurrent<- read.csv("c83_left_110416.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c83_right_110416.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=928,alphaid="M1",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c83.12<-both


#14.Cohort 85_Day8================================================================ 
leftcurrent<- read.csv("c85_left_120616.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c85_right_120616.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=928,alphaid="M1",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c85.8<-both


#15.Cohort 86_Day1================================================================ 
leftcurrent<- read.csv("c86_left_112916.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c86_right_112916.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=928,alphaid="M2",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c86.1<-both


#16.Cohort 86_Day7================================================================ 
leftcurrent<- read.csv("c86_left_120516.csv", stringsAsFactors=FALSE)
rightcurrent<- read.csv("c86_right_120516.csv", stringsAsFactors=FALSE)
both<-gettimes(myseed=928,alphaid="M11",totaldrink =20,totaleat =20,propalpha =0.5,
               leftdf = leftcurrent,
               rightdf = rightcurrent)%>%arrange(side)
both
tomjerry$c86.7<-both



#########sanity check==================================================================
print(tomjerry)

##saveRDS(tomjerry,"durationdata.RDS")
##saveRDS(tomjerry,"durationdata_recodingdata.RDS") #chrt36, 83
##saveRDS(tomjerry,"durationdata_recodingdata_2.RDS") #chrt 46, 69
##saveRDS(tomjerry,"durationdata_recodingdata_3.RDS") #chrt 69 (this is the real deal)
#test<-readRDS("durationdata.RDS")
#test[[1]] #okay it saves. good. 
#print(test)

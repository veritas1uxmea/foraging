#most of the codes are copy/paste of "get_ranks_all.R".

#try to run the code by chunk.... is it just me it works only when run as chunk not whole......-_-;;;;
#when see the problem, look at the data str first. 
library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)
library(PlayerRatings)
library(tidyr)
library(stringr)

setwd("C:/Users/Won/Dropbox/Won/Data_Urine study/raw data/behaviors")

# Run/Source get_dfy  & plotglicko3==> #had to change -12 to -11======================================
get_dfy <- function(dfx){
  
  #Add Timestamp, Hours, Minutes, Seconds
  dfx$date<-as.Date(dfx$Timestamp, format="%m/%d/%Y %H:%M:%S")
  dfx$day <- as.numeric(dfx$date - min(dfx$date) + 1)
  dfx$hour <- strptime(dfx$Timestamp, format="%m/%d/%Y %H:%M:%S") %>% lubridate::hour(.)-12
  dfx$minute <- strptime(dfx$Timestamp, format="%m/%d/%Y %H:%M:%S") %>% lubridate::minute(.)
  dfx$secs <- strptime(dfx$Timestamp, format="%m/%d/%Y %H:%M:%S") %>% lubridate::second(.)
  dfx <- dfx %>% arrange(day,hour,minute,secs) #arrange by date and time
  
  #Remove Duplicates/Ties that are not Start/End
  dfx$Actor <- as.character(dfx$Actor)
  dfx$Recipient <- as.character(dfx$Recipient)
  dfx <- dfx[(dfx$Actor!=dfx$Recipient) | (dfx$Actor=="End") | (dfx$Actor=="Start"),]
  
  # Unsplit actor/receivers
  dfy <-   do.call(rbind, with(dfx, Map(expand.grid, 
                                        Actor = strsplit(Actor, ", "),
                                        Recipient = strsplit(Recipient, ", "),
                                        Behavior = Behavior,
                                        day = day, hour=hour,minute=minute,secs=secs, Timestamp=Timestamp
  )))
  
  
  # Add observation sample number:
  dfy <- dfy %>% mutate(uniqueobs = cumsum(Behavior=="Start"))
  
  # Add time variable 
  dfy$time <-  dfy$hour*60 + dfy$minute + dfy$secs/60  #add time variable
  
  return(dfy)
}


## Get all files into Global Environment=======================================
temp = list.files(pattern="*_startend.csv")
myfiles = lapply(temp, read.csv, stringsAsFactors=F)
names(myfiles) <-paste0("cohort", stringr::str_extract_all(temp,"[0-9]+"))
lapply(myfiles, head)
lapply(myfiles, tail)

myfiles1 <- lapply(myfiles, get_dfy)
lapply(myfiles1, head)
lapply(myfiles1, str)

#bhvr data=all days=============================================================================
#from the original code, I changed row_number() to as.numeric(rownames()). (two cases)
#I added as.data. frame code in the middle
#run this function
get_all<-function(dflist){
  dflist%>%
  map(~ filter(., Actor!="Start" & Recipient!="End")) %>%
  map(~ arrange(., day,hour,minute,secs)) %>% 
  map(~ mutate(., event=as.numeric(rownames(.)), score=1))%>% 
  map(~ filter(.,(as.character(.$Actor) != as.character(.$Recipient))))%>%
  map(~ select(., event, Actor, Recipient, score)) %>%
  map(~ map_if(., is.factor,as.character) )%>%
  map(~as.data.frame(.,stringsAsFactors =FALSE))%>%
  map(~ glicko(., cval=3, history=T))%>%
  map(~ .$ratings) %>%
  map(~ mutate(.,grank=as.numeric(rownames(.)), winprop = Win/sum(Win), loseprop = Loss/sum(Loss), winloseratio = Win/(Win+Loss)))
  }

a1<-get_all(myfiles1)

#bhvr data=lastN=============================================================================
#run this function (also added some modification)
get_lastN <- function(dflist, N=7) {
  
  dflist %>%
    map(~ filter(., Actor!="Start" & Recipient!="End")) %>%
    map(~ filter(., day>=(max(day)-N))) %>%
    map(~ arrange(., day,hour,minute,secs)) %>% 
    map(~ mutate(., event=as.numeric(rownames(.)), score=1)) %>% 
    map(~ select(., event, Actor, Recipient, score)) %>%
    map(~ map_if(., is.factor,as.character) ) %>%
    map(~as.data.frame(.,stringsAsFactors =FALSE))%>%
    map(~ glicko(., cval=3, history=T)) %>%
    map(~ .$ratings) %>%
    map(~ mutate(., grank=as.numeric(rownames(.)), winprop = Win/sum(Win), loseprop = Loss/sum(Loss), winloseratio = Win/(Win+Loss)))
  
}

a2<-get_lastN(myfiles1) #enter the list of data - defaults to last 7 days
a3<-get_lastN(myfiles1,3) #last 3 days

a2$cohort53
a3$cohort53

#now merge this data into one data frame======================================================
m1<-do.call("rbind", a1)
period<-"all"
m1<-cbind(m1,period)

m2<-do.call("rbind",a2)
period<-"last7"
m2<-cbind(m2,period)

m3<-do.call("rbind",a3)
period<-"last3"
m3<-cbind(m3,period)

m<-rbind(m1,m2,m3)
m<-m%>%
  mutate(cohort=substr(rownames(.),7,8)) #good! substr!!!!!!

m$mouse<-paste0("mouse", stringr::str_extract_all(m$Player,"[0-9]+"))

bw<-read.csv("bw.csv")

behavior<-merge(bw,m)
str(behavior)
head(behavior)
write.csv(behavior,file="behavior.csv")

#took some time but it was good learning experience......
#worked, 2/9/2016 2:26pm!



#first 1, 2, 3, days win/loss===========================================================================
get_firstN <- function(dflist, N=3) {
  
  dflist %>%
    map(~ filter(., Actor!="Start" & Recipient!="End")) %>%
    map(~ filter(., day<=N)) %>%
    map(~ arrange(., day,hour,minute,secs)) %>% 
    map(~ mutate(., event=as.numeric(rownames(.)), score=1)) %>% 
    map(~ select(., event, Actor, Recipient, score)) %>%
    map(~ map_if(., is.factor,as.character) ) %>%
    map(~as.data.frame(.,stringsAsFactors =FALSE))%>%
    map(~ glicko(., cval=3, history=T)) %>%
    map(~ .$ratings) %>%
    map(~ mutate(., grank=as.numeric(rownames(.)), winprop = Win/sum(Win), loseprop = Loss/sum(Loss), winloseratio = Win/(Win+Loss)))
  
}

f1<-get_firstN(myfiles1,1)
f2<-get_firstN(myfiles1,2)
f3<-get_firstN(myfiles1) #default N=3

m1<-do.call("rbind", f1)
period<-"first1"
m1<-cbind(m1,period)

m2<-do.call("rbind",f2)
period<-"first2"
m2<-cbind(m2,period)

m3<-do.call("rbind",f3)
period<-"first3"
m3<-cbind(m3,period)

m<-rbind(m1,m2,m3)
m<-m%>%
  mutate(cohort=substr(rownames(.),7,8)) #good! substr!!!!!!

m$mouse<-paste0("mouse", stringr::str_extract_all(m$Player,"[0-9]+"))

firstbehavior<-m%>%
  select(.,mouse,cohort,grank,Win,Loss,winprop,loseprop,winloseratio,period,Rating)

write.csv(firstbehavior,file="firstbehavior.csv")




# input to function:  leftdf, rightdf, alphamaleid, number of desired rows total, proportion alpha, setseed
# output - realtime + left or right + id to check, same number of rows as input.


l36.9 <- read.csv("c36_left_082515.csv", stringsAsFactors=FALSE)
r36.9 <- read.csv("c36_right_082515.csv", stringsAsFactors=FALSE)

df36.9 <- getdf(l36.9,r36.9)[[1]] %>% mutate(cohort=36, day=9)  #function in feedingdrinking_functions


head(df36.9)

nrow(df36.9)

17292 / 12



df36.9_D <- df36.9[df36.9$drinking>=1,]
df36.9_E <- df36.9[df36.9$eating>=1,]

head(df36.9_D)
nrow(df36.9_D)

head(df36.9_E)
nrow(df36.9_E)



df36.9_D_alpha <- df36.9_D[df36.9_D$subject==alphaid,]
df36.9_D_others <- df36.9_D[df36.9_D$subject!=alphaid,]

df36.9_E_alpha <- df36.9_E[df36.9_E$subject==alphaid,]
df36.9_E_others <- df36.9_E[df36.9_E$subject!=alphaid,]


set.seed(100)
df36.9_D_alpha[sample(nrow(df36.9_D_alpha), 10), ]
df36.9_D_others[sample(nrow(df36.9_D_others), 10), ]




#########################################################################################################



myseed <- 211 #setseed
alphaid<-"M8" #alpha male id
totaldrink <- 20 #total number of rows of drinking
totaleat <- 20 #total number of rows of eating
propalpha <- 0.5  #proportion of observations to be alpha male.

leftdf = l36.9
rightdf = r36.9



############################

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



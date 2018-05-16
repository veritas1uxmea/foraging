### Custom functions for Feeding/Drinking Paper


detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}


# Converts Raw Dataframes into tidied Dataframes
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


# need to add location info
get_dfy_location <- function(dfx){
  
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
                                        day = day, hour=hour,minute=minute,secs=secs, Timestamp=Timestamp,
                                        Location=Location
  )))
  
  
  # Add observation sample number:
  dfy <- dfy %>% mutate(uniqueobs = cumsum(Behavior=="Start"))
  
  # Add time variable 
  dfy$time <-  dfy$hour*60 + dfy$minute + dfy$secs/60  #add time variable
  
  return(dfy)
}

# custom ggplot theme
newggtheme <- theme(
  plot.title = element_text(hjust = 0, vjust = 1, size = rel(2.3)), 
  panel.background = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  plot.background = element_blank(), 
  text = element_text(color = "gray20", size = 10), 
  axis.text = element_text(size = rel(1)), 
  axis.text.x = element_text(color = "gray20", size = rel(1.2)), 
  axis.text.y = element_text(color = "gray20", size = rel(1.2)), 
  axis.title.x = element_text(size = rel(1.3), vjust = 0), 
  axis.title.y = element_text(size = rel(1.3), vjust = 1), 
  axis.ticks.y = element_blank(), 
  axis.ticks.x = element_blank(), 
  strip.text.x = element_text(size = rel(1.5)),
  legend.position = "none",
  legend.key=element_rect(fill=NA),
  legend.title = element_blank(),
  legend.text=element_text(size=rel(1.5)),
  plot.subtitle = element_text(color = "gray20", size = rel(1.0), face="italic"),
  plot.caption = element_text(color = "dodgerblue", size = rel(1.0))
)



# with legends
newggtheme1 <- theme(
  plot.title = element_text(hjust = 0, vjust = 1, size = rel(2.3)), 
  panel.background = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  plot.background = element_blank(), 
  text = element_text(color = "gray20", size = 10), 
  axis.text = element_text(size = rel(1)), 
  axis.text.x = element_text(color = "gray20", size = rel(1.2)), 
  axis.text.y = element_text(color = "gray20", size = rel(1.2)), 
  axis.title.x = element_text(size = rel(1.3), vjust = 0), 
  axis.title.y = element_text(size = rel(1.3), vjust = 1), 
  axis.ticks.y = element_blank(), 
  axis.ticks.x = element_blank(), 
  strip.text.x = element_text(size = rel(1.5)),
  #legend.position = "none",
  legend.key=element_rect(fill=NA),
  legend.title = element_blank(),
  legend.text=element_text(size=rel(1.5)),
  plot.subtitle = element_text(color = "gray20", size = rel(1.0), face="italic"),
  plot.caption = element_text(color = "dodgerblue", size = rel(1.0))
)


#I give up forloop - prone to error 
binom.test.pvalue<-function(x,y){temp<-binom.test(x,y)
return(round(temp[[3]],4))
}


partners_howmany<-function(mygrank=1,mycohortid="3922"){
  temp_df_left<-dfx %>% filter(cohortid==mycohortid&side=="left")
  temp_df_right<-dfx %>% filter(cohortid==mycohortid&side=="right")
  
  a_left<-as.data.frame(table(temp_df_left$realtime)) %>% 
    mutate(realtime=as.numeric(as.character(Var1))) %>% 
    select(realtime,Freq)
  b_left<-temp_df_left %>% filter(grank==mygrank)
  
  c_left<-full_join(a_left,b_left,by="realtime") %>% 
    mutate(present=ifelse(!is.na(grank),1,NA)) %>% 
    mutate(partners=ifelse(!is.na(grank),Freq-present,NA)) %>% 
    select(realtime,partners) %>% 
    filter(!is.na(partners)) %>% 
    mutate(side="left",
           grank=mygrank,
           cohortid=mycohortid)
  
  a_right<-as.data.frame(table(temp_df_right$realtime)) %>% 
    mutate(realtime=as.numeric(as.character(Var1))) %>% 
    select(realtime,Freq)
  b_right<-temp_df_right %>% filter(grank==mygrank)
  
  c_right<-full_join(a_right,b_right,by="realtime") %>% 
    mutate(present=ifelse(!is.na(grank),1,NA)) %>% 
    mutate(partners=ifelse(!is.na(grank),Freq-present,NA)) %>% 
    select(realtime,partners,grank) %>% 
    filter(!is.na(partners)) %>% 
    mutate(side="right",
           cohortid=mycohortid)
  
  result<-rbind(c_left,c_right)
  return(result)
}

partners_howmany_all<-function(mycohortid){
  for(i in 1:12){
    G1<-partners_howmany(1,mycohortid)
    G2<-partners_howmany(2,mycohortid)
    G3<-partners_howmany(3,mycohortid)
    G4<-partners_howmany(4,mycohortid)
    G5<-partners_howmany(5,mycohortid)
    G6<-partners_howmany(6,mycohortid)
    G7<-partners_howmany(7,mycohortid)
    G8<-partners_howmany(8,mycohortid)
    G9<-partners_howmany(9,mycohortid)
    G10<-partners_howmany(10,mycohortid)
    G11<-partners_howmany(11,mycohortid)
    G12<-partners_howmany(12,mycohortid)
    all<-rbind(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12)
    return(all)
  }
}



# function (by. James) to calculate simple ratio association index between two individuals 
# Input the dataframe 


sr_fun <- function(df2, grankA=NULL, grankB=NULL){
  
  df2x <- df2 %>% filter(grank==grankA|grank==grankB) %>% select(realtime, side, grank)
  df2z<-data.table(df2x)
  df2Z <- data.table::dcast(df2z, realtime ~ side, 
                            value.var='grank',
                            fill=NA,  
                            fun.aggregate=list)
  
  ddf <- data.frame(time = df2Z[,1])
  ddf$leftx <- apply(df2Z[,2], 1, function(x) grepl(",", x) )
  ddf$rightx <- apply(df2Z[,3], 1, function(x) grepl(",", x) )
  ddf$left1 <-  apply(df2Z[,2], 1, function(x) grepl(paste0("\\b",grankA,"\\b"), x) )
  ddf$right1 <-  apply(df2Z[,3], 1, function(x) grepl(paste0("\\b",grankA,"\\b"), x) )
  ddf$left2 <-  apply(df2Z[,2], 1, function(x) grepl(paste0("\\b",grankB,"\\b"), x) )
  ddf$right2 <-  apply(df2Z[,3], 1, function(x) grepl(paste0("\\b",grankB,"\\b"), x) )
  ddf$sum1 <- ddf$left1 + ddf$right1
  ddf$sum2 <- ddf$left2 + ddf$right2
  
  #rules
  ddf$value <- ifelse(ddf$leftx==T|ddf$rightx==T, "x",
                      ifelse( (ddf$left1==T & ddf$right2==T) |   (ddf$left2==T & ddf$right1==T)  , "yAB",
                              ifelse(ddf$sum1>0 & ddf$sum2 ==0, "yA",
                                     ifelse(ddf$sum2>0 & ddf$sum1==0, "yB", NA))))
  
  #get values
  x <- sum(ddf$value=="x")
  yAB <- sum(ddf$value=="yAB")
  yA <- sum(ddf$value=="yA")
  yB <- sum(ddf$value=="yB")
  
  sr <- x / (x + yAB + yA + yB)
  sr
  
  return(simple_ratio = sr)
}




sr_fun_check <- function(df2, grankA=NULL, grankB=NULL){
  
  df2x <- df2 %>% filter(grank==grankA|grank==grankB) %>% select(realtime, side, grank)
  df2z<-data.table(df2x)
  df2Z <- data.table::dcast(df2z, realtime ~ side, 
                            value.var='grank',
                            fill=NA,  
                            fun.aggregate=list)
  
  ddf <- data.frame(time = df2Z[,1])
  ddf$leftx <- apply(df2Z[,2], 1, function(x) grepl(",", x) )
  ddf$rightx <- apply(df2Z[,3], 1, function(x) grepl(",", x) )
  ddf$left1 <-  apply(df2Z[,2], 1, function(x) grepl(paste0("\\b",grankA,"\\b"), x) )
  ddf$right1 <-  apply(df2Z[,3], 1, function(x) grepl(paste0("\\b",grankA,"\\b"), x) )
  ddf$left2 <-  apply(df2Z[,2], 1, function(x) grepl(paste0("\\b",grankB,"\\b"), x) )
  ddf$right2 <-  apply(df2Z[,3], 1, function(x) grepl(paste0("\\b",grankB,"\\b"), x) )
  ddf$sum1 <- ddf$left1 + ddf$right1
  ddf$sum2 <- ddf$left2 + ddf$right2
  
  #rules
  ddf$value <- ifelse(ddf$leftx==T|ddf$rightx==T, "x",
                      ifelse( (ddf$left1==T & ddf$right2==T) |   (ddf$left2==T & ddf$right1==T)  , "yAB",
                              ifelse(ddf$sum1>0 & ddf$sum2 ==0, "yA",
                                     ifelse(ddf$sum2>0 & ddf$sum1==0, "yB", NA))))
  
  #get values
  x <- sum(ddf$value=="x")
  yAB <- sum(ddf$value=="yAB")
  yA <- sum(ddf$value=="yA")
  yB <- sum(ddf$value=="yB")
  
  sr <- x / (x + yAB + yA + yB)
  sr
  result<-NULL
  result$datarow=nrow(df2 %>%filter(grank==grankA|grank==grankB))
  result$simple_ratio=sr
  result$denominator=(x + yAB + yA + yB)
  return(result)
}



sr_fun_all<- function(df2){
  temp=data.frame(grankA=NA,
                  grankB=NA,
                  simple_ratio=NA)
  for(i in 1:12){
    for(j in 1:12){
      if (i==j){ a=data.frame(grankA=i,grankB=j,simple_ratio=NA)
      temp<-bind_rows(temp,a)}
      else if (i>j){  a=data.frame(grankA=i,grankB=j,simple_ratio=NA)
      temp<-bind_rows(temp,a)}
      else if (i<j){b=data.frame(grankA=i,grankB=j,simple_ratio=sr_fun(df2,grankA=i,grankB=j))
      temp<-bind_rows(temp,b) 
      }
    }
  }
  result<-temp %>% filter(!is.na(simple_ratio))
  return(result)
}





###HWI - to ultimately calculate HWIG (Godde et al. 2013 Animal Behavior)
hwi_fun <- function(df2, grankA=NULL, grankB=NULL){
  
  df2x <- df2 %>% filter(grank==grankA|grank==grankB) %>% select(realtime, side, grank)
  df2z<-data.table(df2x)
  df2Z <- data.table::dcast(df2z, realtime ~ side, 
                            value.var='grank',
                            fill=NA,  
                            fun.aggregate=list)
  
  ddf <- data.frame(time = df2Z[,1])
  ddf$leftx <- apply(df2Z[,2], 1, function(x) grepl(",", x) )
  ddf$rightx <- apply(df2Z[,3], 1, function(x) grepl(",", x) )
  ddf$left1 <-  apply(df2Z[,2], 1, function(x) grepl(paste0("\\b",grankA,"\\b"), x) )
  ddf$right1 <-  apply(df2Z[,3], 1, function(x) grepl(paste0("\\b",grankA,"\\b"), x) )
  ddf$left2 <-  apply(df2Z[,2], 1, function(x) grepl(paste0("\\b",grankB,"\\b"), x) )
  ddf$right2 <-  apply(df2Z[,3], 1, function(x) grepl(paste0("\\b",grankB,"\\b"), x) )
  ddf$sum1 <- ddf$left1 + ddf$right1
  ddf$sum2 <- ddf$left2 + ddf$right2
  
  #rules
  ddf$value <- ifelse(ddf$leftx==T|ddf$rightx==T, "x",
                      ifelse( (ddf$left1==T & ddf$right2==T) |   (ddf$left2==T & ddf$right1==T)  , "yAB",
                              ifelse(ddf$sum1>0 & ddf$sum2 ==0, "yA",
                                     ifelse(ddf$sum2>0 & ddf$sum1==0, "yB", NA))))
  
  #get values
  x <- sum(ddf$value=="x")
  yAB <- sum(ddf$value=="yAB")
  yA <- sum(ddf$value=="yA")
  yB <- sum(ddf$value=="yB")
  
  hwi <- x / (x + yAB + 0.5*yA + 0.5*yB)
  hwi
  
  return(HWI = hwi)
}

hwi_fun_all<- function(df2){
  temp=data.frame(grankA=NA,
                  grankB=NA,
                  HWI=NA)
  for(i in 1:12){
    for(j in 1:12){
      if (i==j){ a=data.frame(grankA=i,grankB=j,HWI=NA)
      temp<-bind_rows(temp,a)}
      else if (i>j){  a=data.frame(grankA=i,grankB=j,HWI=NA)
      temp<-bind_rows(temp,a)}
      else if (i<j){b=data.frame(grankA=i,grankB=j,HWI=hwi_fun(df2,grankA=i,grankB=j))
      temp<-bind_rows(temp,b) 
      }
    }
  }
  result<-temp %>% filter(!is.na(HWI))
  return(result)
}

hwig_fun<-function(hwi_df,i,j){
  temp_i<-hwi_df %>% filter(grankA==i|grankB==i) %>% summarise(sum_hwi=sum(HWI)) 
  temp_j<-hwi_df %>% filter(grankA==j|grankB==j) %>% summarise(sum_hwi=sum(HWI)) 
  temp_ij<-hwi_df %>% filter(grankA==i&grankB==j) %>% select(HWI)
  sum_hwi=sum(hwi_df$HWI)
  
  hwig<-temp_ij[1,1]*sum_hwi/(temp_i[1,1]*temp_j[1,1])
  return(hwig)
}

hwig_fun_all<- function(hwi_df){
  temp=data.frame(grankA=NA,
                  grankB=NA,
                  HWIG=NA)
  for(i in 1:12){
    for(j in 1:12){
      if (i==j){ a=data.frame(grankA=i,grankB=j,HWIG=NA)
      temp<-bind_rows(temp,a)}
      else if (i>j){  a=data.frame(grankA=i,grankB=j,HWIG=NA)
      temp<-bind_rows(temp,a)}
      else if (i<j){b=data.frame(grankA=i,grankB=j,HWIG=hwig_fun(hwi_df,i,j))
      temp<-bind_rows(temp,b) 
      }
    }
  }
  result<-temp %>% filter(!is.na(HWIG))
  return(result)
}

### calculate gregariousness

greg<-function(df){
  temp_i<-list()
  for(i in 1:12){
  temp_i[[i]]<-df %>% filter(grankA==i|grankB==i) %>% 
    group_by(cohortid) %>%
    summarise(sum_sr=sum(simple_ratio)) %>% 
    mutate(grank=i) %>% 
    as.data.frame()
  }
  temp<-rbindlist(temp_i) %>% as.data.frame()
  return(temp)
}


### maximum quiescence
quiescence<-function(df){
  timegap=data.frame(max_quiescence=NA,realtime_1=NA,realtime_2=NA)
  for(i in 1:(length(df$realtime)-1)){
    timegap[i,1]<-df[i+1,1]-df[i,1]
    timegap[i,2]<-df[i,1]
    timegap[i,3]<-df[i+1,1]
  }
  avg_quiescence=mean(timegap$max_quiescence)
  temp_row<-timegap[which.max(timegap$max_quiescence),]
  result=cbind(df[1,3:6],temp_row,avg_quiescence)
  return(result)
}


### Making half box/half point plot 

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

library(ggplot2)
library(dplyr)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


### Example:
ggplot(diamonds, aes(cut, carat)) +
  geom_flat_violin() +
  coord_flip()





######################################################
#source("https://raw.githubusercontent.com/tidyverse/ggplot2/2f3fef72e140d34210daa9d95917c77b19e89669/R/geom-boxplot.r")
library(grid)

ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

geom_boxplot2 <- function(mapping = NULL, data = NULL,
                          stat = "boxplot", position = "dodge",
                          ...,
                          outlier.colour = NULL,
                          outlier.color = NULL,
                          outlier.fill = NULL,
                          outlier.shape = 19,
                          outlier.size = 1.5,
                          outlier.stroke = 0.5,
                          outlier.alpha = NULL,
                          notch = FALSE,
                          notchwidth = 0.5,
                          varwidth = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBoxplot2 <- ggproto("GeomBoxplot2", Geom,
                        setup_data = function(data, params) {
                          data$width <- data$width %||%
                            params$width %||% (resolution(data$x, FALSE) * 0.9)
                          
                          if (!is.null(data$outliers)) {
                            suppressWarnings({
                              out_min <- vapply(data$outliers, min, numeric(1))
                              out_max <- vapply(data$outliers, max, numeric(1))
                            })
                            
                            data$ymin_final <- pmin(out_min, data$ymin)
                            data$ymax_final <- pmax(out_max, data$ymax)
                          }
                          
                          # if `varwidth` not requested or not available, don't use it
                          if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
                            data$xmin <- data$x - data$width / 2
                            data$xmin2 <- data$x - data$width / 4
                            data$xmax <- data$x + data$width / 2
                          } else {
                            # make `relvarwidth` relative to the size of the largest group
                            data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
                            data$xmin <- data$x - data$relvarwidth * data$width / 2
                            data$xmin2 <- data$x - data$relvarwidth * data$width / 4
                            data$xmax <- data$x + data$relvarwidth * data$width / 2
                          }
                          data$width <- NULL
                          if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL
                          
                          data
                        },
                        
                        draw_group = function(data, panel_params, coord, fatten = 2,
                                              outlier.colour = NULL, outlier.fill = NULL,
                                              outlier.shape = 19,
                                              outlier.size = 1.5, outlier.stroke = 0.5,
                                              outlier.alpha = NULL,
                                              notch = FALSE, notchwidth = 0.5, varwidth = FALSE) {
                          
                          common <- data.frame(
                            colour = data$colour,
                            size = data$size,
                            linetype = data$linetype,
                            fill = alpha(data$fill, data$alpha),
                            group = data$group,
                            stringsAsFactors = FALSE
                          )
                          
                          whiskers <- data.frame(
                            x = c(data$x,data$x,data$xmin2,data$xmin2),
                            xend = c(data$x,data$x,data$x,data$x),
                            y = c(data$upper, data$lower,data$ymax,data$ymin),
                            yend = c(data$ymax, data$ymin,data$ymax,data$ymin),
                            alpha = NA,
                            common,
                            stringsAsFactors = FALSE
                          )
                          
                          
                          box <- data.frame(
                            xmin = data$xmin,
                            xmax = data$x,
                            ymin = data$lower,
                            y = data$middle,
                            ymax = data$upper,
                            ynotchlower = ifelse(notch, data$notchlower, NA),
                            ynotchupper = ifelse(notch, data$notchupper, NA),
                            notchwidth = notchwidth,
                            alpha = data$alpha,
                            common,
                            stringsAsFactors = FALSE
                          )
                          
                          if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
                            outliers <- data.frame(
                              y = data$outliers[[1]],
                              x = data$x[1],
                              colour = outlier.colour %||% data$colour[1],
                              fill = outlier.fill %||% data$fill[1],
                              shape = outlier.shape %||% data$shape[1],
                              size = outlier.size %||% data$size[1],
                              stroke = outlier.stroke %||% data$stroke[1],
                              fill = NA,
                              alpha = outlier.alpha %||% data$alpha[1],
                              stringsAsFactors = FALSE
                            )
                            outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
                          } else {
                            outliers_grob <- NULL
                          }
                          
                          ggname("geom_boxplot2", grobTree(
                            outliers_grob,
                            GeomSegment$draw_panel(whiskers, panel_params, coord),
                            GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
                          ))
                        },
                        
                        draw_key = draw_key_boxplot,
                        
                        default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                          alpha = NA, shape = 19, linetype = "solid"),
                        
                        required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)

#################################################################
stat_boxplot <- function(mapping = NULL, data = NULL,
                         geom = "boxplot", position = "dodge",
                         ...,
                         coef = 1.5,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBoxplot <- ggproto("StatBoxplot", Stat,
                       required_aes = c("x", "y"),
                       non_missing_aes = "weight",
                       
                       setup_params = function(data, params) {
                         params$width <- params$width %||% (resolution(data$x) * 0.75)
                         
                         if (is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
                           warning(
                             "Continuous x aesthetic -- did you forget aes(group=...)?",
                             call. = FALSE)
                         }
                         
                         params
                       },
                       
                       compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
                         qs <- c(0, 0.25, 0.5, 0.75, 1)
                         
                         if (!is.null(data$weight)) {
                           mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                           stats <- as.numeric(stats::coef(mod))
                         } else {
                           stats <- as.numeric(stats::quantile(data$y, qs))
                         }
                         names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                         iqr <- diff(stats[c(2, 4)])
                         
                         outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + coef * iqr)
                         if (any(outliers)) {
                           stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
                         }
                         
                         if (length(unique(data$x)) > 1)
                           width <- diff(range(data$x)) * 0.9
                         
                         df <- as.data.frame(as.list(stats))
                         df$outliers <- list(data$y[outliers])
                         
                         if (is.null(data$weight)) {
                           n <- sum(!is.na(data$y))
                         } else {
                           # Sum up weights for non-NA positions of y and weight
                           n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                         }
                         
                         df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                         df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                         
                         df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                         df$width <- width
                         df$relvarwidth <- sqrt(n)
                         df
                       }
)


###
### Summarize Ratings Table for Various  Measures of Inequality

domineq <- function(df, top=12){
  
  dineq <-  df %>% 
    mutate(winprop=Win/sum(Win))%>%
    filter(row_number()<=top) %>%
    mutate(winpct=Win/Games) %>%
    select(2,5,7,9,10) %>% 
    data.table %>%
    .[,c(list(stat=c('mean','sd', 'CV', 'max', 'min', 'Gini', 'LAsym', 'Theil', 'Atkinson')),
         lapply(.SD,function(x)c(mean(x),sd(x), ineq::var.coeff(x), max(x), min(x),
                                 ineq::Gini(x), ineq::Lasym(x), ineq::Theil(x), ineq::Atkinson(x))))] %>%
    as.data.frame
  
  
  
  dineq[,2:6] <- round(dineq[,2:6], 3)
  
  return(dineq)
  
}

### 

#### Unpacking Glicko history data for better replotting

plotglicko1 <- function(df, cval=3, mycolors=c("black", "grey", "orange", "red"), 
                        ltypes=c(1,2,3,1,2,3,1,2,3,1,2,3), thetitle="", events=c(0), linewd=1, ylim1=1000,ylim2=3200,
                        ndays=1){
  
  robj <- glicko(df, cval=cval, history=T)
  
  x<-unlist(robj$history) %>% as.data.frame
  
  z<-as.factor(df[,1])  #this is the df the glicko was conducted on
  n<-nlevels(z)
  
  x.ratings<-x[,1:n]
  x.deviations<-x[,(1+n):(n+n)]
  
  #longform the data
  x.ratingsmelt<-reshape2::melt(x.ratings)
  
  ids<-rownames(x.ratings)       #to make id column
  x.ratingsmelt$ids<-rep(ids, n)  #making id column
  
  l.ids<-length(ids)
  x.ratingsmelt$event<-rep(1:n, each=l.ids) 
  
  
  #add ranks
  xrn<-as.data.frame(x.ratings[n])
  colnames(xrn)<-c("finalrating")
  
  x.ratingsmelt$rank<-rank(-xrn$finalrating, ties.method="random")
  head(x.ratingsmelt)
  
  #make ids1 a factor with levels defined by rank
  x.ratingsmelt1 <- data.frame(ids=unique(x.ratingsmelt$ids),rank=unique(x.ratingsmelt$rank))
  x.ratingsmelt1 <- x.ratingsmelt1[order(x.ratingsmelt1$rank),]
  x.ratingsmelt$ids1 <- factor(x.ratingsmelt$ids,levels=x.ratingsmelt1$ids)
  head(x.ratingsmelt)
  str(x.ratingsmelt) #note the difference between ids and ids1
  
  
  #define color palette, multiple options (see below)
  colourCount <-length(unique(x.ratingsmelt$ids))
  getPalette = colorRampPalette(mycolors)
  
  ## Make labels for x-axis (days not events index)
  eventsb <- events[seq(1, length(events), ndays)]
  
  
  ### now plot using ids1 instead of ids.
  p1<-ggplot(x.ratingsmelt, aes(x = event, y = value, col=ids1, linetype=ids1)) +
    scale_colour_manual(values = getPalette(colourCount)) +
    scale_linetype_manual(values=ltypes) +
    scale_x_discrete(breaks=eventsb, labels=(1:length(eventsb)*ndays)) +
    ylab("Glicko Rating") +
    xlab("") +
    #  geom_hline(yintercept=2200, lwd=.5, color="gray54", lty=1) +
    geom_vline(xintercept = events, color="gray86", lty=2) +
    ggtitle(thetitle)+
    ylim(ylim1,ylim2)+
    geom_line(lwd = linewd) +
    theme(plot.title = element_text(hjust = 0, vjust = 1, size = rel(2.3)), 
          panel.background = element_blank(), 
          plot.background = element_blank(), 
          panel.grid.major.y = element_line(color = "gray65"), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor = element_blank(), 
          strip.background  = element_blank(),
          strip.text = element_text(size=rel(1.8)),
          text = element_text(color="gray20", size=10),
          axis.text = element_text(size=rel(1.0)),
          # axis.text.x = element_blank(),
          axis.text.x = element_text(color="gray20", size=rel(1.6)),
          axis.text.y = element_text(color="gray20", size=rel(1.6)),
          axis.title.x = element_text(size=rel(2.0), vjust=0),
          axis.title.y = element_text(size=rel(1.7), vjust=1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  
  
  return(p1)
}



###

plotglicko3 <- function(df, cval=3, mycolors=c("black", "grey", "orange", "red"), 
                        ltypes=c(1,2,3,1,2,3,1,2,3,1,2,3), thetitle="", events=c(0), linewd=1, ylim1=1250,ylim2=3250,
                        ndays=1){
  
  robj <- glicko(df, cval=cval, history=T)
  
  x<-unlist(robj$history) %>% as.data.frame
  
  z<-as.factor(df[,1])  #this is the df the glicko was conducted on
  n<-nlevels(z)
  
  x.ratings<-x[,1:n]
  x.deviations<-x[,(1+n):(n+n)]
  
  #longform the data
  x.ratingsmelt<-reshape2::melt(x.ratings)
  
  ids<-rownames(x.ratings)       #to make id column
  x.ratingsmelt$ids<-rep(ids, n)  #making id column
  
  l.ids<-length(ids)
  x.ratingsmelt$event<-rep(1:n, each=l.ids) 
  
  
  #add ranks
  xrn<-as.data.frame(x.ratings[n])
  colnames(xrn)<-c("finalrating")
  
  x.ratingsmelt$rank<-rank(-xrn$finalrating, ties.method="random")
  head(x.ratingsmelt)
  
  #make ids1 a factor with levels defined by rank
  x.ratingsmelt1 <- data.frame(ids=unique(x.ratingsmelt$ids),rank=unique(x.ratingsmelt$rank))
  x.ratingsmelt1 <- x.ratingsmelt1[order(x.ratingsmelt1$rank),]
  x.ratingsmelt$ids1 <- factor(x.ratingsmelt$ids,levels=x.ratingsmelt1$ids)
  head(x.ratingsmelt)
  str(x.ratingsmelt) #note the difference between ids and ids1
  
  
  #define color palette, multiple options (see below)
  colourCount <-length(unique(x.ratingsmelt$ids))
  getPalette = colorRampPalette(mycolors)
  
  ## Make labels for x-axis (plot each week)
  week1 <- events[7]
  week2 <- events[14]
  week3 <- events[21]
  
  eventsb <- c(week1,week2,week3)
  
  
  ### now plot using ids1 instead of ids.
  p1<-ggplot(x.ratingsmelt, aes(x = event, y = value, col=ids1, linetype=ids1)) +
    scale_colour_manual(values = getPalette(colourCount)) +
    scale_linetype_manual(values=ltypes) +
    scale_x_discrete(breaks=eventsb, labels=c("1", "2", "3")) +
    ylab("Glicko Rating") +
    xlab("Weeks") +
    #  geom_hline(yintercept=2200, lwd=.5, color="gray54", lty=1) +
    geom_vline(xintercept = eventsb, color="gray86", lty=2) +
    ggtitle(thetitle)+
    ylim(ylim1,ylim2)+
    geom_line(lwd = linewd) +
    theme(plot.title = element_text(hjust = 0, vjust = 1, size = rel(2.3)), 
          panel.background = element_blank(), 
          plot.background = element_blank(), 
          panel.grid.major.y = element_line(color = "gray65"), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor = element_blank(), 
          strip.background  = element_blank(),
          strip.text = element_text(size=rel(1.1)),
          text = element_text(color="gray20", size=10),
          axis.text = element_text(size=rel(1.0)),
          # axis.text.x = element_blank(),
          axis.text.x = element_text(color="gray20", size=rel(1.0)),
          axis.text.y = element_text(color="gray20", size=rel(1.0)),
          axis.title.x = element_text(size=rel(1.0), vjust=0),
          axis.title.y = element_text(size=rel(1.0), vjust=1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  
  
  return(p1)
}

library("ggmap")
library("ggplot2")
library("geosphere")
library("gridExtra")
library(dplyr)
library(RColorBrewer)
library(lubridate)
data<-read.csv("the_hawks.csv")
data$tag<-as.factor(data$tag)
box<-c(-121.7843,38.3565,-121.3615,38.8835)
long_range<-range(data$long)
lat_range<-range(data$lat)
mymap<-get_map(location=c(long_range[1]+(long_range[2]-long_range[1])/2,
                          lat_range[1]+(lat_range[2]-lat_range[1])/2))
ggmap(mymap)+
   geom_point(data=data,aes(x=long,y=lat,color=tag),alpha=0.5)+
  labs(title="Spatial Map of Hawks' Location",x="Longitude",y="Latitude")

#2
data[which(data$stage=="arrival"),]
set1<-subset(data,stage=="arrival"&tag=="105936")
set2<-subset(data,stage=="arrival"&tag=="105928")
set1_long<-range(set1$long)
set1_lat<-range(set1$lat)
set2_long<-range(set2$long)
set2_lat<-range(set2$lat)

set1map<-get_map(location=c(set1_long[1]+(set1_long[2]-set1_long[1])/2,
                            set1_lat[1]+(set1_lat[2]-set1_lat[1])/2),zoom=15)
set2map<-get_map(location=c(set2_long[1]+(set2_long[2]-set2_long[1])/2,
                            set2_lat[1]+(set2_lat[2]-set2_lat[1])/2),zoom=15)
start_end1<-as.data.frame(rbind(c(set1$long[1],set1$lat[1]),
                                c(set1$long[length(set1$long)],set1$lat[length(set1$lat)])))
ggmap(set1map)+
  geom_point(data=set1,aes(x=long,y=lat,size=speed,color=height))+
  labs(title="Arrival Sequence of Tag 105936",x="Longitude",y="Latitude")+
  geom_segment(aes(x=set1$long[-1],y=set1$lat[-1],xend=set1$long[-length(set1$long)],
                   yend=set1$lat[-length(set1$lat)]),data=set1[-1,],
               arrow = arrow(length=unit(0.15,"inches")),alpha=0.3)+
   geom_point(data=start_end1,aes(x=V1,y=V2),color=c("green","red"))

start_end2<-as.data.frame(rbind(c(set2$long[1],set2$lat[1]),
                                c(set2$long[length(set2$long)],set2$lat[length(set2$lat)])))
ggmap(set2map)+
  geom_point(data=set2,aes(x=long,y=lat,size=speed,color=height))+
  labs(title="Arrival Sequence of Tag 105928",x="Longitude",y="Latitude")+
  geom_segment(aes(x=set2$long[-length(set2$long)],y=set2$lat[-length(set2$lat)],xend=set2$long[-1],
                 yend=set2$lat[-1]),data=set2[-1,],
            arrow = arrow(length=unit(0.15,"inches")),alpha=0.3)+
  geom_point(data=start_end2,aes(x=V1,y=V2),color=c("green","red"))
  
#3
tag<-unique(data$tag)
data_by_tag<-lapply(tag,function(x)subset(data,tag==x))
data_by_nest<-lapply(data_by_tag,function(x)filter(x,stage=="nestling"))
hawk_nest<-as.data.frame(t(sapply(data_by_nest,FUN=function(x)apply(x[,3:4],2,FUN = median))))
data_by_premigra<-lapply(data_by_tag,function(x)filter(x,stage=="preMigration"))
data_by_premigra[3]<-NULL



dist_to_nest<-lapply(1:4,function(x)as.data.frame(distGeo(data_by_premigra[[x]][,3:4],hawk_nest[-3,][x,])/1609))
dist_to_nest<-lapply(1:4,function(x)cbind(data_by_premigra[[x]]$time,dist_to_nest[[x]]))
dist_to_nest<-lapply(dist_to_nest,setNames,c("time","distance"))
dist_to_nest<-lapply(dist_to_nest,function(x){x$time<-ymd_hms(x$time)
       return(x)})



ggplot(data=dist_to_nest[[1]],aes(x=time,y=distance))+geom_bar(stat="identity")+
  scale_x_datetime()+labs(y="distance to nest",title="Hawk 105936 Distance to Nest at PreMigrate Stage")
ggplot(data=dist_to_nest[[2]],aes(x=time,y=distance))+geom_bar(stat="identity")+
  scale_x_datetime()+labs(y="distance to nest",title="Hawk 105930 Distance to Nest at PreMigrate Stage")
ggplot(data=dist_to_nest[[3]],aes(x=time,y=distance))+geom_bar(stat="identity")+
  scale_x_datetime()+labs(y="distance to nest",title="Hawk 105928 Distance to Nest at PreMigrate Stage")
ggplot(data=dist_to_nest[[4]],aes(x=time,y=distance))+geom_bar(stat="identity")+
  scale_x_datetime()+labs(y="distance to nest",title="Hawk 117527 Distance to Nest at PreMigrate Stage")

#4
leave_seq1<-data_by_premigra[[1]][which(date(data_by_premigra[[1]]$time)>="2012-08-05"),]
nrow(leave_seq1)
ggmap(mymap)+
  geom_point(data=leave_seq1,aes(x=long,y=lat),alpha=0.5)+
  labs(title="Spatial Map of Hawks' Location",x="Longitude",y="Latitude")+
  geom_point(data=hawk_nest[1,],aes(x=long,y=lat),color="blue")+
  geom_segment(aes(x=leave_seq1$long[-1],y=leave_seq1$lat[-1],xend=leave_seq1$long[-8],
                  yend=leave_seq1$lat[-8]),data=leave_seq1[-1,],
              arrow = arrow(length=unit(0.1,"inches"),ends="first"),alpha=0.3)+
  geom_point(data=leave_seq[c(1,8),],aes(x=long,y=lat),color=c("green","red"))


leave_seq2<-data_by_premigra[[3]][which(date(data_by_premigra[[3]]$time)>="2012-09-13"),]
nrow(leave_seq2)
ggmap(mymap)+
  geom_point(data=leave_seq2,aes(x=long,y=lat),alpha=0.5)+
  labs(title="Spatial Map of Hawks' Location",x="Longitude",y="Latitude")+
  geom_point(data=hawk_nest[4,],aes(x=long,y=lat),color="blue")+
  geom_segment(aes(x=leave_seq2$long[-1],y=leave_seq2$lat[-1],xend=leave_seq2$long[-9],
                   yend=leave_seq2$lat[-9]),data=leave_seq2[-1,],
               arrow = arrow(length=unit(0.1,"inches"),ends="first"),alpha=0.3)+
  geom_point(data=leave_seq2[c(1,9),],aes(x=long,y=lat),color=c("green","red"))

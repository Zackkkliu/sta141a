#read in data
data<-readRDS("housing.rds")
data1<-readRDS("housing.rds")
#load library
library(dplyr)
library(tidyr)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))
library(lubridate)
library(treemap)
library(maps)
#1
#get a summary of the data
str(data)
#turn the data into appropriate types.
data1$county<-as.factor(data$county)
data1$zip <-as.factor(data$zip)
data1$street <- as.character((data$street))
data1$br<- as.factor(data$br)
data1$lsqft <-as.numeric(data$lsqft)
data1$year <- as.integer(data$year)
data1$date <- as_date(data$date)

#I didn't do most of clean in the first question, I cleaned
#data when I needed it in the later questions.

data1$year[which(data1$year<1800|data1$year>2018)]=NA #set invaild year to NA

#2
#timespan for year of construction and date of sales 
range(data1$year,na.rm=TRUE)
range(data1$date)

#3
#round down the date to month 
data1$date<-floor_date(data1$date, unit = "month")
#create a dataframe that store the monthly sales. 
monthsales<-as.data.frame(table(data1$date))
names(monthsales)<-c("date","sales")
monthsales$date<-ymd(monthsales$date)
#create a plot of monthly sales over time
ggplot(monthsales,aes(date,sales))+geom_point()+
  scale_x_date(date_labels = "%b %Y")+labs(title="Monthly sales over time",y="Monthly Sales")
#get monthly average house prices and store as dataframe
monthprice<-aggregate(data1$price,list(data1$date),mean,na.rm=TRUE)
names(monthprice)<-c("date","price")
#create a plot of monthly sales over time
ggplot(monthprice,aes(date,price))+geom_point()+
  scale_x_date(date_labels = "%b %Y")+labs(title="Monthly price over time",y="Monthly price", x="Month")+
  scale_y_continuous(labels=scales::dollar)

#4
#correcting county names
table(data1$county) 
data1$county<-gsub("county","County",data1$county,ignore.case=TRUE)
data1$county<-gsub("San Franciscoe","San Francisco",data1$county)
table(data1$county)
#Alpine is not a county in bay area, by the zip code we notice that they are from SF county
data1$zip[which(data1$county=="Alpine County")]
data1$county<-gsub("Alpine County","San Francisco County",data1$county)
data1$county<-as.factor(data1$county)
#catergorize bathroom
levels(data1$br)<-c(levels(data1$br),"4+") #add a level "4+"
data1$br[which(data$br>=4)]="4+"
data1$br<-droplevels(data1$br) #drop unused levels
#convert sale date into year and make it categorical 
data1$date<-year(data1$date)
data1$date<-as.factor(data1$date)
#create a dataframe that store the average price of b
average_price<-aggregate(price~br+county+date,data1,mean)
#remove year 2006 data
average_price<-average_price[which(average_price$date!=2006),]
#plot the associated graph
ggplot(average_price,aes(date,price,linetype=br,group=br))+geom_point()+geom_line()+
  facet_wrap(~county)+scale_y_continuous(labels=scales::dollar)+
  labs(x="Year",y="Average Price",title="Average Sale Price")+
  scale_linetype_discrete(name = "#'s of bedroom")

#5
#create a table that summarize the sales by city and county
city_county<-as.matrix(table(data1$city,data1$county))
#display the number of county that a city has sales in. 
city_county[155,]
rowSums(city_county!=0)
#display the city has sale in more than one county
which(rowSums(city_county!=0)>1)

#6
#plot the original data and diagnostic  plots
plot(price~bsqft,data1)
model<-lm(price~bsqft,data1)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
#transforming data
y<-log(data1$price+1)
x<-log(data1$bsqft)
#plot the transformed data and diagnostic plots
trans_model<-lm(y~x)
par(mfrow=c(2,2))
plot(trans_model)
par(mfrow=c(1,1))
plot(y~x)
data1[which(y<5),]
#add regression line
abline(trans_model)
#7
#fit multivariate model
multi_model=lm(price~bsqft+lsqft,data1)
#get coefficients and their standard errors
coef<-summary(multi_model)$coefficients
beta=coef[2,1]-coef[3,1]
beta_sd=sqrt(coef[2,2]^2+coef[3,2]^2)
test_statistic=beta/beta_sd
#8
ggplot(data1,aes(y=price,x=bsqft,color=county))+stat_smooth(method="lm",se=FALSE,fullrange=TRUE)+
  scale_y_continuous(labels=scales::dollar)+labs(title="Regression Line for Separate County",y="Price")
#9
#create a dataframe the has the average sale price by city and county
price_by_cc<-aggregate(price~city+county,data1,mean)
#create a dataframe that store the sales in each city
city_count<-as.data.frame(table(data1$city,data1$county))
#rename the coloumn names 
names(city_count)<-c("city","county","count")
#delete garbege values 
city_count<-city_count[which(city_count$count!=0),]
#get top 3 cities for each county
top3<-lapply(split(city_count,city_count$county),function(x)top_n(x,3))
#initialize a empty dataframe to store all top 3 cities
totaltop3<-top3[[1]][FALSE,]
#store all top 3 cities of each counties in one dataframe
for(i in 1:9){
  totaltop3<-rbind(totaltop3,top3[[i]])
}
#create a coloumn price in totaltop3 to store averge sale price.

for(i in 1:dim(totaltop3)[1]){
  #use if-else to choose correct average sale price for "Valleo" 
  if(length(price_by_cc[which(price_by_cc$city==totaltop3$city[i]),]$price)==2){
    totaltop3$price[i]<-price_by_cc[which(price_by_cc$city==totaltop3$city[i]),]$price[2]
  }
  else{
    totaltop3$price[i]<-price_by_cc[which(price_by_cc$city==totaltop3$city[i]),]$price
  }
}
#plot treemap
treemap(totaltop3,index=c("county","city"),vSize="count",type="value",vColor="price",
        palette = "RdYlBu",title="Average Sales For Top 3 Cities In Each County ",
        fontsize.labels = c(8, 8),
        align.labels = list(c("left", "top"), c("center","center")),title.legend = "Average Sale Price")

#10.1 use lots of reference provided by patrick on piazza 
#select san francisco data and factorize longtitude and latitude
sfdata<-filter(data1,county=="San Francisco County")
sfdata$long2<-round(sfdata$long,2)
sfdata$lat2<-round(sfdata$lat,2)
long_range<-range(sfdata$long2,na.rm=TRUE)
long_seq<-seq(long_range[1],long_range[2],0.01)
sfdata$longF<-factor(sfdata$long2,levels=long_seq)
lat_range<-range(sfdata$lat2,na.rm=TRUE)
lat_seq<-seq(lat_range[1],lat_range[2],0.01)
sfdata$latF<-factor(sfdata$lat2,levels=lat_seq)
#ceate a dataframe that has average price and number of sales in each tiles
sf_agg<-aggregate(price~latF+longF,sfdata,function(x)c(mean(x),length(x),drop=FALSE))
#refine dataframe for create a matrix
sf_agg$ave_price<-sf_agg$price[,1]
sf_agg$sales<-sf_agg$price[,2]
sf_agg_price<-select(sf_agg,latF,longF,ave_price)
#sort data by longtitude and latitdue
sf_agg_price<-arrange(sf_agg_price,latF,longF)
#convert dataframe to a matrix for heatmap
sf_agg_price_m<-as.matrix(spread(sf_agg_price,latF,ave_price))
storage.mode(sf_agg_price_m)<-"numeric"
sf_agg_price_m1<-sf_agg_price_m[,2:12]
#create heatmap
image(z=sf_agg_price_m1,y=lat_seq,x=long_seq,
      xlab="longtitude",ylab="latitude",main="Heapmap For Average Price of Sales")
points(y=sfdata$lat,x=sfdata$long)
#create boarder
object<-(map("county","california,san francisco",plot=FALSE))
lines(object$x,object$y)
#10.2
#similiar to above, create appropriate matrix for heatmap
sf_agg_sales<-select(sf_agg,latF,longF,sales)
sf_agg_sales<-arrange(sf_agg_sales,latF,longF)
sf_agg_sales_m<-as.matrix(spread(sf_agg_sales,latF,sales))
storage.mode(sf_agg_sales_m)<-"numeric"
sf_agg_sales_m1<-sf_agg_sales_m[,2:12]
#draw heatmap , boarder and excat location
image(z=sf_agg_sales_m1,y=lat_seq,x=long_seq,
      xlab="longtitude",ylab="latitude",main="Heapmap For Number of Sales")
points(y=sfdata$lat,x=sfdata$long)
lines(object$x,object$y)

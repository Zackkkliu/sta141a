############ HW1 ###################
#read data
data=readRDS("college_scorecard_2013.rds")
#1)
dim(data) #each row represents one observation
sum(data$main_campus)#number of college
#2)
dim(data) #each column represnets one feature
table(sapply(data, class)) #categorize the features
#3)
NA_counts=colSums(is.na(data))
sum(NA_counts) #total number of NA's
which.max(NA_counts) #the feature that has the most missing values
hist(NA_counts) #display the pattern of missing values
#4)
table(factor(data$ownership=="Public",labels=c("Private","Public"))) #count the numbers of private and public campus.
data$is_public=factor(data$ownership=="Public",labels=c("Private","Public")) #creates a new column that indicate whether a campus is private
prop.table(table(data$is_public,data$highest_degree),margin=1) #shows that proportion of different highest degrees awarded in private and public
par(mfrow=c(1,2)) #combine two graph
barplot(prop.table(table(data[which(data$is_public=="Public"),]$highest_degree)),ylim=c(0,1),main="Public Campus") #generates a barplot of the proportion of highest degree awarded in public
barplot(prop.table(table(data[which(data$is_public=="Private"),]$highest_degree)),ylim=c(0,1),main="Private Campus") #generates a barplot of the proprtion of highest degree awarded in priviate

#5)
par(mfrow=c(1,1))
UP_mean=mean(data$undergrad_pop,na.rm=TRUE) #get the average for the undergraduate population
UP_median=median(data$undergrad_pop,na.rm=TRUE) #get the median for the undergraduate population
UP_decile=quantile(data$undergrad_pop, seq(0, 1, 0.1), na.rm = TRUE) #get the deciles for the UP
plot(density(data$undergrad_pop,na.rm=TRUE),main="denstiy plot",xlab="Undergraduate Popluation") #plot the denstiy plot for UP
abline(v=UP_mean,col="red",lty="dotted") #add mean to the plot 
abline(v=UP_median,col="blue",lty="dashed") #add median to the plot
legend("topright",legend=c("mean","median"),lty=c("dotted","dashed"),col=c("red","blue")) #add legend

#6)
pop=data[data$state%in%c("CA","TX","NY","IL","FL"),]
pop=droplevels(pop)
pop
boxplot(tuition~state, data=pop, ylab="Tuition")
#7)
#a)
data[which.max(data$avg_sat),"name"]
#b
data[which.max(data$undergrad_pop),"open_admissions"]
#c
publicC_index=which(data$is_public=="Public")
publicC=data[publicC_index,]
publicC[which.min(publicC$avg_family_inc),"zip"]
#d
data[which.max(data$undergrad_pop),"name"]
data[which.max(data$grad_pop),"name"]

#8
#a)
forProfit=data[which(data$ownership=="For Profit"),]
data2=forProfit[which(forProfit$primary_degree=="Bachelor"),]
plot(data2$revenue_per_student,data2$spend_per_student)
model=lm(data2$spend_per_student~data2$revenue_per_student)
par(mfrow = c(2, 2))
plot(model)
#b)
data2[is.na(data2$grad_pop),"grad_pop"]=0
data2$total_pop=data2$undergrad_pop+data2$grad_pop
data2$total_net_income=(data2$revenue_per_student-data2$spend_per_student)*data2$total_pop
data2[order(data2$total_net_income),"name"]

#9
#part a
par(mfrow = c(1, 1))
plot(data$avg_sat,data$admission)
group_index=which(data$avg_sat>1100&data$admission<0.5)
data[group_index,"group"]="group1"       
data[-group_index,"group"]="group2"
#part b
#a
boxplot(med_10yr_salary~group, data=data, ylab="med_10yr_salary")
#b
data$totalAW=data$race_asian+data$race_white
boxplot(totalAW~group, data=data, ylab="total asian and white percentage")
#c
data$grad_per=data$grad_pop/(data$grad_pop+data$undergrad_pop)
boxplot(grad_per~group, data=data, ylab="percentage of graduates")
#part c
#a
table(data$open_admissions,data$group)
table(data$main_campus,data$group)
table(data$ownership,data$group)
data$branch_more_than_one=ifelse(data$branches>1,TRUE,FALSE)
table(data$branch_more_than_one,data$group)

#10)
#a)
plot(data$avg_family_inc,data$avg_10yr_salary)
abline(lm(data$avg_10yr_salary~data$avg_family_inc))
summary(lm(data$avg_10yr_salary~data$avg_family_inc))$r.squared
summary(lm(data$avg_10yr_salary~data$avg_family_inc+data$group))
#b)
subset(data,data$avg_10yr_salary>150000&data$avg_family_inc<40000)
str(data)

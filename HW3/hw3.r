#HW3
#part1
#1.1
library(dplyr)
library(ggplot2)
library(tidyr)
#store the indices of appropriate tiles.
go=0
cc=c(2,17,33)
ch=c(7,22,36)
j=10
g2j=30
rr=c(5,15,25,35)
e3=24
h2=39
ut=c(12,28)
deck_cc=sample(1:16,16)
deck_ch=sample(1:16,16)
t=c(4,38)
# I use many sub-functions to support simulate_monopoly.
#The take_cc function simulate drawing a community chest card.
take_cc<-function(position){
  draw<-deck_cc[1]
  deck_cc<<-c(deck_cc[-1],deck_cc[1]) #giving by Patrick on piazza
  if (draw==1){
    return(go)
  }else if(draw==2){
    return(j)
  }else{
    return(position)
  }
}
#The take_ch function simulate drawing a chance card
take_ch<-function(position){
  draw<-deck_ch[1]
  deck_ch<<-c(deck_ch[-1],deck_ch[1]) #giving by Patrick on piazza
  draw<-as.character(draw) #coerce integer to character for using swich
  switch(draw,
         "1"=return(go),
         "2"=return(j),
         "3"=return(cc[1]),
         "4"=return(e3),
         "5"=return(h2),
         "6"=return(rr[1]),
         "7"=,"8"={
           if (position==ch[1]){
             return(rr[2])
           }else if (position==ch[2]){
             return(rr[3])
           }else{
             return(rr[1])
           }
         },
         "9"={
           if (position==ch[1]){
             return(ut[1])
           }else if (position==ch[2]){
             return(ut[2])
           }else {
             return(ut[1])
           }
         },
         "10"=return(position-3),
         return(position)
         )
}
# The lookup function checks what the player should do.
lookup<-function(position){
  if (position==g2j){
    return(j)
  }else if (position %in% cc){
    return(take_cc(position))
  }else if (position %in% ch){
    return(take_ch(position))
  }else
    return(position)
}
double=0 #counter for consecutive doubles
#The main simulate_monopoly function
simulate_monopoly<-function(n,d){
  #initialize a array to store the landing position at each turn
  position<- rep(0,n+1)
  for (i in 2:(n+1)){
    #simulate dices
    dice1<-sample(1:d,1)
    dice2<-sample(1:d,1)
    dice_output<-dice1+dice2
    #check consecutive doubles
    if (dice1==dice2){
      double<-double+1
    }else{
      double<-0
    }
    if (double==3){
      #three consecutive doubles, send the player to jail
      position[i]<-j
      double<-0
    }else{
      #assign landing position 
      position[i]<-(position[i-1]+dice_output)%%40
      position[i]<-lookup(position[i]) 
    }
  }
  position
}

#1.2
#The estimate_monopoly function uses simulate_monoploy and it returns 
#a dataframe that store the probability of landing on each square.
estimate_monopoly<-function(n,d){
  result<-as.data.frame(prop.table(table(factor(simulate_monopoly(n,d),levels=0:39))))
  names(result)<-c("position","prob")
  result
}
#calculate the probabilities for 3,4,5,6-sided dice.
sides6<-estimate_monopoly(10000,6)
sides5<-estimate_monopoly(10000,5)
sides4<-estimate_monopoly(10000,4)
sides3<-estimate_monopoly(10000,3)
#get 3 most likely squares to end a turn on when using a 6-sided dice
top_n(sides6,3)
#create a data frame for visualization of long-term
#probabities for 3,4,5,6-sided dice.
all_sides<-cbind(sides6,sides5[,2],sides4[,2],sides3[,2])
names(all_sides)<-c("position","sides6","sides5","sides4","sides3")
all_sides<-gather(all_sides,sides,probability,sides6:sides3)
all_sides$sides<-as.factor(all_sides$sides)
#create a plot that shows marginal difference for each sided dice.
ggplot(all_sides,aes(position,probability,color=sides,group=sides))+geom_line()+
  xlab("Landing Position")+ggtitle("Probability of Landing on Each Tile for Different Sided Dices")+
  scale_color_discrete(name="Sides",breaks=c("sides3","sides4","sides5","sides6"),
                      labels=c("3 sided", "4 sided","5 sided","6 sided"))

# 1.3
#create a array of 0 with length 10000 to store probability of landing on jail
prop_jail<-rep(0,10000)
for(i in 1:1000){
  #store probability of landing on jail
  prop_jail[i]<-estimate_monopoly(10000,6)[10,2]
}
#calculate the standard error
sd(prop_jail)

# 2.1
# I use sub-function check to support simulate_monoply2
# The check function calculate how much should the player pay.
check<-function(position,p_index,p_rent){
  wealth=0
  position<-as.character(position)
  #check if the player need to pay tax
  if (position %in% t){
    switch(position,
           "4"={
             wealth=-200
           },
           "38"={
             wealth=-100
           })
  #check if the palyer need to pay rent
  }else if(position %in% p_index){
    wealth=-p_rent[which(p_index==position)]
  }
  wealth
}
#a modified version of simulate_monoply
simulate_monopoly2<-function(n,d,p_index,p_rent){
  position<- rep(0,n+1)
#the wealth array store the money gain or lost by the end of each turn
  wealth<-rep(0,n+1)
  for (i in 2:(n+1)){
    dice1<-sample(1:d,1)
    dice2<-sample(1:d,1)
    dice_output<-dice1+dice2
    if (dice1==dice2){
      double<-double+1
    }else{
      double<-0
    }
    if (double==3){
      position[i]<-j
      double<-0
    }else{
      #only the players don't ending on jail, they can collect "go" money
      if (position[i-1]+dice_output>39&&(position[i-1]+dice_output)%%40!=j){
        wealth[i]=wealth[i]+200
      }
      position[i]<-(position[i-1]+dice_output)%%40
      position[i]<-lookup(position[i]) 
      #calling check to see how much should the player pay.
      wealth[i]<-wealth[i]+check(position[i],p_index,p_rent)
    }
  }
  #combine the landing position and net wealth of each turn into one variable
  result<-cbind(position,wealth)
  rownames(result)<-0:n
  result
}



#2.2
#read in the data "properties.csv"
property<-read.csv("properties.csv")
#get a set of colornames
colorsnames<-as.character(unique(property$Color))
#create a list that can be used to store the result of 1000 simulations for each color.
wealth_color<-lapply(colorsnames,function(x){assign(x,rep(0,1000))})
names(wealth_color)<-unique(property$Color)
#separate the property by colors and store in list 
colors<-lapply(unique(property$Color),function(x){property[which(property$Color==x),]})
names(colors)<-unique(property$Color)
#store the simulation results in wealth_color.
for (i in 1:length(colors)){
  for( k in 1:1000){
    wealth_color[[i]][k]<-sum(simulate_monopoly2(100,6,colors[[i]]$Index,colors[[i]]$Rent)[,2])
  }
}
#convert the wealth_color into a dataframe for plot
result<-as.data.frame(wealth_color)
#correct the column name for "light blue"
colnames(result)[2]<-"Light Blue"
#clean the data, so each row has only one observation
result<-gather(result,color,gain_lost,Purple:Blue)
#get correct color for boxplot
boxplot_color<-levels(as.factor(result$color))
#create a boxplot to visualize the result.
ggplot(result,aes(color,gain_lost))+geom_boxplot(color=boxplot_color)+
  labs(title="Total Money Gained or Lost with Different Colored Properties",
       x="Color",y="Gained or Lost")


#2.3
#simulate_monopoly3 is a modified version f simulate_monopoly2
#In this function, I include a varaible pay_rent which indicates
#how much the player pay to his opponent at each turn.
simulate_monopoly3<-function(n,d,p_index,p_rent){
  #new varaible pay_rent
  pay_rent<-rep(0,n+1)
  position<- rep(0,n+1)
  wealth<-rep(0,n+1)
  for (i in 2:(n+1)){
    dice1<-sample(1:d,1)
    dice2<-sample(1:d,1)
    dice_output<-dice1+dice2
    if (dice1==dice2){
      double<-double+1
    }else{
      double<-0
    }
    if (double==3){
      position[i]<-j
      double<-0
    }else{
      if (position[i-1]+dice_output>39&&(position[i-1]+dice_output)%%40!=j){
        wealth[i]=wealth[i]+200
      }
      position[i]<-(position[i-1]+dice_output)%%40
      position[i]<-lookup(position[i]) 
      wealth[i]<-wealth[i]+check(position[i],p_index,p_rent)
      #calculate how much the player needs to pay,
      #if the player lands on his opponent's property. 
      if (position[i]%in%p_index){
        pay_rent[i]<-p_rent[which(p_index==position[i])]
      }
    }
  }
  #return pay_rent along with the original result.
  result<-cbind(position,wealth,pay_rent)
  rownames(result)<-0:n
  colnames(result)[3]<-"lost"
  result
}
#read in color_combos
color_combos<-read.csv("color_combos.csv")
property<-read.csv("properties.csv")
#convert all elements in color_combos into character. 
color_combos<-mutate_all(color_combos,as.character)
#separate the dataframe property by colors and store in list 
colors<-lapply(unique(property$Color),function(x){property[which(property$Color==x),]})
names(colors)<-unique(property$Color)
#The get_win_lost function simulate 100 head-to-head mathcups for each of 28 combinations of colors
#and return a table that shows win and lost for each color with number of turns n.
get_win_lost<-function(n){
  #create a dataframe that shows number of win and lost for each color 
  win_lost<-data.frame(matrix(ncol = 4, nrow = 8))
  colnames(win_lost)<-c("color","win","lost","win %")
  win_lost[,1]<-as.character(unique(property$Color))
  win_lost[,2:4]<-0
  #This for loop run 100 simulations for each of 28 pairwise combinations of colors, 
  #and assigns the number of wins for each color into the second column of win_lost
  for(i in 1:100){
    #two_player is a list of 8 game outputs,which each game is a player play agianst one color.
    two_player<-lapply(colors,function(x){simulate_monopoly3(n,6,x$Index,x$Rent)})
    #I match these 8 game outputs to form 28 conbinations
    for(k in 1:nrow(color_combos)){
      #calculate the total wealth for each player
      color1_wealth<-sum(two_player[[color_combos[k,2]]][,2])+sum(two_player[[color_combos[k,1]]][,3])+5000
      -colors[[color_combos[k,1]]][1,4]
      color2_wealth<-sum(two_player[[color_combos[k,1]]][,2])+sum(two_player[[color_combos[k,2]]][,3])+5000
      -colors[[color_combos[k,2]]][1,4]
      #update the number of wins for winner
      if (color1_wealth>color2_wealth){
        win_lost[which(win_lost$color==color_combos[k,1]),2]<- win_lost[which(win_lost$color==color_combos[k,1]),2]+1
      }else{
        win_lost[which(win_lost$color==color_combos[k,2]),2]<- win_lost[which(win_lost$color==color_combos[k,2]),2]+1
      }
    }
  }
  #calculate the lost for each color
  win_lost[,3]<-700-win_lost[,2]
  #calculate the win percentage for each color
  win_lost[,4]<-scales::percent(win_lost[,2]/700)
  #output the final result
  win_lost
}
get_win_lost(25)
get_win_lost(50)
get_win_lost(100)


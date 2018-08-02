#a script for understanding how populations behave
#when adjusting parameters that make up their dynamic rule- ie
#their carrying capacity, k, and intrinsic rate of increase r


#first, let's create a vector with numbers to repesent alpossible values up to some maximum population-, this will serve as
#our N(t) vector-ie the population of an organism on day t

#set maximum population
maxpop<-3000
Nt<-(1:maxpop)

#set carrying capacity
k<-2000
#set intrinsic rate of increase
r<-2.3

#Ricker model predicts population at next time step Nt1 using Nt, r and k
Nt1<- Nt*exp(r*(1- Nt/k))

plot(Nt, Nt1)

#cool, so now we have a plot, but let's say we want to compare the behavior easily
#let's write a function:

ricker<-function(N, r, k){
  Nt1<- Nt*exp(r*(1- Nt/k))
  return(Nt1)
}

lower.r<-ricker(Nt, r-1, k)
lower.k<-ricker(Nt, r, k-10)
higher.r<-ricker(Nt, r+1, k)
higher.k<-ricker(Nt, r, k+10)


#pack responses together
responses<-as.data.frame(cbind(Nt, Nt1, lower.r, higher.r, lower.k, higher.k))
#reshape the data to list form

library(reshape2)
simulation<-melt(responses, id=1)

#plot 'em

library(ggplot2)

gg<-ggplot(simulation, aes(x=Nt, y=value, color=variable))+
  geom_point()+
  geom_line()+
  scale_x_continuous(name="N(t)")+
  scale_y_continuous(name="N(t+1)")
gg


#timeseries

steps<-25
years<-(1:25)
Num<-c()
Nt1<-maxpop

for (i in 1:steps){
  Num<-c(Num, Nt1)
  Nt1<- Nt1*exp(r*(1- Nt1/k))
  
}

timeseries<-as.data.frame(cbind(years, Num))


gg2<-ggplot(timeseries, aes(x=years, y=Num))+
  geom_point()+
  geom_line()+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="N(t)")
gg2



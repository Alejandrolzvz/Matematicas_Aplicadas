library(Hmisc)
library(rriskDistributions)
library(fitdistrplus)
library(bootstrap)
library(Bolstad)
data<- read.csv(file = "C:/Users/Alejandro/Documents/1 ITESM/MCI/2do semestre/Nueva carpeta/delivery 8/contamination.csv")
names(data)
plot(data, main="Phosphorus contamination")
fit.cont(data$Phosphorus.contamination)
quantile(data$Phosphorus.contamination, c(.025, .5, .975)) 
get.norm.par(p= c(.025, .5, .975), q =c(0.072575, 0.0955, 0.114425),tol = 0.001)

op<-data$Operator
wo<-data$Workshift
hf<-data$High.furnace
gr<-data$Group
pr<-data$Proportion.of.coke.mineral.vs.iron
t1<-data$Temperature.of.arrival.to.zone.1
t2<-data$Temperature.of.arrival.to.zone.2
as<-data$Air.feeding.speed
pc<-data$Phosphorus.contamination
linmod<-glm(pc~t2,data=data, family="binomial")
summary(linmod)
p<-predict(linmod, type="response")
p


#prior
plot(density(pc))
#likelihood
ll.normal<-function(data,par){
  return(-sum(log(dnorm(data,mean=par[1],sd=par[2]))))
}
op<-optim(par=c(1,0.1), fn=ll.normal, data=data$Phosphorus.contamination)
plot(density(data$Phosphorus.contamination), main="Prior, likelihood and Posterior")
abline(v=mean(pc),col="green")
x<-seq(0,0.15,0.001)
lines(x,dnorm(x,mean=op$par[1],sd=op$par[2]), col="red", lty=2)
lines(x,dnorm(x,mean=op$par[1],sd(pc)), col="blue", lty=3)

#CRLB
m = mean(pc)
std = sd(pc)
n = 64
Data = rnorm(n = n,mean = m, sd = std)
#tetahat = mlecauchy(Data)
tetahat = optimize(function(miu)-sum(dnorm(Data,mean =  miu, sd = std ,log = TRUE)), c(-10,10))

bias = NA
for (i in 1:1000){
  Data = rnorm(n = n,mean = m, sd = std)
  #tetahat = mlecauchy(Data)
  tetahat = optimize(function(mean)-sum(dnorm(Data,mean = mean, sd = std ,log = TRUE)), c(-10,10))
  bias[i]= round((m-tetahat$minimum)^2, digits = 15)
}

plot(sort(bias))
h<- std^2/n
abline(h=h, col="red")
h

#JS 
n=length(pc)
Data=rnorm(n,mean(pc),sd(pc))
fitdist(Data,dnorm)
MLE=mean(pc)

#JS= x_bar+B(x-x_bar)
#B=1-(n-3) sig/sum(sum(x-x_bar)^2)

B= 1-((n-3)*var(Data))/sum((Data-MLE)^2)
JS=MLE+B*sum(Data-MLE)
plot(JS)

james.stein(data$Phosphorus.contamination,group = data$High.furnace.purge.time)
#regression

library(MLmetrics)
data_reg <- data[,!names(data) %in% c("High.furnace", "Operator", "Workshift", "Group", "Date", "Proportion.of.coke.mineral.vs.iron", "Temperature.of.arrival.to.zone.1")]
str(data_reg)
sum(is.na(data_reg))
logita<-glm(Phosphorus.contamination~.,data=data_reg, family = "gaussian")
logita
summary(logita)
c<-sort(logita$coefficients)
c
prediction = predict(object = logita, type = "response")
prediction

MSE(prediction, data$Phosphorus.contamination)
R2_Score(prediction, data$Phosphorus.contamination)


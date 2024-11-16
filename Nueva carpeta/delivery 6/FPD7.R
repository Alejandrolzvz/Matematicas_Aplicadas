library(dplyr)
library(tidyverse)
#Read csvs
CP<-read.csv("C:/Users/benja/Downloads/CP.csv",  header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
Tool<-read.csv("C:/Users/benja/Downloads/Tool.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")

#make Tool.cvs a data frame
data_tool<- as.data.frame(Tool) 
#Transform "NA" in tool to 0 
data_tool[is.na(data_tool)] <- 0
#transform tool of each stage to 1 if it is used
for (i in 2:101) {
  data_tool[,i]<-as.numeric(as.character(data_tool[,i]))
  data_tool[is.na(data_tool)] <- 1 
}
#make CP.cvs a data frame
data_cp<- as.data.frame(CP) 
#Transform "NA" in cp to 0 
data_cp[is.na(data_cp)] <- 0

#verify if there is a CP value for corresponding from data_tool to data_cp
y<-data_tool$CP
x<-data_cp$X
z<-x[!(x %in% y)] #find the values of Lot in CP that are not present in Tool
z
a<-match(z,x) #find the lines of the values not present in Tool
a
data_cp<- data_cp %>% slice(-a)
data_cp

w<-y[!(y %in% x)] #find the values of Lot in Tool that are not present in CP
w
b<-match(w,y) #find the lines of the values not present in CP
b
data_tool<- data_tool %>% slice(-b) 
data_tool
data_tool$CP<-data_cp$CP #change values of CP from cp to tool

data_tool$CP[data_tool$CP==0]<-NA

data_tool <- na.omit(data_tool)
#Parametrize data to obtain vales between 0 and 1
data_tool$CP<-data_tool$CP/max(data_tool$CP)
#sort data to obtain max values of CP and min values of CP 
sort(data_tool$CP)
m<-median(data_tool$CP)
m #median value of the whole CP less than m low CP more than m high CP
dfh<-data_tool[data_tool$CP>m,] #high values
dfl<-data_tool[data_tool$CP<m,] #low values
#logistic regression high CP values 
str(dfh)
sum(is.na(dfh))
logit<-glm(CP~.,data=dfh)
logit
summary(logit)
a<-sort(logit$coefficients)
a
tail(a,11) #show the beta values of the 10 variables that most affect CP value to be high
#logistic regression low CP values 
str(dfl)
sum(is.na(dfl))
logitl<-glm(CP~.,data=dfl)
logitl
summary(logitl)
b<-sort(logitl$coefficients)
b
head(b,10)#show the beta values of the 10 variables that most affect CP value to be low

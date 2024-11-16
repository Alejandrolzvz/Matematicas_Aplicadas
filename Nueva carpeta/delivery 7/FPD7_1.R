library(dplyr)
library(tidyverse)
#Read csvs
CP<-read.csv("C:/Users/Alejandro/Documents/1 ITESM/MCI/2do semestre/Nueva carpeta/delivery 7/CP.csv",  header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
Tool<-read.csv("C:/Users/Alejandro/Documents/1 ITESM/MCI/2do semestre/Nueva carpeta/delivery 7/Tool.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")

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
observed = data_tool$CP
maximo = max(data_tool$CP)
data_tool$CP<-data_tool$CP/maximo

str(data_tool)
sum(is.na(data_tool$CP))
logita<-glm(CP~.,data=data_tool, family = "binomial")
logita
summary(logita)
c<-sort(logita$coefficients)
c

# #sort data to obtain max values of CP and min values of CP 
# sort(data_tool$CP)
# m<-median(data_tool$CP)
# m #median value of the whole CP less than m low CP more than m high CP
# dfh<-data_tool[data_tool$CP>m,] #high values
# dfl<-data_tool[data_tool$CP<m,] #low values
# #logistic regression high CP values 
# str(dfh)
# sum(is.na(dfh))
# logit<-glm(CP~.,data=dfh)
# logit
# summary(logit)
# a<-sort(logit$coefficients)
# a
# tail(a,11) #show the beta values of the 10 variables that most affect CP value to be high
# #logistic regression low CP values 
# str(dfl)
# sum(is.na(dfl))
# logitl<-glm(CP~.,data=dfl)
# logitl
# summary(logitl)
# b<-sort(logitl$coefficients)
# b
head(c,10)#show the beta values of the 10 variables that most affect CP value to be low
tail(c,11)#show the beta values of the 10 variables that most affect CP value to be high

#Model with only the 20 variables that affect the most

data_tool_2  <- select(data_tool, stage045, stage061, stage056, stage038, stage043, stage089, stage094, stage081, stage090, stage008, stage002, stage040, stage055, stage068, stage051, stage058, stage088, stage031, stage013, stage057, CP)

str(data_tool_2)
sum(is.na(data_tool_2$CP))
logita<-glm(CP~.,data=data_tool_2, family = "binomial")
logita
summary(logita)
c<-sort(logita$coefficients)
c

prediction = predict(object = logita, type = "response")

response = prediction*maximo
response

chi_sqrd= sum(((observed-response)^2)/response)
chi_sqrd

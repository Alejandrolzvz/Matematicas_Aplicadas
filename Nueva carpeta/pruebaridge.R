library(data.table)
library(glmnet)
library(ggplot2)
library(modelr)
library(base)
library(dplyr)
#Read csvs
CP<-read.csv("C:/Users/Alejandro/Documents/1 ITESM/MCI/2do semestre/Nueva carpeta/delivery 6/CP.csv",  header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
Tool<-read.csv("C:/Users/Alejandro/Documents/1 ITESM/MCI/2do semestre/Nueva carpeta/delivery 6/Tool.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
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

data_tool <- na.omit(data_tool)
boxplot(data_tool$CP) #check for outliers

data_tool$CP[data_tool$CP==0]<-NA

data_tool <- na.omit(data_tool)

plot(data_tool$CP,data_tool$stage001)
boxplot(data_tool$CP) #zero outliers and range been reduced

cp<-data_tool$CP
x <- select(data_tool, stage045, stage061, stage056, stage043, stage038, stage094, stage089, stage081, stage090, stage008, stage002, stage040, stage055, stage068, stage051, stage058, stage088, stage013, stage031, stage057)
x <- as.matrix(x)
x

set.seed(100)
lasso_model=cv.glmnet(x, cp, lambda = 10^seq(2, -3, by = -.1), alpha=1)
lasso_model$lambda.1se
plot(lasso_model)
best_lambda=lasso_model$lambda.1se
lasso_coef=lasso_model$glmnet.fit$beta[,lasso_model$glmnet.fit$lambda==best_lambda]

set.seed(100)
ridge_model=cv.glmnet(x, cp, lambda = 10^seq(2, -3, by = -.1), alpha=0)
ridge_model$lambda.1se
plot(ridge_model)
best_lambda=ridge_model$lambda.1se
ridge_coeff=ridge_model$glmnet.fit$beta[,ridge_model$glmnet.fit$lambda==best_lambda]







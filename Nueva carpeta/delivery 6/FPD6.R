library(MASS)
library(stats)
library(datos)
library(tidyverse)
library(broom)
library(graphics)
library(dplyr)
library(data.table)
library(metafor)
#Read csvs
CP<-read.csv("C:/Users/benja/Downloads/CP.csv", na.strings = "NA")
Tool<-read.csv("C:/Users/benja/Downloads/Tool.csv")

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

sort(data_tool$CP) #sort from lowe ro higher to select 10 lower and 10 higher CP values








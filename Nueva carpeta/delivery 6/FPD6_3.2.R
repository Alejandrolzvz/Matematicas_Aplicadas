library(MASS)
library(stats)
library(datos)
library(tidyverse)
library(broom)
library(graphics)
library(dplyr)
library(data.table)
library(metafor)
library(glmnet)

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
plot(data_tool$CP,data_tool$stage001)
boxplot(data_tool$CP) #check for outliers

data_tool$CP[data_tool$CP==0]<-NA

data_tool <- na.omit(data_tool)

plot(data_tool$CP,data_tool$stage001)
boxplot(data_tool$CP) #zero outliers and range been reduced

sort(data_tool$CP)
observed<- data_tool$CP

# ###################################
# 
# 
# 
# x <- select(data_tool, -c(CP))
# x <- as.matrix(x)
# 
# y = data_tool %>%
#   select(CP) %>%
#   unlist() %>%
#   as.numeric()
# 
# grid = 10^seq(10, -2, by = -.1)
# # cv_glm = cv.glmnet(x, y, family ="gaussian", alpha = 0, nlambda = 100, lambda = grid)
# # lambda_opt <- cv_glm$lambda.min
# # 
# # dim(coef(cv_glm))
# # plot(cv_glm)    # Draw plot of coefficients
# 
# ridge_mod = glmnet(x, y, family ="gaussian", alpha = 0, lambda = grid)
# dim(coef(ridge_mod))
# plot(ridge_mod)    # Draw plot of coefficients
# 
# predict(ridge_mod, s = 50, type = "coefficients")[1:20,]
# 
# #trainning
# set.seed(1)
# 
# train = data_tool %>%
#   sample_frac(0.5)
# 
# test = data_tool %>%
#   setdiff(train)
# 
# x_train = model.matrix(CP~., train)[,-1]
# x_test = model.matrix(CP~., test)[,-1]
# 
# y_train = train %>%
#   select(CP) %>%
#   unlist() %>%
#   as.numeric()
# 
# y_test = test %>%
#   select(CP) %>%
#   unlist() %>%
#   as.numeric()
# 
# #fitting the moddel
# ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)
# set.seed(1)
# cv.out = cv.glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on training data
# bestlam = cv.out$lambda.min  # Select lamda that minimizes training MSE
# bestlam
# 
# plot(cv.out)
# 
# ridge_pred = predict(ridge_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
# mean((ridge_pred - y_test)^2) # Calculate test MSE
# 
# out = glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
# c<-predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
# dim(coef(out))
#   
# c<-sort(c)
# c
# 
# head(c,10)
# tail(c,11)
# ################################

 
x <- select(data_tool, stage045, stage061, stage038, stage089, stage056, stage043, stage094, stage090, stage081, stage008, stage040, stage017, stage068, stage055, stage051, stage058, stage088, stage013, stage031, stage057)
x <- as.matrix(x)

y = data_tool %>%
  select(CP) %>%
  unlist() %>%
  as.numeric()

grid = 10^seq(10, -2, by = -.1)
# cv_glm = cv.glmnet(x, y, family ="gaussian", alpha = 0, nlambda = 100, lambda = grid)
# lambda_opt <- cv_glm$lambda.min
#
# dim(coef(cv_glm))
# plot(cv_glm)    # Draw plot of coefficients

ridge_mod = glmnet(x, y, family ="gaussian", alpha = 0, lambda = grid)
dim(coef(ridge_mod))
plot(ridge_mod)    # Draw plot of coefficients

predict(ridge_mod, s = 50, type = "coefficients")[1:20,]

#trainning
set.seed(1)

train = data_tool %>%
  sample_frac(0.5)

test = data_tool %>%
  setdiff(train)

x_train = model.matrix(CP~., train)[,-1]
x_test = model.matrix(CP~., test)[,-1]

y_train = train %>%
  select(CP) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(CP) %>%
  unlist() %>%
  as.numeric()

#fitting the moddel
ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)
set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on training data
bestlam = cv.out$lambda.min  # Select lamda that minimizes training MSE
bestlam

plot(cv.out)

ridge_pred = predict(ridge_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
mean((ridge_pred - y_test)^2) # Calculate test MSE

out <- glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
dim(coef(out))
plot(out)    # Draw plot of coefficients

#######3
train = data_tool %>%
  sample_frac(0)

test = data_tool %>%
  setdiff(train)

x_train = model.matrix(CP~., train)[,-1]
x_test = model.matrix(CP~., test)[,-1]



response <- predict(ridge_mod, s = bestlam, newx = x_test)
response

chi_sqrd= sum(((observed-response)^2)/response)
chi_sqrd


##########################################

#Alternative fitting
# ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)
# m_max <-0
# lambda_opt <-0
# for (i in 1:401){
#   ridge_pred = predict(ridge_mod, s = i, newx = x_test)
#   m <- mean((ridge_pred - y_test)^2)
#   m
#   if (m > m_max){
#     m.max <- m
#     lambda_opt<-i
#   }
# }
# m.max
# lambda_opt
# 
# ridge_pred = predict(ridge_mod, s = 0, newx = x_test, exact = T)
# m <- mean((ridge_pred - y_test)^2)
# m


# norma=sqrt(sum(coef(ridge_mod)))
# 
# ridge_reg = glmnet(x, y, family ="gaussian", alpha = 0,nlambda = 100, lambda = grid, upper.limits = norma, lower.limits = 0)
# 
# dim(coef(ridge_reg))
# plot(ridge_reg)    # Draw plot of coefficients

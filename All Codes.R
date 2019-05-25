library(ISLR)
A = data.frame(WAGE)
ISLR::Wage


A = data.frame(WAGE)


library(caret)
library(glmnet)
i = 0
while (i < 20){
  if (i %% 2 == 0){
    cat ("E",+i)
  i = i + 1 }
  }

i = 0
while (i < 20){
  if (i %% 2 == 0){
    cat("Even", +i)
    i = i+1
  }
  else if (i %% 2 == 1 ){
    cat("odd", +i)
    i = i+1
  }
} 











#-------------------for iris data------------------------#

library(ISLR)
library(naivebayes)
library(e1071)

A = data.frame(iris)
str(A)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,.3))
trd = A[sf==1,]
tsd = A[sf==2,]

model_nb = naive_bayes(Species ~ .,data = trd)

model_e1 = naiveBayes(Species ~ .,data = trd)

pred_nb = predict(model_nb,tsd)
pred_nb
table(pred_nb,tsd$Species)

pred_e1 = predict(model_e1,tsd)
pred_e1
table(pred_e1,tsd$Species)

#----------------------another Datast--------------------------------------#

A = data.frame(Credit)
str(A)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,.3))
trd = A[sf==1,]
tsd = A[sf==2,]

model_nb = naive_bayes(Ethnicity ~ .,data = trd)

model_e1 = naiveBayes(Ethnicity ~ .,data = trd)

pred_nb = predict(model_nb,tsd)
pred_nb
table(pred_nb,tsd$Ethnicity)



model_nb = naive_bayes(Married ~ .,data = trd)

model_e1 = naiveBayes(Married ~ .,data = trd)

pred_nb = predict(model_nb,tsd)
pred_nb
table(pred_nb,tsd$Ethnicity)

pred_e1 = predict(model_e1,tsd)
pred_e1
table(pred_e1,tsd$Ethnicity)


library(MASS)
A = data.frame(Cars93)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,.3))
trd = A[sf==1,]
tsd = A[sf==2,]

model_nb = naive_bayes(AirBags ~ .,data = trd)
pred_nb = predict(model_nb,tsd)
pred_nb
















library(ISLR)
A = data.frame(Credit)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
tsd = A[sf==1,]
trd = A[sf==2,]
#-----------------imple Model-----------------------------------
m1 = lm(Limit ~ Income + Rating + Balance, data = A)
pred = predict(m1,tsd)
#------------------Ridge model---------------------------------

tc = traincontrol(Method = 'cv',number = 10,verboseIter = T)

library(ISLR)
library(iris)

A = data.frame(iris)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]
str(A)
model1 = lm(Sepal.Length ~ .,data = A)




library(ggplot2)
 a =c(21,19,39,50,60,55,78,69,90,110,111,120,130,141)
 b =c(32,46,38,47,40,48,67,50,40,52,65,74,85,79)
 
 f =data.frame(a,b)
 
 ggplot(f,aes(y=b,x=a)) +
   geom_point(alpha = .9) +
   stat_smooth(method = "lm",formula = y ~ I(x^2))






summary(model1)





A = data.frame(Auto)
A1=na.omit(A)

sf =sample(2,nrow(A1),replace = TRUE,prob = c(0.7,03))
trd =A1[sf==1,]
tsd =A1[sf==2,]
str(A1)
pairs.panels(A)
cor(A[,1:8])


model1 = lm(displacement ~ cylinders,data=trd)

summary(model1)

pred =predict(model1,tsd)


CMP =cbind(tsd$displacement,pred)


plot(trd$cylinders,trd$displacement)

abline(-109.035,53.738)




library(MASS)
library(ISLR)
library(psych)
        
A =data.frame(Hitters)
sf =sample(2,nrow(A),replace = TRUE,prob = c(0.8,0.2))
trd = A[sf==1,]
tsd = A[sf==2,]

str(A)
cor(A[,c(-20,-14,-15)])


# prediction model for AtBAt

model1 =lm(Atbat ~ .,data = trd)
summary(model1)


sal =750000
if(sal <=250000) print(0) else 
  print ("tax")


library(glmnet)
library(ISLR)

A = data.frame(Credit)
A = na.omit(A)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]
cor(A[,c(2,3,4,5,6,12)])
#----------SIMPLE MODEL----------
m1 = lm(Limit ~ Income + Rating + Balance,data = trd)

pred = predict(m1,tsd)

#===========RIDGE MODEL=============

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A1 = na.omit(A)

#--------------------Classification-------------------------#
library(glmnet)
library(ISLR)
str(ISLR::Credit)
A =data.frame(Credit)
fivenum(A$Income)

A$NEWINCOME = ifelse(A$Income >33.11,1,0)
sf = sample(2,nrow(A),replace = TRUE,prob = )
library(tree)

A = data.frame(iris)

sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))

trd = A[sf==1,]
tsd = A[sf==2,]

m1 = tree(Species ~ . ,data = trd)
summary()

plot(m1)
text(m1)

pred_T = predict(m1,tsd)
pred_T
table(tsd$Species)

Q = ifelse(pred_T[,1] > 0.5,"Setosa",ifelse(pred_T[,2] > 0.5,"versicolor","virginica"))

Q1 = table(Q,tsd$Species)

sum(diag(Q1))
sum(diag(Q1))/nrow(tsd)
sum(diag(Q1))/nrow(tsd)*100

library(ISLR)

A = data.frame(Credit)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]

m1 = lm(Income ~ .,data = trd)
pred = predict(m1,trd)
summary(m1)

cmp = cbind(pred,tsd$Income)


model_T = tree(Income ~ .,data = trd)
plot(model_T)
text(model_T)

summary(model_T)
library(naivebayes)
m2 = naive_bayes(Ethnicity ~ .,data = trd)

pred1 = predict(m2,tsd)
cbind(pred1,tsd$Ethnicity)
table(pred1,tsd$Ethnicity)

######################Random Forest ##################################

library(randomForest)
library(ISLR)
library(tree)
set.seed(243)
A = data.frame(iris)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,03))
trd = A[sf==1,]
tsd = A[sf==2,]
model_dt = tree(Species ~ .,data = trd)
model_rf = randomForest(Species ~ .,data = trd)

pred_dt = predict(model_dt,tsd)
pred_dt
pred_rf = predict(model_rf,tsd)
pred_rf
pred_rf = predict(model_rf,tsd,type = "prob")

Q = ifelse(pred_dt[,1] > 0.5,"Setosa",ifelse(pred_dt[,2] > 0.5,"Versicolor","Virginica"))
table(Q,tsd$Species)

Q1 = ifelse(pred_rf[,1] > 0.5,"Setosa",ifelse(pred_rf[,2] > 0.5,"Versicolor","Virginica"))
table(Q1,tsd$Species)

?prune.tree

library(MASS)
A =data.frame(Cars93)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,03))
trd = A[sf==1,]
tsd = A[sf==2,]

str(Cars93)

model_dt = tree(AirBag ~ .,data = trd)
model_rf = randomForest(Horsepower ~ .,data = trd)

############################KNN Model ##############################################


library(caret)
library(ISLR)
A = data.frame(Hitters)

A = na.omit(A)
colnames(A)
str(A)
set.seed(123)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]

tc =trainControl(method = "cv",number = 10)
set.seed(123)

model1 = train(League ~ . ,
               data = trd,
               method = 'knn',
               trControl = tc,
               preProc = c("center","scale"))
PR = predict(model1,tsd$League)

while (i < 20){
  if (i %% 2 == 0) print("\nEven")
    i = i+1
  else if (i %% 2 == 1 ) print("\nodd")
    i = i+1
} 

taxcalcm = if (sal <= 250000) print("No tax") else
  if (sal > 250000 && sal <= 500000) print ((sal - 2500000)*0.1) else 
    if (sal > 500000 && sal <= 1000000) print(25000 + (sal - 500000)*0.2) else 
      print(25000 + 100000 + ((sal- 100000 )*0.3))

                               

##############################Kmeans for credit data frame #################################

A1 = data.frame(ISLR::Credit)

A = na.omit(A1[,c(2:7,12)])
m1 = kmeans(A,3)
plot(A$Age,A$Income,col = m1$cluster)

plot(A$Age,A$Rating,col = m1$cluster)
points(x =m1$centers[,5],y=m1$centers[,3],col = 2,pch = 2,cex =1,lwd = 4)


###################with caret###############################################

library(caret)
dmy = dummyVars("~.",data = iris)
newiris = data.frame(predict(dmy,newdata = iris))




############################### 12-may - Hierarchical Clustering#####################################
library(data.table)
A = data.frame(iris)

model1 = hclust(dist(A),method = "complete")

plot(model1,main = "Heirarchical",xlab = "leaf of dendorgram",sub = "",cex = .9)

model1$height


################## Missing  VAlues ######################################

A = data.frame(iris)

A[4,1] = NA
A[8,1] = NA
A[10,1] = NA
A[2,2] = NA
A[7,2] = NA
A[5,3] = NA
A[9,3] = NA
psych::pairs.panels(A)
trd = A[is.na(A$Petal.Length) ==FALSE,] 
tsd = A[is.na(A$Petal.Length) ==TRUE,] 

model = lm(Petal.Length ~ Petal.Width,data = trd)
mv = predict(model,tsd)
mv
A[is.na(A$Petal.Length) ==TRUE,3] = mv
A[1:10,]
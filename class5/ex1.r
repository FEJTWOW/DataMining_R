library(readr)
library(dplyr)
library(corrplot)
library(lubridate)
library(DataCombine)
library(psych)
library(class)
library(naivebayes)

str(iris)

head(iris)

summary(iris)

table(iris$Species)

cor(iris[-5])
pairs.panels(iris[-5])
boxplot(Sepal.Length~Species,iris)
boxplot(Sepal.Width~Species,iris)
boxplot(Petal.Length~Species,iris)
boxplot(Petal.Width~Species,iris)

stand = function(x) { (x-mean(x))/(sd(x)) }

iris_std = as.data.frame(lapply(iris[,c(1,2,3,4)],stand ))
iris_std_sp = data.frame(iris_std, iris$Species)

iris_std_sp

hist(iris$Sepal.Length)

hist(iris$Sepal.Width)

hist(iris$Petal.Width)

hist(iris$Petal.Length)



sets = sample(1:nrow(iris), 0.75 * nrow(iris))

train_ir=iris_std_sp[sets,] 
test_ir=iris_std_sp[-sets,] 

train_ir_class=iris[sets,5] 
test_ir_class=iris[-sets,5] 
test_ir_class
length(train_ir)
length(test_ir_class)

model_ir3 = knn(train = train_ir[, 1:4], test = test_ir[,1:4], cl = 
                   train_ir_class, k=3) 
t_ir3=table(test_ir_class, model_ir3) 
acc_ir3=mean(test_ir_class == model_ir3) 
table(test_ir_class, model_ir3) 

model_ir5 = knn(train = train_ir[, 1:4], test = test_ir[,1:4], cl = 
                   train_ir_class, k=5) 

t_ir5=table(test_ir_class, model_ir5) 
acc_ir5=mean(test_ir_class == model_ir5) 

model_ir7 = knn(train = train_ir[, 1:4], test = test_ir[,1:4], cl = 
                   train_ir_class, k=7) 

t_ir7=table(test_ir_class, model_ir7) 
acc_ir7=mean(test_ir_class == model_ir7)

nb_ir1 = naive_bayes(iris.Species ~ ., train_ir) 
summary(nb_ir1)

nb_ir1_pred=predict(nb_ir1, test_ir[,-5], type = "class") 
nb_ir1_pred_prob=predict(nb_ir1, test_ir[,-5], type = "prob") 
hist(nb_ir1_pred_prob)

get_cond_dist(nb_ir1)

t_nb_ir1<-table(test_ir[,5], nb_ir1_pred) 
acc_nb_ir1<-mean(test_ir[,5] == nb_ir1_pred)

nb_ir2 <- naive_bayes(iris.Species ~ ., train_ir, prior=c(0.1, 0.1, 0.8)) 
summary(nb_ir2)

nb_ir2_pred<-predict(nb_ir2, test_ir[,-5], type = "class") 
nb_ir2_pred_prob<-predict(nb_ir2, test_ir[,-5], type = "prob")
hist(nb_ir2_pred_prob)
nb_ir2_pred_prob
get_cond_dist(nb_ir2)

t_nb_ir2<-table(test_ir[,5], nb_ir2_pred) 
acc_nb_ir2<-mean(test_ir[,5] == nb_ir2_pred) 

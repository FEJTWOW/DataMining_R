library(readr)
library(dplyr)
library(corrplot)
library(lubridate)
library(DataCombine)
library(psych)
library(class)
library(naivebayes)
library(pROC)


head(iris)
tail(iris)
summary(iris)

stand = function(x) { (x -mean(x))/(sd(x)) } 
iris_std = as.data.frame(lapply(iris[,c(1,2,3,4)],stand )) 

sets = sample(1:nrow(iris), 0.96 * nrow(iris))  
train_ir = iris_std[sets,] 
test_ir = iris_std[-sets,] 

?dist

dist_mat_c = dist(train_ir, method ='canberra')
dist_mat_e = dist(train_ir, method = 'euclidean') 

library(stats) 
m1_c_ir = hclust(dist_mat_c, method = 'average') 
m2_c_ir = hclust(dist_mat_c, method = 'complete') 
m3_c_ir = hclust(dist_mat_c, method = 'ward.D') 
m4_c_ir = hclust(dist_mat_c, method = 'single')

plot(m1_c_ir) 
abline(h = 3, col = 'red') 

plot(m2_c_ir) 
abline(h = 3, col = 'red')

plot(m3_c_ir) 
abline(h = 3, col = 'red')

plot(m4_c_ir) 
abline(h = 3, col = 'red')

cut_m1_c_ir = cutree(m1_c_ir, k = 2) 
cut_m2_c_ir = cutree(m2_c_ir, k = 4) 
cut_m3_c_ir = cutree(m3_c_ir, k = 2) 
cut_m4_c_ir = cutree(m4_c_ir, k = 3)

dunn(dist_mat_c, cut_m1_c_ir) 

dunn(dist_mat_c, cut_m2_c_ir)

dunn(dist_mat_c, cut_m3_c_ir)

dunn(dist_mat_c, cut_m4_c_ir)

table(cut_m3_c_ir)

#---------------------------------

dist_mat_m = dist(train_ir, method = 'manhattan')

library(stats) 
m1_m_ir = hclust(dist_mat_m, method = 'average') 
m2_m_ir = hclust(dist_mat_m, method = 'complete') 
m3_m_ir = hclust(dist_mat_m, method = 'ward.D') 
m4_m_ir = hclust(dist_mat_m, method = 'single')

plot(m1_m_ir) 
abline(h = 3, col = 'red') 

plot(m2_m_ir) 
abline(h = 3, col = 'red')

plot(m3_m_ir) 
abline(h = 3, col = 'red')

plot(m4_m_ir) 
abline(h = 3, col = 'red')

cut_m1_m_ir = cutree(m1_m_ir, h = 3.7) 
cut_m2_m_ir = cutree(m2_m_ir, h = 8) 
cut_m3_m_ir = cutree(m3_m_ir, k = 3) 
cut_m4_m_ir = cutree(m4_m_ir, k = 2)

dunn(dist_mat_e, cut_m1_c_ir) 

dunn(dist_mat_e, cut_m2_c_ir)

dunn(dist_mat_e, cut_m3_c_ir)

dunn(dist_mat_e, cut_m4_c_ir)

table(cut_m3_c_ir)

m4_e_ir = hclust(dist_mat_e, method = 'single') 
cut_m4_e_ir = cutree(m4_e_ir, k = 2)

table(cut_m4_e_ir)

#kmeans

set.seed(1707) 

k1_ir = kmeans(train_ir,centers=2, nstart = 20) 
str(k1_ir) 

table(k1_ir$cluster) 

dunn(clusters=k1_ir$cluster,Data=train_ir) 

set.seed(2137) 

k2_ir = kmeans(train_ir,centers=4, nstart = 210) 
str(k2_ir) 

table(k2_ir$cluster) 

dunn(clusters=k2_ir$cluster,Data=train_ir) 

set.seed(1337) 

k3_ir = kmeans(train_ir,centers=7, nstart = 137) 
str(k3_ir) 

table(k3_ir$cluster) 

dunn(clusters=k3_ir$cluster,Data=train_ir) 



results_clus_ir = mutate(results_clus_ir, k1_ir=k1_ir$cluster) 

plot(results_clus_ir$Sepal.Length, results_clus_ir$Petal.Width, 
     col = as.factor(results_clus_ir$k1_ir)) 




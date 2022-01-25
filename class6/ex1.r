library(readr)
library(dplyr)
library(corrplot)
library(lubridate)
library(DataCombine)
library(psych)
library(class)
library(naivebayes)
library(pROC)

#przypomnienie jak wygl�daj� dane (pe�na analiza by�a wykonana na poprzednich 
#zaj�ciach) 
head(iris)
tail(iris)
summary(iris)

#dane, kt�re przedstawiane s� na r�nych skalach lub maj� r�ny �redni poziom 
#zjawiska nale�y przekszta�ci�: standaryzacja, normalizacja min-max. 
#mo�na te� u�y� funkcji scale() 

stand = function(x) { (x -mean(x))/(sd(x)) } 
iris_std = as.data.frame(lapply(iris[,c(1,2,3,4)],stand )) 

#do zbioru ucz�cego prosz� przenie�� 144 obserwacje 
sets = sample(1:nrow(iris), 0.96 * nrow(iris))  
train_ir = iris_std[sets,] 
test_ir = iris_std[-sets,] 

#wyliczenie macierzy odleg�o�ci 
dist_mat_e = dist(train_ir, method = 'euclidean') 
dist_mat_m = dist(train_ir, method = 'manhattan') 

dist_mat_c = dist(train_ir, method ='canberra')

?dist

head(dist_mat_e) 

head(dist_mat_m)

#model klasteryzacji aglomeracyjnej 
library(stats) 
m1_e_ir = hclust(dist_mat_e, method = 'average') 
m2_e_ir = hclust(dist_mat_e, method = 'complete') 
m3_e_ir = hclust(dist_mat_e, method = 'ward.D') 
m4_e_ir = hclust(dist_mat_e, method = 'single') 

?hclust

plot(m1_e_ir) 
abline(h = 3, col = 'red') 

plot(m2_e_ir) 
abline(h = 3, col = 'red')

plot(m3_e_ir) 
abline(h = 3, col = 'red')

plot(m4_e_ir) 
abline(h = 3, col = 'red')

#Obcinamy drzewo do wybranej liczby klastr�w, mo�emy te� obci�� na wybranej 
#wysoko�ci jak np. by�o zaznaczone na wykresach 

cut_m1_e_ir = cutree(m1_e_ir, k = 3) 
cut_m2_e_ir = cutree(m2_e_ir, k = 3) 
cut_m3_e_ir = cutree(m3_e_ir, k = 3) 
cut_m4_e_ir = cutree(m4_e_ir, k = 2)

library(cluster) 
library(clValid) 
#opis funkcji dunn: 
#The Dunn Index is the ratio of the smallest distance between observations not 
#in the same cluster to the largest intra-cluster distance. The Dunn Index h
#as a value between zero and infinity, and should be maximized.  

dunn(dist_mat_e, cut_m1_e_ir) 

dunn(dist_mat_e, cut_m2_e_ir)

dunn(dist_mat_e, cut_m3_e_ir)

dunn(dist_mat_e, cut_m4_e_ir)


#na podstawie indeksu dunn mo�emy stwierdzi�, �e optymalny model to m4_e_ir 

#wykonujemy df przechowuj�c� dane pierwotne plus numery klastr�w 
library(dplyr) 
results_clus_ir = mutate(train_ir, m4_e_ir=cut_m4_e_ir) 
table(results_clus_ir$m4_e_ir) 

head(train_ir)
head(results_clus_ir)

#wizualizacja danych: 
plot(results_clus_ir$Sepal.Length, results_clus_ir$Petal.Width,
     col = as.factor(results_clus_ir$m4_e_ir))

plot(results_clus_ir$Sepal.Length, results_clus_ir$Sepal.Width,
     col = as.factor(results_clus_ir$m4_e_ir))

plot(results_clus_ir$Sepal.Length, results_clus_ir$Petal.Length,
     col = as.factor(results_clus_ir$m4_e_ir))

plot(results_clus_ir$Sepal.Width, results_clus_ir$Petal.Length,
     col = as.factor(results_clus_ir$m4_e_ir))

plot(results_clus_ir$Sepal.Width, results_clus_ir$Petal.Width,
     col = as.factor(results_clus_ir$m4_e_ir))

plot(results_clus_ir$Petal.Length, results_clus_ir$Petal.Width,
     col = as.factor(results_clus_ir$m4_e_ir))


#wykresy nale�y powt�rzy� dla wszystkich kombinacji zmiennych 

#wnioski: dlaczego ten model nie jest najlepszy?? 
#predykcja dla nowych obserwacji metod� k-NN (poprzednie zaj�cia) 



#K-means

#ziarno losowe 
set.seed(15) 

k1_ir = kmeans(train_ir,centers=3, nstart = 20) 
str(k1_ir) 

table(k1_ir$cluster) 

dunn(clusters=k1_ir$cluster,Data=train_ir) 

#zapisanie do data frame z wynikami z poprzedniej metody 
results_clus_ir = mutate(results_clus_ir, k1_ir=k1_ir$cluster) 
table(results_clus_ir$k1_ir) 

plot(results_clus_ir$Sepal.Length, results_clus_ir$Petal.Width, 
     col = as.factor(results_clus_ir$k1_ir)) 

#kryterium "z�amanego kija"- wyb�r optymalnej liczby skupie� 
#generujemy wektor z samymi 0 
ratio_ss = rep(0, 7) 

for (k in 1:7) { 
  models = kmeans(train_ir, k, nstart = 20) 
  ratio_ss[k] = models$tot.withinss / models$totss 
  
} 
plot(ratio_ss, type = "b", xlab = "k") 

library(readr)
library(dplyr)
library(corrplot)
library(lubridate)
library(DataCombine)
library(psych)
library(class)
library(naivebayes)
library(pROC)
library(cluster) 
library(clValid) 

head(USArrests)
USArrests[,c(1,2,3,4)]
USArrests[,c(1,2,3,4)]
USArrests_names = rownames(USArrests)
stand = function(x) { (x -mean(x))/(sd(x)) } 
USArrests_std = as.data.frame(sapply(USArrests[,c(1,2,3,4)],stand, USE.NAMES=TRUE )) 

#do zbioru ucz¹cego proszê przenieœæ 144 obserwacje 
sets = sample(1:nrow(USArrests), 0.96 * nrow(USArrests))  
train_arrests = USArrests_std[sets,]
train_names = USArrests_names[sets]
test_arrests = USArrests_std[-sets,] 
test_names = USArrests_names[-sets]

head(train_arrests)
train_names
test_names

set.seed(15) 

k1_arrests = kmeans(train_arrests,centers=3, nstart = 20) 
str(k1_arrests) 

table(k1_arrests$cluster) 

dunn(clusters=k1_arrests$cluster,Data=train_arrests) 

plot(train_arrests$Murder, train_arrests$Assault,
     col = k1_arrests$cluster)

plot(train_arrests$Murder, train_arrests$UrbanPop,
     col = k1_arrests$cluster)

plot(train_arrests$Murder, train_arrests$Rape,
     col = k1_arrests$cluster)

plot(train_arrests$Assault, train_arrests$UrbanPop,
     col = k1_arrests$cluster)

plot(train_arrests$Assault, train_arrests$Rape,
     col = k1_arrests$cluster)

plot(train_arrests$UrbanPop, train_arrests$Rape,
     col = k1_arrests$cluster)



#kryterium "z³amanego kija"- wybór optymalnej liczby skupieñ 
#generujemy wektor z samymi 0 
ratio_ss = rep(0, 7) 

for (k in 1:7) { 
  models = kmeans(train_arrests, k, nstart = 20) 
  ratio_ss[k] = models$tot.withinss / models$totss 
  
} 
plot(ratio_ss, type = "b", xlab = "k") 

# 3 albo 2 najbardziej optymalne

#kmeans dla dwóch

set.seed(15) 

k2_arrests = kmeans(train_arrests,centers=2, nstart = 17) 
str(k2_arrests) 

table(k2_arrests$cluster) 

dunn(clusters=k2_arrests$cluster,Data=train_arrests) 

plot(train_arrests$Murder, train_arrests$Assault,
     col = k2_arrests$cluster)

plot(train_arrests$Murder, train_arrests$UrbanPop,
     col = k2_arrests$cluster)

plot(train_arrests$Murder, train_arrests$Rape,
     col = k2_arrests$cluster)

plot(train_arrests$Assault, train_arrests$UrbanPop,
     col = k2_arrests$cluster)

plot(train_arrests$Assault, train_arrests$Rape,
     col = k2_arrests$cluster)

plot(train_arrests$UrbanPop, train_arrests$Rape,
     col = k2_arrests$cluster)

# Dodajemy dwa klastry z metod k-means

results_clus_arrests = mutate(train_arrests, k1_arrests = k1_arrests$cluster)
results_clus_arrests = mutate(results_clus_arrests, k2_arrests = k2_arrests$cluster)

head(results_clus_arrests)


dist_mat_c = dist(train_arrests, method = 'canberra') 

library(stats) 
m1_c_arrests = hclust(dist_mat_c, method = 'average') 
m2_c_arrests = hclust(dist_mat_c, method = 'complete') 
m3_c_arrests = hclust(dist_mat_c, method = 'ward.D') 
m4_c_arrests = hclust(dist_mat_c, method = 'single') 

plot(m1_c_arrests) 
abline(h = 3, col = 'red') 

plot(m2_c_arrests) 
abline(h = 3, col = 'red')

plot(m3_c_arrests) 
abline(h = 3, col = 'red')

plot(m4_c_arrests) 
abline(h = 3, col = 'red')

cut_m1_c_arrests = cutree(m1_c_arrests, k = 2) 
cut_m2_c_arrests = cutree(m2_c_arrests, k = 2) 
cut_m3_c_arrests = cutree(m3_c_arrests, k = 2) 
cut_m4_c_arrests = cutree(m4_c_arrests, k = 2)

dunn(dist_mat_c, cut_m1_c_arrests) 

dunn(dist_mat_c, cut_m2_c_arrests)

dunn(dist_mat_c, cut_m3_c_arrests)

dunn(dist_mat_c, cut_m4_c_arrests)

results_clus_arrests = mutate(results_clus_arrests, m3_c_arrests=cut_m3_c_arrests) 
table(results_clus_arrests$m3_c_arrests) 

plot(train_arrests$Murder, train_arrests$UrbanPop,
     col = as.factor(results_clus_arrests$m3_c_arrests))

plot(train_arrests$Murder, train_arrests$Rape,
     col = as.factor(results_clus_arrests$m3_c_arrests))

plot(train_arrests$Assault, train_arrests$UrbanPop,
     col = as.factor(results_clus_arrests$m3_c_arrests))

plot(train_arrests$Assault, train_arrests$Rape,
     col = as.factor(results_clus_arrests$m3_c_arrests))

plot(train_arrests$UrbanPop, train_arrests$Rape,
     col = as.factor(results_clus_arrests$m3_c_arrests))

head(results_clus_arrests)

#cluster voting

occurences = array(0, dim=3)
occurences[1] = 2;
occurences[2] = 3;
occurences[3] = 5;
which(occurences == max(occurences))[1]
results_clus_arrests[1,][7]


voting = function(x) {
  occurences = array(0, dim=3)
  occurences[x[7]] = occurences[x[7]]+1
  occurences[x[6]] = occurences[x[6]]+1
  occurences[x[5]] = occurences[x[5]]+1
  voted = which(occurences == max(occurences))
   
  
} 

voted = c();
?append

head(results_clus_arrests)
results_clus_arrests[[2,7]]

for (i in 1:nrow(results_clus_arrests))
{
  occurences = array(0, dim=3)
  occurences[results_clus_arrests[i, 7]] = occurences[results_clus_arrests[i, 7]]+1
  occurences[results_clus_arrests[i, 6]] = occurences[results_clus_arrests[i, 6]]+1
  occurences[results_clus_arrests[i, 5]] = occurences[results_clus_arrests[i, 5]]+1
  voted = append(voted,which(occurences == max(occurences))[1])
}

results_clus_arrests['voted'] = voted

head(results_clus_arrests)

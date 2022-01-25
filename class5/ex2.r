library(readr)
library(dplyr)
library(corrplot)
library(lubridate)
library(DataCombine)
library(psych)
library(class)
library(naivebayes)
library(pROC)

IndianPima = read_delim("C:/Users/kamil/Downloads/IndianPima1.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_names = FALSE, trim_ws = TRUE, skip = 10)

str(IndianPima)
head(IndianPima)
colnames(IndianPima) = c("timesPregnant","glucoseConc","bloodPressure","skinfoldThickness","serumInsulin","bmi","diabetesPedigreeFunction","age","diabetic")
head(IndianPima)

table(IndianPima$diabetic) # 262 chore, 130 zdrowe

cor(IndianPima)
# korelacja powyzej 0,5 dla bycia cukrzykiem i koncertracji glukozy

summary(IndianPima)
# spory rozrzut w koncentracji glukozy

boxplot(IndianPima$glucoseConc)
pairs.panels(IndianPima)
# œrednio coœ widaæ, rozk³ady natomiast histogramy przypominaj¹ rozk³ady normalne

boxplot(IndianPima)

boxplot(timesPregnant~diabetic,IndianPima)
boxplot(glucoseConc~diabetic,IndianPima)
boxplot(bloodPressure~diabetic,IndianPima)
boxplot(skinfoldThickness~diabetic,IndianPima)
boxplot(serumInsulin~diabetic,IndianPima)
boxplot(bmi~diabetic,IndianPima)
boxplot(diabetesPedigreeFunction~diabetic,IndianPima)
boxplot(age~diabetic,IndianPima)


stand = function(x) { (x-mean(x))/(sd(x)) }

diabetic_std_values = as.data.frame(lapply(IndianPima[,c(2,4,5,6)],stand ))
diabetic_std_all = data.frame(diabetic_std_values, IndianPima$diabetic)
head(diabetic_std_all)
 
sets = sample(1:nrow(IndianPima), 0.75 * nrow(IndianPima))
train_ip=diabetic_std_all[sets,]
test_ip=diabetic_std_all[-sets,]

head(diabetic_std_all)

train_ip_class=IndianPima[sets,9]
test_ip_class=IndianPima[-sets,9]

length(train_ip)
head(train_ip)
length(train_ip_class)


model_ip3 = knn(train = train_ip[,c(1,2,3,4)], test = test_ip[,c(1,2,3,4)], cl =
                   t(train_ip_class), k=3)

t_ip3=table(t(test_ip_class), model_ip3) 
acc_ip3=mean(t(test_ip_class) == model_ip3) 
table(t(test_ip_class), model_ip3)
tpr_ip3 = t_ip3[1,1]/(t_ip3[1,1]+t_ip3[2,1]) # czu³oœæ 
tnr_ip3 = t_ip3[2,2]/(t_ip3[2,1]+t_ip3[2,2]) # specyficznoœæ 
fpr_ip3 = t_ip3[2,1]/(t_ip3[2,1]+t_ip3[2,2]) # fp rate
model_ip3
t(test_ip_class)

ROC_ip3 = roc(t(test_ip_class),as.vector(as.numeric(model_ip3)))
ROC_ip3
plot(ROC_ip3)

model_ip5 = knn(train = train_ip[,c(1,2,3,4)], test = test_ip[,c(1,2,3,4)], cl =
                  t(train_ip_class), k=5)

t_ip5=table(t(test_ip_class), model_ip5) 
acc_ip5=mean(t(test_ip_class) == model_ip5) 
table(t(test_ip_class), model_ip5) 
tpr_ip5 = t_ip5[1,1]/(t_ip5[1,1]+t_ip5[2,1]) # czu³oœæ 
tnr_ip5 = t_ip5[2,2]/(t_ip5[2,1]+t_ip5[2,2]) # specyficznoœæ 
fpr_ip5 = t_ip3[2,1]/(t_ip5[2,1]+t_ip5[2,2]) # fp rate

model_ip7 = knn(train = train_ip[,c(1,2,3,4)], test = test_ip[,c(1,2,3,4)], cl =
                  t(train_ip_class), k=7)

t_ip7=table(t(test_ip_class), model_ip7) 
acc_ip7=mean(t(test_ip_class) == model_ip7) 
table(t(test_ip_class), model_ip7) 
tpr_ip7 = t_ip7[1,1]/(t_ip7[1,1]+t_ip7[2,1]) # czu³oœæ 
tnr_ip7 = t_ip7[2,2]/(t_ip7[2,1]+t_ip7[2,2]) # specyficznoœæ 
fpr_ip7 = t_ip7[2,1]/(t_ip7[2,1]+t_ip7[2,2]) # fp rate
#naiwne sieci bayesa
head(train_ip)

train_ip$IndianPima.diabetic = as.logical(train_ip$IndianPima.diabetic)

nb_ip1 = naive_bayes(IndianPima.diabetic ~ ., train_ip) 

summary(nb_ip1)

head(test_ip)

nb_ip1_pred=predict(nb_ip1, test_ip[,-5], type = "class") 
nb_ip1_pred_prob=predict(nb_ip1, test_ip[,-5], type = "prob") 
hist(nb_ip1_pred_prob)

get_cond_dist(nb_ip1)
test_ip[,5] = as.logical(test_ip[,5])

t_nb_ip1=table(test_ip[,5], nb_ip1_pred) 
acc_nb_ip1=mean(test_ip[,5] == nb_ip1_pred)
table(test_ip[,5], nb_ip1_pred) 
tpr_nb_ip1 = t_nb_ip1[1,1]/(t_nb_ip1[1,1]+t_nb_ip1[2,1]) # czu³oœæ 
tnr_nb_ip1 = t_nb_ip1[2,2]/(t_nb_ip1[2,1]+t_nb_ip1[2,2]) # specyficznoœæ 
fpr_nb_ip1 = t_nb_ip1[2,1]/(t_nb_ip1[2,1]+t_nb_ip1[2,2]) # fp rate

ROC_nb_ip1 = roc(t(test_ip_class),as.vector(as.numeric(nb_ip1_pred)))
ROC_nb_ip1
plot(ROC_nb_ip1)
# pakiet caret 
head(train_ip)

nb_ip2 = naive_bayes(IndianPima.diabetic ~ ., train_ip, prior=c(0.8, 0.2)) 
summary(nb_ip2)

nb_ip2_pred=predict(nb_ip2, test_ip[,-5], type = "class") 
nb_ip2_pred_prob=predict(nb_ip2, test_ip[,-5], type = "prob")
hist(nb_ip2_pred_prob)
nb_ip2_pred_prob
get_cond_dist(nb_ip2)

t_nb_ip2=table(test_ip[,5], nb_ip2_pred) 
acc_nb_ip2=mean(test_ip[,5] == nb_ip2_pred)
table(test_ip[,5], nb_ip2_pred) 
tpr_nb_ip2 = t_nb_ip2[1,1]/(t_nb_ip2[1,1]+t_nb_ip2[2,1]) # czu³oœæ 
tnr_nb_ip2 = t_nb_ip2[2,2]/(t_nb_ip2[2,1]+t_nb_ip2[2,2]) # specyficznoœæ 
fpr_nb_ip2 = t_nb_ip2[2,1]/(t_nb_ip2[2,1]+t_nb_ip2[2,2]) # fp rate

ROC_nb_ip2 = roc(t(test_ip_class),as.vector(as.numeric(nb_ip2_pred)))
ROC_nb_ip2
plot(ROC_nb_ip2)




# IndianPima$diabetic <- as.factor(unlist(IndianPima$diabetic)) 
# IndianPima$diabetic <- factor(IndianPima$diabetic, levels=c("1", "0"),           
#                       labels = c("Positive", "Negative")) 
# summary(IndianPima$diabetic)
# 
# head(IndianPima)
# 
# IndianPima$timesPregnant = ifelse(IndianPima$timesPregnant==0, "No", "Yes") %>% factor()
# head(IndianPima)
# 
# IndianPima$bmi = ifelse(IndianPima$bmi<19,"Underweight", 
#                       ifelse(IndianPima$bmi>=19 & IndianPima$bmi<=25, "Normal", 
#                              ifelse(IndianPima$bmi>=25 & IndianPima$bmi<=30, "Overweight","Obese"))) %>% 
#   factor(levels=c("Underweight","Normal", "Overweight","Obese"))
# 
# list(BMI = summary(IndianPima$bmi))
# head(IndianPima)
# 
# IndianPima$glucoseConc = IndianPima$glucoseConc*0.0555
# IndianPima$glucoseConc
# IndianPima$glucoseConc = if_else(IndianPima$glucoseConc<2.2,"Hypoglycemia", 
#                            if_else(IndianPima$glucoseConc>=2.2 & IndianPima$glucoseConc <=7.8,"Normal", 
#                                    if_else(IndianPima$glucoseConc>7.8 & IndianPima$glucoseConc <=11.1, "Hyperglycemia","Diabetes"))) %>% factor()
# list( `Test Result` = summary(IndianPima$glucoseConc) )
# head(IndianPima)
# 
# sets = sample(1:nrow(IndianPima), 0.75 * nrow(IndianPima))
# train_ip=IndianPima[sets,]
# test_ip=IndianPima[-sets,]
# 
# train_ip_class=IndianPima[sets,9]
# test_ip_class=IndianPima[-sets,9]
# 
# length(train_ip)
# length(t(train_ip_class))
# 
# model_ip3 = knn(train = train_ip[,c(2,4,5,6)], test = test_ip[,c(2, 4,5,6)], cl =
#                   t(train_ip_class), k=3)
# 
# t_ip3=table(t(test_ip_class), model_ip3) 
# acc_ip3=mean(t(test_ip_class) == model_ip3) 
# table(t(test_ip_class), model_ip3)
# 
# 
# model_ip5 = knn(train = train_ip[,c(2,4,5,6)], test = test_ip[,c(2, 4,5,6)], cl =
#                   t(train_ip_class), k=5)
# 
# t_ip5=table(t(test_ip_class), model_ip5) 
# acc_ip5=mean(t(test_ip_class) == model_ip5) 
# table(t(test_ip_class), model_ip5) 
# 
# model_ip7 = knn(train = train_ip[,c(2,4,5,6)], test = test_ip[,c(2, 4,5,6)], cl =
#                   t(train_ip_class), k=7)
# 
# t_ip7=table(t(test_ip_class), model_ip7) 
# acc_ip7=mean(t(test_ip_class) == model_ip7) 
# table(t(test_ip_class), model_ip7) 
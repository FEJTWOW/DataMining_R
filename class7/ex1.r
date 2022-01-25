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
tail(IndianPima)
IndianPima
table(IndianPima$diabetic) # 262 chore, 130 zdrowe

cor(IndianPima)
# korelacja powyzej 0,5 dla bycia cukrzykiem i koncertracji glukozy

summary(IndianPima)

stand = function(x) { (x-mean(x))/(sd(x)) }

diabetic_std_values = as.data.frame(lapply(IndianPima[,c(2,4,5,6)],stand ))
diabetic_std_all = data.frame(diabetic_std_values, IndianPima$diabetic)
head(diabetic_std_all)

sets = sample(1:nrow(IndianPima), 0.75 * nrow(IndianPima))
train_ip=diabetic_std_all[sets,]
test_ip=diabetic_std_all[-sets,]

head(test_ip)

head(diabetic_std_all)

train_ip_class=IndianPima[sets,9]
test_ip_class=IndianPima[-sets,9]

?neuralnet()

head(train_ip)
head(test_ip)



nn_ip = neuralnet(as.factor(IndianPima.diabetic) ~ glucoseConc + skinfoldThickness + serumInsulin + bmi, 
                   train_ip,  hidden=3, act.fct = "logistic", rep=5, linear.output = FALSE, err.fct = "ce",  
                   threshold = 0.005) 

plot(nn_ip, rep="best")

?


# = compute(nn_ip, test_ip[,c(1,2,3,4)]) 
#"class"
nn_ip_pred=predict(nn_ip, test_ip[,-5], type = "class") 
nn_ip_pred

id = apply(nn_ip_pred, 1, which.max) 
id
t_nn_ip = table(as.factor(test_ip[,5]), as.factor(id-1))
acc_nn_ip=mean(test_ip[,5] == id-1)

tpr_nn_ip = t_nn_ip[1,1]/(t_nn_ip[1,1]+t_nn_ip[2,1]) # czu³oœæ 
tnr_nn_ip = t_nn_ip[2,2]/(t_nn_ip[2,1]+t_nn_ip[2,2]) # specyficznoœæ 

ROC_nb_ip = roc(as.factor(test_ip[,5]),as.numeric(as.factor(id-1)))
ROC_nb_ip
plot(ROC_nb_ip)


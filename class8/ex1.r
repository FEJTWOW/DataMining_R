library(rpart)
library(rpart.plot)
library(readr)

IndianPima = read_delim("C:/Users/kamil/Downloads/IndianPima1.txt", 
                        delim = "\t", escape_double = FALSE, 
                        col_names = FALSE, trim_ws = TRUE, skip = 10)

str(IndianPima)
head(IndianPima)
colnames(IndianPima) = c("timesPregnant","glucoseConc","bloodPressure","skinfoldThickness","serumInsulin","bmi","diabetesPedigreeFunction","age","diabetic")
head(IndianPima)

IndianPima$diabetic = as.factor(IndianPima$diabetic)
head(IndianPima)

sets = sample(1:nrow(IndianPima), 0.75 * nrow(IndianPima))
train_ip = IndianPima[sets,]
test_ip = IndianPima[-sets,]

set.seed(1337)
head(train_ip)



ip_cl1 = rpart(diabetic~.,data=train_ip)
ip_cl1

print(ip_cl1)

summary(ip_cl1)

rpart.plot(ip_cl1, box.col=c("red", "green"))

ip_cl1_pred<- predict(ip_cl1,newdata=test_ip[-9])

ip_cl1_pred

library(caret)
id = apply(ip_cl1_pred, 1, which.max) 
ip_cl1_pred = id-1
test_ip$diabetic
confusionMatrix(as.factor(ip_cl1_pred),test_ip$diabetic)


printcp(ip_cl1)

opt = which.min(ip_cl1$cptable[,'xerror'])
cp = ip_cl1$cptable[opt, 'CP']
pruned_ip = prune(ip_cl1,cp)
rpart.plot(pruned_ip, box.col=c("red", "green"))

ip_prun_pred = predict(pruned_ip,newdata=test_ip[-9],type = 'class')

confusionMatrix(as.factor(ip_cl1_pred),test_ip$diabetic)

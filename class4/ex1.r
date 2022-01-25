library(liver)

data("cereal")

str(cereal) 

summary(cereal) 

plot(cereal$rating)

hist(cereal$rating) 

boxplot(cereal$rating)

library(corrplot) 

cor_matrix<-round(cor(cereal[4:16]),2) 
cor_matrix

plot(cereal$rating~cereal$calories) 

plot(cereal$rating~cereal$sugars) 

plot(cereal$rating~cereal$fiber) 

corrplot(cor_matrix)

#podzia³ na zbiór ucz¹cy i testowy 
train<-cereal[-c(5, 15, 25, 35, 55),] 
test<-cereal[c(5, 15, 25, 35, 55),] 

#model 1 
m1<-lm(rating~sugars+fiber+protein+sodium+fat, train) 
summary(m1)

plot(train$rating, type="l", col="red") 
lines(m1$fitted.values, type="l", col="blue") 

aic_m1<-AIC(m1) 
bic_m1<-BIC(m1) 

hist(m1$residuals)

plot(m1$residuals~m1$fitted.values) 

library(MLmetrics)

pred_m1<-predict(m1, newdata = test) 
mape_m1<-MAPE(pred_m1, test$rating) 


library(readr)
library(dplyr)
library(corrplot)
library(lubridate)
library(DataCombine)
data = read_delim("C:/Users/kamil/Desktop/Series_G.csv", 
                       delim = "\t", escape_double = FALSE, 
                       col_names = FALSE, col_types = cols(X2 = col_date(format = "%d-%m-%Y")), 
                       trim_ws = TRUE)
head(data)
names(data) = c("Air","Date")
summary(data)
str(data)

plot(data$Air,type="l")

data_idx = data%>%
  mutate(t=c(1:144), t2=t^2)
str(data_idx)
head(data_idx)

df = data_idx%>%
  mutate(month=month(Date))

head(df)

# one hot encoding na miesi¹ce, musimy miec mo¿liwoœæ jakoœ przekazaæ do algorytmu dla którego miesi¹ca bierzemy sampla
df = df%>%
  mutate(jan=ifelse(month==1,1,0), feb=ifelse(month==2,1,0), mar=ifelse(month==3,1,0), ap=ifelse(month==4,1,0)
         ,may=ifelse(month==5,1,0), jun=ifelse(month==6,1,0), jul=ifelse(month==7,1,0), aug=ifelse(month==8,1,0), sep=ifelse(month==9,1,0),
         oct=ifelse(month==10,1,0), nov=ifelse(month==11,1,0), dec=ifelse(month==12,1,0))

head(df,20)

plot(data$Air)

hist(data$Air) 

boxplot(data$Air)

plot(data$Air~data$Date)

acf(data$Air, plot = TRUE)
pacf(data$Air, plot = TRUE)


# tworzymy lag value, czyli zmienne opóŸnione, poka¿¹ one algorytmowi jaka by³a liczba wylotów w zesz³ym roku 
df = df %>%
  group_by(month) %>%
  mutate(lag.value = lag(Air, n = 1, default = NA))

head(df,20)
tail(df)

df_noNA = df[c(13:144),]
tail(df_noNA)
head(df_noNA,20)
train<-df_noNA[-c(10,76,132),] 
test<-df_noNA[c(10,76,132),]

# œmieszna sprawa, okaza³o siê ¿e wystarcz¹ tylko te lag values 
m1<-lm(Air ~ lag.value, train) 
summary(m1)

plot(train$Air, type="l", col="red") 
lines(m1$fitted.values, type="l", col="blue")


aic_m1<-AIC(m1) 
bic_m1<-BIC(m1) 

hist(m1$residuals)

plot(m1$residuals~m1$fitted.values) 

library(MLmetrics)

pred_m1<-predict(m1, newdata = test) 
mape_m1<-MAPE(pred_m1, test$Air) 

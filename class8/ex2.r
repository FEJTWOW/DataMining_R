library(rpart)
library(rpart.plot)
library(dplyr)
library(lubridate)
library(DataCombine)
library(readr)
library(MLmetrics)
Randki = read_table2("C:/Users/kamil/Downloads/Randki1.txt", 
                       col_types = cols(data_logowania = col_date(format = "%d-%m-%y")))

head(Randki)
Randki

df = Randki%>%
  mutate(month=month(data_logowania))
head(df)

df = df%>%
  mutate(jan=ifelse(month==1,1,0), feb=ifelse(month==2,1,0), mar=ifelse(month==3,1,0), ap=ifelse(month==4,1,0)
         ,may=ifelse(month==5,1,0), jun=ifelse(month==6,1,0), jul=ifelse(month==7,1,0), aug=ifelse(month==8,1,0), sep=ifelse(month==9,1,0),
         oct=ifelse(month==10,1,0), nov=ifelse(month==11,1,0), dec=ifelse(month==12,1,0))
head(df)
df = df[,-1]
df = df[-3]
head(df)



train_df <-df[-c(149,148,147,146),] 
test_df <-df[c(149,148,147,146),]

head(train_df)


set.seed(10)
rt_df = rpart(Liczba_logowan~., data = train_df,
                control = rpart.control(cp = 0.00001))
printcp(rt_df)

plot(rt_df)
text(rt_df, cex = 0.9, xpd = TRUE)

plotcp(rt_df)

rt_df_pr = prune(rt_df, cp = 0.0120165)
plot(rt_df_pr)
text(rt_df_pr, cex = 0.9, xpd = TRUE)

rpart.plot(rt_df_pr)

rt_df_pr_mod = predict(rt_df_pr)

R2 = cor(train_df$Liczba_logowan, rt_df_pr_mod)^2

rt_df_pr_res=train_df$Liczba_logowan-rt_df_pr_mod

rt_df_pr_pred = predict(rt_df_pr, test_df[,-2])

mape_df = MAPE(rt_df_pr_pred, test_df$Liczba_logowan) 

plot(train_df$Liczba_logowan, type="l", col="red") 
lines(rt_df_pr_mod, type="l", col="blue")

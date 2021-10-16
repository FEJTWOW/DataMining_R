mydata = data.frame(x=c(11,12,14),y=c("a","b","b"),z=c(T,F,T))
mydata
class(mydata)
str(mydata)
mydata$x
mydata[["x"]]
mydata[,1]
subset(mydata,subset=x<=12)
position = order(mydata$z)
position
?order
mydata[position,]

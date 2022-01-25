d_pogodowe <- read.delim("dane_pogodowe.txt", sep="\t", header=FALSE)  
print(d_pogodowe)
colnames(d_pogodowe)<-c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")  
rownames(d_pogodowe)<- c(1955:2014)  
print(d_pogodowe) 
mean(d_pogodowe$VII); 
mean(d_pogodowe[["VII"]]); 
mean(d_pogodowe[,7]) 

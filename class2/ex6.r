m1 <- matrix(1:10, nrow=5, ncol=6) 
m1 
a_m1 <- apply(m1, 2, sum) 
a_m1

movies <- c("SPIDERMAN","BATMAN","VERTIGO","CHINATOWN") 
movies_lower <-lapply(movies, tolower) 
str(movies_lower) 
movies_lower <-unlist(lapply(movies,tolower)) 
str(movies_lower)

?matrix
m2 = matrix(1:30, nrow = 10, ncol=3)
m2
a_m2 = apply(m2, 1, sum) 
a_m2

my_list = list(c(1,2,3,4,5),c(4,12,20,28,36),c(1,3,5,7,9))
lapply(my_list,mean)


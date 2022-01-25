library(kohonen)


head(USArrests)

usa = scale(USArrests)
usa


set.seed(15) 

grid1 = somgrid(xdim=3, ydim=3, topo=c("rectangular")) 
grid1 
som_usa = som(usa, grid=grid1, rlen = 100, alpha = c(0.05, 0.01)) 
som_usa

summary(som_usa) 

plot(som_usa) 
# dobrze je�li klastry s� r�ne

som_usa$grid$pts 

plot(som_usa, type="count")

usa_cluster<-som_usa[['unit.classif']] 
table(usa_cluster) 

#wariancja wewn�trz klastra i dla ca�o�ci danych

mean_som_usa <- mean(som_usa$distances)  
mean_som_usa 

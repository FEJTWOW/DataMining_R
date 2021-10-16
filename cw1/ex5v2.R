w1 = seq(from=2, to=17, length.out=16)
w1
mw1 = matrix(w1,ncol=4)
mw2 = matrix(w1,ncol=4, byrow=TRUE)
mw1
mw2
mw1 = mw1/3
res = mw1*mw2
res
res
res[which(res >= 20, arr.ind = TRUE)]

frow = c(460.998,314.4)
srow = c(290.475, 247.9)
trow = c(309.306, 165.8)
star_wars = matrix(c(frow,srow,trow), ncol = 2,byrow=TRUE)
star_wars
colnames(star_wars) = c("US","non-US")
rownames(star_wars) = c("A New Hope","The Empire Strikes Back","Return of the Jedi")
star_wars
star_wars[3,]
star_wars[1,2]
m = matrix(rnorm(9, mean=6, sd=12),ncol=3)
m

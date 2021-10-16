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

frow = c(NA,"US","non-US")
srow = c("A New Hope",460.998,314.4)
trow = c("The Empire Strikes Back", 290.475, 247.9)
ftrow = c("Return of the Jedi", 309.306, 165.8)
star_wars = matrix(c(frow,srow,trow,ftrow), ncol = 3,byrow=TRUE)
print(star_wars)
prmatrix(star_wars,rowlab = rep("", 4),collab = rep("",6))
star_wars[2,3]
star_wars[4,]
m = matrix(rnorm(9, mean=6, sd=12),ncol=3)
m


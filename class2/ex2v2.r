nycflights13::airports
nycflights13::airlines
nycflights13::flights
nycflights13::planes
nycflights13::weather

cars = read.delim("cars.txt", sep=",", header=FALSE)
colnames(cars) = c('mpg', 'cylinders', 'cubicinches', 'hp', 'weightlbs', 'time-to-60', 'year', 'brand')
rownames(cars) = seq(nrow(cars))
head(cars)


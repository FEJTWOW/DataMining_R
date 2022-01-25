library(dplyr)
library(zoo)

wastewater <- read.delim("wastewater.txt", sep="\t", header=TRUE)
wastewater

glimpse(wastewater)
summary(wastewater)
str(wastewater)

boxplot(wastewater$Sandomierz) 
plot(wastewater$Sandomierz, type="l") 
hist(wastewater$Sandomierz, breaks=8) 

wastewater = wastewater%>%
  mutate(a_Sandomierz = ifelse(Sandomierz < 0 | Sandomierz > 20, NA, Sandomierz))
wastewater

mean = mean(wastewater$a_Sandomierz, na.rm=TRUE)
std = sd(wastewater$a_Sandomierz, na.rm=TRUE)

wastewater = wastewater%>%
  mutate(a_Sandomierz = ifelse(a_Sandomierz > (mean+3*std) | a_Sandomierz < (mean-3*std), NA, a_Sandomierz))


boxplot(wastewater$a_Sandomierz)
plot(wastewater$a_Sandomierz, type="l")
hist(wastewater$a_Sandomierz, breaks=8)

keeps = c("a_Sandomierz")
wastewater = wastewater[keeps]
wastewater
acf(wastewater, lag.max = NULL, type = "correlation", plot = TRUE, na.action = na.pass)


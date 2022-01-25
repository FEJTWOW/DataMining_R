library(gapminder)
library(dplyr)

glimpse(gapminder) 
slice(gapminder, 12:20) 

gapminder %>% 
  filter(year==1957) 

gapminder%>% 
  arrange(desc(lifeExp)) 

gapminder%>% 
  mutate(lifeExp=lifeExp*12) 

gapminder%>% 
  summarize(medianLifeExp=median(lifeExp))

gapminder%>% 
  group_by(year)%>% 
  summarize(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap)) 

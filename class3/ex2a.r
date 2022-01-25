#Przygotuj skrypt z nastêpuj¹cymi elementami: 

library(gapminder)
library(dplyr)

glimpse(gapminder) 

#Wykonaj filtrowanie danych gapminder dla Chin z roku 2002. 

gapminder%>%
  filter(year==2002)%>%
    filter(country=="China")

# Wykonaj filtrowanie danych gapminder dla roku 1957 i wykonaj sortowanie wyników wed³ug 
# zmiennej pop.

gapminder%>%
  filter(year==1957)%>%
    arrange(pop)

#Dla danych z roku 2007, utwórz now¹ zmienn¹ w obrêbie gapminder 
#lifeExpMonths=lifeExp*12 
# posortuj wyniki wed³ug nowo utworzonej zmiennej  od najwy¿szych do najni¿szych wartoœci. 

gapminder%>%
  filter(year==2007)%>%
    mutate(lifeExpMonths=lifeExp*12)%>%
      arrange(desc(lifeExpMonths))

#Dla danych z roku 1957 oblicz medianê z lifeExp i umieœæ w zmiennej medianLifeExp  oraz 
#maksimum z gdpPercap i umieœæ w zmiennej maxGdpPercap. 

gapminder%>%
  filter(year==1957)%>%
    mutate(medianLifeExp=median(lifeExp))
    mutate(maxGdpPercap=max(gdpPercap))
    
    
#Dla pogrupowanych danych wed³ug kontynentu i roku, oblicz medianê z lifeExp i umieœæ w 
#zmiennej  medianLifeExp  oraz  wartoœæ  œredni¹  z  gdpPercap  i  umieœæ  w  zmiennej 
#meanGdpPercap.
    
gapminder%>%
  group_by(continent,year)%>%
    mutate(medianLifeExp=median(lifeExp))
    mutate(meanGdpPercap=mean(gdpPercap))
    


#Przygotuj skrypt z nast�puj�cymi elementami: 

library(gapminder)
library(dplyr)

glimpse(gapminder) 

#Wykonaj filtrowanie danych gapminder dla Chin z roku 2002. 

gapminder%>%
  filter(year==2002)%>%
    filter(country=="China")

# Wykonaj filtrowanie danych gapminder dla roku 1957 i wykonaj sortowanie wynik�w wed�ug 
# zmiennej pop.

gapminder%>%
  filter(year==1957)%>%
    arrange(pop)

#Dla danych z roku 2007, utw�rz now� zmienn� w obr�bie gapminder 
#lifeExpMonths=lifeExp*12 
# posortuj wyniki wed�ug nowo utworzonej zmiennej  od najwy�szych do najni�szych warto�ci. 

gapminder%>%
  filter(year==2007)%>%
    mutate(lifeExpMonths=lifeExp*12)%>%
      arrange(desc(lifeExpMonths))

#Dla danych z roku 1957 oblicz median� z lifeExp i umie�� w zmiennej medianLifeExp  oraz 
#maksimum z gdpPercap i umie�� w zmiennej maxGdpPercap. 

gapminder%>%
  filter(year==1957)%>%
    mutate(medianLifeExp=median(lifeExp))
    mutate(maxGdpPercap=max(gdpPercap))
    
    
#Dla pogrupowanych danych wed�ug kontynentu i roku, oblicz median� z lifeExp i umie�� w 
#zmiennej  medianLifeExp  oraz  warto��  �redni�  z  gdpPercap  i  umie��  w  zmiennej 
#meanGdpPercap.
    
gapminder%>%
  group_by(continent,year)%>%
    mutate(medianLifeExp=median(lifeExp))
    mutate(meanGdpPercap=mean(gdpPercap))
    


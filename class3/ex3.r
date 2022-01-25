#Dla zbioru danych starwars wykonaj nast�puj�ce operacje i przekszta�cenia:

library(dplyr)

glimpse(starwars)

#Wy�wietl wy��cznie Mirialan (species). 

starwars%>%
  filter(species=='Mirialan')  

#Wy�wietl zmienne: nazwa oraz wszystkie zawieraj�ce na ko�cu ��color�.

starwars%>%
  select(name,ends_with('color'))

# Oblicz bmi i umie�� w zmiennej bmi. 

starwars%>%
  mutate(bmi = (mass / (height/100)^2))

#Dla poszczeg�lnych gatunk�w oblicz liczebno�� i zapisz jako n,  �redni� mas� i zapisz jako 
#mass,  maksymalny  wzrost  i  zapisz  jako  height.  Nast�pnie  wybierz  te  wyniki  dla  kt�rych 
#liczebno�� jest r�wna lub wy�sza od 2 oraz masa jest wi�ksza od 60.

starwars%>%
  group_by(species)%>%
    summarize(n=n(),mass=mean(mass),height=max(height))%>%
      filter(n>=2,mass>60)

#Wy�wietl wy��cznie wiersz 13 i 14 z danych.

slice(starwars,13:14)

#Wy�wietl informacje o nazwach, gatunku i planecie pochodzenia.

starwars%>%
  select(name,species,homeworld)

#Przypisz do nowej zmiennej na_species obserwacje, dla kt�rych species nie zawiera informacji 
#wraz z informacjami o nazwie, rasie i planecie pochodzenia . 

na_species = starwars %>%
    filter(is.na(species)) %>%
      select(name, species, homeworld)

na_species

#Policz liczb� obserwacji dla ka�dej p�ci.

starwars %>%
  count(gender)

#Wy�wietl informacje (nazwa, kolor sk�ry, kolor oczu, p�e�) o osobach o jasnym kolorze sk�ry 
#i br�zowym kolorze oczu i posortuj wed�ug p�ci.

starwars %>%
  select(name,skin_color,eye_color,gender)%>%
    filter(skin_color=="light" & eye_color=="brown") %>%
      arrange(gender)

#Znajd� informacje o funkcji slice_sample a nast�pnie wylosuj 5 obserwacji ze zbioru.    
slice_sample(starwars,n=5)

#Wy�wietl informacje o osobach, kt�re maj� wprowadzony wzrost i posortuj od najwy�szych 
#warto�ci. 
starwars %>%
  filter(!is.na(height)) %>%
  arrange(desc(height))

#Zmodyfikuj  dane:  utw�rz  w  obr�bie  starwars  now�  zmienn�  height_m,  kt�ra  b�dzie 
#przechowywa� wzrost podany w metrach.
starwars %>%
  mutate(height_m=height/100)


#Dla zbioru danych starwars wykonaj nastêpuj¹ce operacje i przekszta³cenia:

library(dplyr)

glimpse(starwars)

#Wyœwietl wy³¹cznie Mirialan (species). 

starwars%>%
  filter(species=='Mirialan')  

#Wyœwietl zmienne: nazwa oraz wszystkie zawieraj¹ce na koñcu ‘’color”.

starwars%>%
  select(name,ends_with('color'))

# Oblicz bmi i umieœæ w zmiennej bmi. 

starwars%>%
  mutate(bmi = (mass / (height/100)^2))

#Dla poszczególnych gatunków oblicz liczebnoœæ i zapisz jako n,  œredni¹ masê i zapisz jako 
#mass,  maksymalny  wzrost  i  zapisz  jako  height.  Nastêpnie  wybierz  te  wyniki  dla  których 
#liczebnoœæ jest równa lub wy¿sza od 2 oraz masa jest wiêksza od 60.

starwars%>%
  group_by(species)%>%
    summarize(n=n(),mass=mean(mass),height=max(height))%>%
      filter(n>=2,mass>60)

#Wyœwietl wy³¹cznie wiersz 13 i 14 z danych.

slice(starwars,13:14)

#Wyœwietl informacje o nazwach, gatunku i planecie pochodzenia.

starwars%>%
  select(name,species,homeworld)

#Przypisz do nowej zmiennej na_species obserwacje, dla których species nie zawiera informacji 
#wraz z informacjami o nazwie, rasie i planecie pochodzenia . 

na_species = starwars %>%
    filter(is.na(species)) %>%
      select(name, species, homeworld)

na_species

#Policz liczbê obserwacji dla ka¿dej p³ci.

starwars %>%
  count(gender)

#Wyœwietl informacje (nazwa, kolor skóry, kolor oczu, p³eæ) o osobach o jasnym kolorze skóry 
#i br¹zowym kolorze oczu i posortuj wed³ug p³ci.

starwars %>%
  select(name,skin_color,eye_color,gender)%>%
    filter(skin_color=="light" & eye_color=="brown") %>%
      arrange(gender)

#ZnajdŸ informacje o funkcji slice_sample a nastêpnie wylosuj 5 obserwacji ze zbioru.    
slice_sample(starwars,n=5)

#Wyœwietl informacje o osobach, które maj¹ wprowadzony wzrost i posortuj od najwy¿szych 
#wartoœci. 
starwars %>%
  filter(!is.na(height)) %>%
  arrange(desc(height))

#Zmodyfikuj  dane:  utwórz  w  obrêbie  starwars  now¹  zmienn¹  height_m,  która  bêdzie 
#przechowywaæ wzrost podany w metrach.
starwars %>%
  mutate(height_m=height/100)


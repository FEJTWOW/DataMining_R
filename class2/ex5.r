linkedin <- c(16, 9, 13, 5, 2, 17, 14) 

for (li in linkedin) { 
  if (li > 10) { 
    print("niez³y wynik!") 
  } else { 
    print("slabo!") 
  } 
}

nyc <- list(pop = 8405837,  
            boroughs = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"),  
            capital = FALSE) 

# wersja 1 
for(p in nyc){ 
  print(p) 
}

for(i in 1:length(nyc)){ 
  print(nyc[[i]]) 
}

for (i in mtcars$disp)
{
  if (i>=160)
  {
    print(i)
  } 
  else
  {
    break
  }  
}

rivers

for (i in rivers)
{
  if (i<500)
  {
    print("krotka rzeka");
  }
  else if (i>2000)
  {
    print("dluga rzeka");
  }
  else
  {
    print(i)
  }
  
}
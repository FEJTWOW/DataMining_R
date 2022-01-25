medium <- "UPEL" 
obecnosci <- 14 

if (medium == "UPEL") {
  print("Obecnosci UPEL") 
} else if (medium == "MS TEAMS") { 
  print("Obecnosci MS TEAMS") 
} else { 
  print("Inne zrodla") 
} 

if (obecnosci > 15) { 
  print("Bardzo dobra frekwencja") 
} else if (obecnosci <= 15 & obecnosci > 10) { 
  print("Dobra frekwencja") 
} else { 
  print("Slaba frekwencja") 
} 


studenci.df = data.frame( imie = c("Anna", "Ewa", 
                                   "Henryk", "Jan"), 
                          plec = c("k", "k", "m", "m"), 
                          wiek = c(21,31,29,19)); 
studenci.df 

studenci.df$m.mlodsi= ifelse(studenci.df$plec == "m" & 
                               studenci.df$wiek < 20, "T", "F") 
studenci.df

#prawda
#nieprawda (duzy)
#nieprawda (bardzo niski)
#prawda

v = c(1,7, 8,12,14,19)
res = ifelse(v %% 2 == 0, "Parzyste","Nieparzyste")
res

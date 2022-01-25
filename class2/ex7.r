x = 64

while(TRUE)
{
  if (x>80)
  {
    break;
  }
  else if (x<=30)
  {
    break;
  }
  cat("Twoja predkosc to ",x,"\n")
  if (x>48)
  {
    print("zwolnij mocno")
    x = x-11
  }
  else
  {
    print("zwolnij")
    x = x-6
  }
}


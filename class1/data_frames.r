planets = data.frame(
  name = c("Mercury","Venus","Earth","Mars","Jupiter","Saturn","Uranus","Neptune"),
  type = c("Terrestrial planet","Terrestrial planet","Terrestrial planet","Terrestrial planet","Gas giant","Gas giant","Gas giant","Gas giant"),
  diameter = c(0.382,0.949,1.000,0.532,11.209,9.449,4.007,3.883),
  rotation = c(58.64,-243.02,1.00,1.03,0.41,0.43,-0.72,0.67),
  rings = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE)
)

mars_diameter = planets$diameter[which(planets$name == "Mars")]
mars_diameter
uranus_data = planets[which(planets$name == "Uranus"),]
uranus_data

class(planets)
str(planets)

planets$rings


planets_with_rings = planets[which(planets$ring == TRUE),]
planets_with_rings

planets_with_small_delimeter = planets[which(planets$diameter < 1),]
planets_with_small_delimeter


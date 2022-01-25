ankieta<-c("M","K","K","M","M") 
f_ankieta<-factor(ankieta) 
levels(f_ankieta)<-c("Kobieta","Mężczyzna") 
f_ankieta 
summary(f_ankieta)

my_vector <- 1:10  
my_matrix <- matrix(1:9, ncol = 3) 
my_df <- mtcars[1:10,] 
my_list <-list(my_vector, my_matrix, my_df) 
my_list


ankieta = c("medium","slow","slow","medium","fast")
l = c("slow","medium","fast")
speed_factor = factor(ankieta,levels=l, ordered=TRUE);
speed_factor
summary(speed_factor)

sf2 = speed_factor[2];
sf2
sf5 = speed_factor[5];
sf5

if (sf2 > sf5)
{
  print("sf2 > sf5")
} else
{
  print("sf5 >= sf2")
}

title = c("The Shining")
actors = c("Jack Nicholson", "Shelley Duvall", "Danny Lloyd", "Scatman Crothers", "Barry Nelson" )
reviews = factor(c("Good", "OK", "Good", "Perfect", "Bad", "Perfect","Good"), levels=c("Bad","OK","Good","Perfect"), ordered=TRUE)

my_list = list(title,actors,reviews);
names(my_list) = c("title","actors","reviews")
my_list$actors
my_list$actors[2]


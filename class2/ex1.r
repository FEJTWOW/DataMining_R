mycount<-function(x){sum(x<=15,na.rm=TRUE)}  
x1<-c(2.4, 3.4, 8, 9.2, NA, 8, 29, 35) 
mycount(x1) 

my_equation<-function(x, y,z){ 
  result<-x+y^z 
  print(result) 
} 
my_equation(1,2,3) 

change_m = function(x,type){
  if(type =="stopy"){
    val = x*3.280840;
    print(val);
  }
  else if(type == "cale"){
    val = x*0.03;
    print(val);
  }
}

m = function(x){
  val = x-mean(x)
  print(x)
  print(val)
}


m(c(1,2))
change_m(1,"stopy")

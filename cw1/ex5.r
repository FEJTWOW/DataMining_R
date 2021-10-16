attendance_list = data.frame(
  day = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", 
        "Sunday"  
  ),
  num = c(25,30,16,20,10,21,12)
)
attendance_list$num[which(attendance_list$day=="Monday")]
attendance_list$num[which(attendance_list$day=="Friday")]
attendance_list$day[which(attendance_list$num==max(attendance_list$num))]
attendance_list$day[which(attendance_list$num==min(attendance_list$num))]
as.integer(mean(attendance_list$num))
length(attendance_list$day[which(attendance_list$num<=20)])

perfect_attendance_list = data.frame(
  day = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", 
          "Sunday"  
  ),
  num = c(25,30,18,20,15,21,15)
)
attendance_list$day[which(attendance_list$num == perfect_attendance_list$num)]
attendance_list$day[which(attendance_list$num <= as.integer(perfect_attendance_list$num/2))]
x3 = c(5,8,NA,91,3,NA,14,30,100)
result = x3[which(x3 > 15)]
result


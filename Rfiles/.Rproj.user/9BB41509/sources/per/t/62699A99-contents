basa = read.csv('C:/Users/Lenovo/Desktop/Seatwork Midterm/midtermseatwork_data.csv')
basa_data<-function(basa,min=25,max=70) {
  sub<- ifelse(basa$Ozone>min & basa$Temp>max,basa$Wind,NA)
  mean(sub,na.rm = TRUE)
}
basa_data(basa)



real<- function(basa, Month=9,Day=8){
  col_num<-ncol(basa)
  for (element in 1:nrow(basa)) {
    col_num[element]<- ifelse(basa[element,5]==Month & basa[element,6]==Day,basa[element,4],NA)
  }
  mean(col_num,na.rm = TRUE)
}

real(basa)



num<-1
while(num==1){

  sub10 = subset(basa, Month == 5 & !is.na(Ozone), select = Ozone)
  x<-apply(sub10, 2, min)
  print(x)
  sub11 = subset(basa, Month == 6 & !is.na(Ozone), select = Ozone)
  y<-apply(sub11, 2, min)
  print(y)
  sub12 = subset(basa, Month == 7 & !is.na(Ozone), select = Ozone)
  z<-apply(sub12, 2, min)
  print(z)
  sub13 = subset(basa, Month == 8 & !is.na(Ozone), select = Ozone)
  a<-apply(sub13, 2, min)
  print(a)
  sub14 = subset(basa, Month == 9 & !is.na(Ozone), select = Ozone)
  b<-apply(sub14, 2, min)
  print(b)

  num<-num+1



}



num<-1
repeat{
  sub1=subset(basa,Ozone>25 & Temp>70,select = Wind)
  x<-apply(sub1,2,mean)

  print(x)
  num<-num+1

  if (num>1) {
    break
  }
}


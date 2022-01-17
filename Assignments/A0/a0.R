# Exercise 1
#2
install.packages(c("Hmisc","gdata","boot","xtable","MASS","moments","snow","mvtnorm"))
#3
setwd("C:/Users/alans/Documents/613/A0")
#4
dir()
ls()
#5
678%%9==0
#6
save.image()
#7
?mean
??cut2
#8
log(-1)
#Exercise 2
#1
Titanic
#(a)
sum(Titanic)
#(b)
sum(Titanic[,,"Adult",])
#(c)
sum(Titanic["Crew",,,])
#(d)
sum(Titanic["3rd",,"Child",])
#(e)
sum(Titanic["2nd","Female","Adult",])
#(f)
sum(Titanic["1st","Male","Child",])
#(g)
sum(Titanic["Crew","Female",,"Yes"])
#(h)
sum(Titanic["1st","Male","Adult","Yes"])
#2
#(a)
prop.table(Titanic["1st","Male","Adult",])
#(b)
prop.table(Titanic["1st","Female","Adult",])
#(c)
prop.table(Titanic["1st","Male","Child",])
#(d)
prop.table(Titanic["3rd","Female","Adult",])
#Exercise 3
#1
#(a)
a <- c(1:50)
a <- rev(c(50:1))
a <- seq(from=1,to=50)
#(b)
b <- c(50:1)
b <- rev(c(1:50))
b <- seq(from=50,to=1)
#2
#(a)
a <- rep(c(10,19,7),times=15)
#(b)
b <- rep(c(1,2,5,6),times=8)
#3
a <- seq(from=3.1,to=6,by=0.1)
log(a)*sin(a)
#4
mean(sample(c(0:100),90,replace=F))
mean(sample(c(0:100),90,replace=T))
#5
#(a)
sum=0
for (a in c(1:20)){
  for (b in c(1:15)){
    sum=sum+exp(sqrt(a))*log(a**5)/(5+cos(a)*sin(b))
}}
sum
#(b)
sum=0
for (a in c(1:20)){
  for (b in c(1:a)){
    sum=sum+exp(sqrt(a))*log(a**5)/(5+exp(a*b)*cos(a)*sin(b))
}}
sum
#6
a <- seq(from=3,to=6,by=0.1)
exp(a)*cos(a)
#Exercise 4
#1
xVec=sample(c(0:999),1000,replace=T)
yVec=sample(c(0:999),1000,replace=T)
#2
#(a)
zVec=yVec[-1]-xVec[-1000]
#(b)
wVec=sin(yVec[-1000])/cos(xVec[-1])
#(c)
subX=xVec[xVec>=200]
#(d)
which(yVec>=600)
#Exercise 5
#1
A=matrix(c(1,5,-2,1,2,-1,3,6,-3),nrow=3)
#(a)
A^3
#(b)
cbind(A,A[,1]+A[,3])
#(c)
A[3,]=A[1,]+A[2,]
#(d)
rowMeans(A)
colMeans(A)
#2
A=matrix(c(2,1,1,1,1,3,3,1,2,10,6,13),nrow=3)
#3
solve(A[,1:3],A[,4])
#Exercise 6
#1
fun1=function(a,n){
  i=1:n
  return(sum(a^i/i))
}
#2
fun2=function(x){
  if(x<0){
    return(x^2+2*x+abs(x))
  }else if(x>=0&x<2){
    return(x^2+3+log(1+x))
  }else{
    return(x^2+4*x-14)
  }
}
fun2(-3)
fun2(0)
fun2(3)
#Exercise 7
#1
v1=sample(1:20,36,replace=T)
#2
v1[2:36]
v1[-1]
#3
v2=v1>5
as.integer(v2)
#4
ml=matrix(v1,nrow=6,byrow=T)
#5
x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))
#6
is.na(x)
is.infinite(x)
x[!(is.na(x)+is.infinite(x))]
#Exercise 8
#1
install.packages("AER")
library(AER)
data("GSOEP9402",package="AER")
dat=GSOEP9402
#2
typeof(dat)
nrow(dat)
ncol(dat)
colnames(dat)
#3
library(dplyr)
library(ggplot2)
ggplot(dat%>%group_by(year)%>%summarize(average_income=mean(income)),aes(x=year,y=average_income))+geom_line()
#4
gender=dat%>%group_by(gender)%>%summarize(average_income=mean(income))
school=dat%>%group_by(school)%>%summarize(average_income=mean(income))        
memployment=dat%>%group_by(memployment)%>%summarize(average_income=mean(income))
list(gender,school,memployment)
#Exercise 9
#1
data("CASchools",package="AER")
dat1=CASchools
#2
reg1=lm(read~.-math,dat1)

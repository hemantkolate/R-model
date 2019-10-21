data1=readRDS("loan_data_ch1.rds")
#install.packages("gmodels")
head(data)
tail(data)
CrossTable(data1$home_ownership)
library(gmodels)
CrossTable(data1$loan_status)
CrossTable(data$home_ownership,data$loan_status,prop.r=TRUE,
           prop.c=FALSE,prop.t = FALSE,prop.chisq = FALSE)
data=CrossTable(data1$grade,data1$loan_status,prop.r=TRUE,
           prop.c=FALSE,prop.t = FALSE,prop.chisq = FALSE)
data_relationship=data$prop.row[,2]
#plotting it
position=data_relationship/2
text(x=barplot(data_relationship),labels = names(data_relationship),y=position)
title("The worse the grade,the higher the dafault probability")

difference=diff(data_relationship)
plot(difference,type="b",xlab="grades",ylab="Changes in Default probability",xaxtx="n")
axis(1,at=1:6,labels=names(difference))
title("Probability of default is changing dramatically from E/F and D/F")

library(xts)
data_locf=na.locf(data1)
head(data_locf)

data_nocb=na.locf(data1,fromLast = TRUE)
head(data_nocb[1:6])

data1$int_bin=rep(NA,length(data1$int_rate))

data1$int_bin[which(data1$int_rate<=7)]="0-7"
data1$int_bin[which(data1$int_rate>7 & data1$int_rate<=9)]="7-9"
data1$int_bin[which(data1$int_rate>9 & data1$int_rate<=11)]="9-11"
data1$int_bin[which(data1$int_rate>11 & data1$int_rate<=13.5)]="11-13.5"
data1$int_bin[which(data1$int_rate>13.5)]="13.5+"
data1$int_bin[which(is.na(data1$int_rate))]="Missing"
data1$int_bin=as.factor(data1$int_bin)
data1$int_bin
plot(data1$int_bin)

data1$emp_bin[which(data1$emp_length<=1)]="0-1"
data1$emp_bin[which(data1$emp_length >1 & data1$emp_length<=3)]="1-3"
data1$emp_bin[which(data1$emp_length >3 & data1$emp_length<=7)]="3-7"
data1$emp_bin[which(data1$emp_length>7)]="7+"
data1$emp_bin[which(is.na(data1$emp_length))]="Missing"
data1$emp_bin=as.factor(data1$emp_bin)
plot(data1$emp_bin)


#removing older ones
data1$int_rate=NULL
data1$emp_length=NULL

plot(data1$age, ylab="age")
index_highage=which(data1$age>100)
data1=data1[-index_highage,]

max(data1$age)
plot(data1$annual_inc,ylab="Annual Income")

index_highincome=which(data1$annual_inc>1000000)
data1=data1[-index_highincome,]

training=data1[1:20000,]
test=data1[20000:29092,]

model_age=glm(loan_status~age,family="binomial",data=training)
model_age

model_all=glm(loan_status~.,family="binomial",data=training)
summary(model_all)

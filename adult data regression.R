library(dplyr)
data_adult=read.csv("data_adult.csv")
glimpse(data_adult)
continuous=select_if(data_adult,is.numeric)
summary(continuous)

ggplot(continuous,aes(x=hours.per.week))+geom_density(alpha=0.2,fill="blue")
boxplot(continuous$age)

top_one_percent=quantile(data_adult$hours.per.week,.98)
top_one_percent
#filter out top one percent
data_adult_drop=data_adult %>% filter(hours.per.week<top_one_percent)
dim(data_adult_drop)

data_adult_rescale=data_adult_drop %>% mutate_if(is.numeric,funs(as.numeric(scale(.))))
head(data_adult_rescale)
ggplot(data_adult_rescale,aes(x=hours.per.week))+geom_density(alpha=0.2,fill="blue")

factor=data.frame(select_if(data_adult_rescale,is.factor))
ncol(factor)

graph=lapply(names(factor),function(x)
  ggplot(factor,aes(get(x)))+geom_bar()+theme(axis.text.x=element_text(angle=90)))
graph[1]

recast_data=data_adult_rescale %>% select(-x) %>% mutate(education=factor(ifelse(education=="Preschool" | education=="10th" | education=="11th"|education=="12th"|education=="1st-4th"|education=="5th-6th"|education=="7th-8th"|education=="9th","dropout",
                                                                                 ifelse(education=="HS-grad","HighGrad",
                                                                                        ifelse(education=="Some-College"|education=="Assoc-acdm"|education=="Assoc-voc","community",
                                                                                               ifelse(education=="Bachelors","Bachelors",
                                                                                                      ifelse(education=="Masters"|education=="Prof-School","Master","Phd")))))))
recast_data %>% group_by(education) %>% summarize(average_educ_year=mean(educational.num),
                                                  count=n()) %>% arrange(average_educ_year)                                                      

recast_data_1=recast_data %>% mutate(marital.status=factor(ifelse(marital.status=="Never-married"|marital.status=="Married-spouse-absent","Not-married",
                                                                  ifelse(marital.status=="Married-AF-spouse"|marital.status=="Married-civ-spouse","Married",
                                                                         ifelse(marital.status=="Seperated"|marital.status=="Divorced","Seperated","Widow")))))
table(recast_data_1$marital.status)

ggplot(recast_data_1,aes(x=gender,fill=income))+geom_bar(position="fill")+theme_classic()

ggplot(recast_data_1,aes(x=race, fill=income))+geom_bar(position = "fill")+theme_classic()+theme(axis.text.x = element_text(angle=90))

ggplot(recast_data_1,aes(x=gender,y=hours.per.week))+geom_boxplot()+stat_summary(fun.y=mean,
                                                                                 geom="point",
                                                                                 size=3,
                                                                                 color="steelblue")+theme_classic()

ggplot(recast_data_1,aes(x=hours.per.week))+geom_density(aes(color=education),alpha=0.5)+theme_classic()


ggplot(recast_data_1,aes(x=age,y=hours.per.week))+geom_point(aes(color=income),size=0.5)+stat_smooth(method='lm',formula=y~poly(x,1),
                                                                                                     se=TRUE,aes(color=income))+theme_classic()

library(GGally)
#convert data to numeric
corr=data.frame(lapply(recast_data_1,as.integer))
#plot the graph
ggcorr(corr,method=c("pairwise","spearman"),
       nbreaks = 6,hjust=0.8,label=TRUE,label_size=3,color="grey50")

set.seed(1234)
create_train_test=function(data,size=0.8,train=TRUE){
  n_row=nrow(data)
  total_row=size*n_row
  train_sample=1:total_row
  if(train==TRUE){
    return(data[train_sample,])
  }else{
    return(data[-train_sample,])
  }
}

data_train=create_train_test(recast_data_1,0.8,train=TRUE)
data_test=create_train_test(recast_data_1,0.8,train=FALSE)
dim(data_train)
dim(data_test)

formula=income~.
logit=glm(formula,data=data_train,family='binomial')
summary(logit)

predict=predict(logit,data_test,type='response')
table_mat=table(data_test$income,predict>0.5)
table_mat
accuracy_test=sum(diag(table_mat))/sum(table_mat)
accuracy_test

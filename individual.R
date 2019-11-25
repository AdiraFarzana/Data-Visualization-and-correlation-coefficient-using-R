concrete1=read.csv(file.choose(),header=T)
str(concrete1)
head(concrete1)
attach(concrete1)
library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())

names(concrete1)
[1] "cement"                        "slag"                         
[3] "fly_ash"                       "water"                        
[5] "superplasticizer"              "coarse_aggregate"             
[7] "fine_aggregate"                "day"                          
[9] "concrete_compressive_strength"

ggplot(concrete1, aes(x = cement, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

ggplot(concrete1, aes(x = slag, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

ggplot(concrete1, aes(x = fly_ash, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

ggplot(concrete1, aes(x = water, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

ggplot(concrete1, aes(x = superplasticizer, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

ggplot(concrete1, aes(x = coarse_aggregate, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

ggplot(concrete1, aes(x = fine_aggregate, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

ggplot(concrete1, aes(x = day, y = concrete_compressive_strength)) +
  geom_point() +
  stat_smooth()

cor(concrete1$concrete_compressive_strength, concrete1$day)
[1] 0.328873

cor(concrete1$concrete_compressive_strength, concrete1$fine_aggregate)

cor(concrete1$concrete_compressive_strength, concrete1$coarse_aggregate)

cor(concrete1$concrete_compressive_strength, concrete1$superplasticizer)

cor(concrete1$concrete_compressive_strength, concrete1$water)

cor(concrete1$concrete_compressive_strength, concrete1$fly_ash)

cor(concrete1$concrete_compressive_strength, concrete1$slag)

cor(concrete1$concrete_compressive_strength, concrete1$cement)

cor.test(concrete1$cement,concrete1$concrete_compressive_strength, method = "pearson")
par(mfrow=c(1,1))
ggscatter(concrete1, x = "cement", y = "concrete_compressive_strength", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cement", ylab = "concrete_compressive_strength")
shapiro.test(concrete1$concrete_compressive_strength)
ggqqplot(concrete1$concrete_compressive_strength, ylab = "concrete_compressive_strength")


model <- lm(concrete_compressive_strength ~ cement, data = concrete1)

ggplot(concrete1, aes(cement, concrete_compressive_strength)) +
  geom_point() +
  stat_smooth(method = lm)

summary(model)

confint(model)

prediction:-
library(caret)
set.seed(123)

training.samples <- concrete1$concrete_compressive_strength %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- concrete1[training.samples, ]

test.data <- concrete1[-training.samples, ]
concrete2=data.frame(concrete1,list=FALSE)

model2=lm(concrete2~concrete_compressive_strength, data = train.data)
summary(model2)$coef

summary(model)
par(mfrow=c(2,2))
plot(model)
summary(cement)
x1=350
x1=as.data.frame(x1)
colnames(x1)="day"
predict(model,x1)
pred.int <- predict(model,x1,interval="pred")
#pred.int <- predict(model, interval = "prediction")
mydata <- cbind(concrete1, pred.int)
p <- ggplot(mydata, aes(cement, concrete_compressive_strength)) +
  geom_point() +
  stat_smooth(method = lm)
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y = upr), color = "red", linetype = "dashed")


concrete.n=concrete1[,1:9]
attach(concrete.n)
lm3=lm(concrete_compressive_strength~.,data=concrete.n)
lm3
summary(lm3)
summary(model)
library(leaps)
choice=regsubsets(concrete_compressive_strength~.,data=concrete.n,nbest=1,nvmax=9)
choice
plot(choice,scale="bic")
summary(choice)
summary(choice)$which[which.min(summary(choice)$bic),]

#best.model <- lm(concrete_compressive_strength ~ cement+slag+fly_ash+water+superplasticizer+day, data = concrete.n)
summary(best.model)

lm0=lm(concrete_compressive_strength~1,data=concrete.n)
lm0
step(lm0,~cement+slag+fly_ash+water+superplasticizer+coarse_aggregate+fine_aggregate+day)
lm4=step(lm0,~cement+slag+fly_ash+water+superplasticizer+coarse_aggregate+fine_aggregate+day,trace=F)
summary(lm4)

lm5=lm(concrete_compressive_strength ~ cement+slag+fly_ash+water+superplasticizer+day,data=concrete.n)
summary(lm5)
drop1(lm5)
step(lm0,~cement+slag+fly_ash+water+superplasticizer+day,trace=F)

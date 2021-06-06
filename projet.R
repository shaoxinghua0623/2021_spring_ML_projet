rm(list = ls())

# 读取训练集和测试机数据
X.adult <- read.table("F:\\机器学习\\Projet\\adult.data",header=FALSE,sep=",",fill=FALSE,strip.white=T,na.strings="?")

# 加标签
#TARGET = "income_class"
names(X.adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status','occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss','hours_per_week', 'native_country', 'income_class')

#################################################################################################
#################################################################################################

# 查看缺省值分布图
library(mice)
library("VIM") 
#md.pattern(X.adult)
aggr(X.adult, prop = T, numbers = T)#用比例代替计数

# 删除缺省值
#adult.train<-na.omit(adult.train)
#adult.test<-na.omit(adult.test)

#################################################################################################
#################################################################################################

# 删除变量 “fnlwgt” “education”
X.adult[["fnlwgt"]]=NULL
X.adult[["education"]]=NULL

# 查看每个属性的分布情况
#plot(X.adult[1:3000,])
par(mfrow = c(1,4))
boxplot(X.adult$age, main="boxplot age")
boxplot(X.adult$capital_gain, main="boxplot capital_gain")
boxplot(X.adult$capital_loss, main="boxplot capital_loss")
boxplot(X.adult$hours_per_week, main="boxplot hours_per_week")

par(mfrow = c(2,4))
barplot(prop.table(table(X.adult$workclass)), main = "barplot workclass")
barplot(prop.table(table(X.adult$marital_status)), main = "barplot marital_status")
barplot(prop.table(table(X.adult$education)), main = "barplot education")
barplot(prop.table(table(X.adult$occupation)), main = "barplot occupation")
barplot(prop.table(table(X.adult$relationship)), main = "barplot relationship")
barplot(prop.table(table(X.adult$race)), main = "barplot race")
barplot(prop.table(table(X.adult$sex)), main = "barplot sex")
barplot(prop.table(table(X.adult$native_country)), main = "barplot native_country")

#################################################################################################
#################################################################################################

# 删除一些数据
X.adult.caploss= X.adult[X.adult$capital_loss!=0,]
X.adult.capgain= X.adult[X.adult$capital_gain!=0,]
X.adult = rbind(X.adult.caploss, X.adult.capgain)

par(mfrow = c(1,2))
boxplot(X.adult$capital_gain, main="boxplot capital_gain")
boxplot(X.adult$capital_loss, main="boxplot capital_loss")

max <- quantile(X.adult$capital_gain,0.75, na.rm=TRUE) + (IQR(X.adult$capital_gain, na.rm=TRUE) * 3)
X.adult <- X.adult[X.adult$capital_gain < as.numeric(max),]

par(mfrow = c(1,2))
boxplot(X.adult$capital_gain, main="boxplot capital_gain")
boxplot(X.adult$capital_loss, main="boxplot capital_loss")

#################################################################################################
#################################################################################################

# 有些预测器里的变量个数太多了，合并一些
X.adult$workclass = as.character(X.adult$workclass)
X.adult$occupation = as.character(X.adult$occupation)
X.adult$native_country = as.character(X.adult$native_country)
X.adult$race = as.character(X.adult$race)
X.adult$marital_status = as.character(X.adult$marital_status)

X.adult$marital_status[X.adult$marital_status=="Never-married"] = "Never-Married"
X.adult$marital_status[X.adult$marital_status=="Married-AF-spouse"] = "Married"
X.adult$marital_status[X.adult$marital_status=="Married-civ-spouse"] = "Married"
X.adult$marital_status[X.adult$marital_status=="Married-spouse-absent"] = "Not-Married"
X.adult$marital_status[X.adult$marital_status=="Separated"] = "Not-Married"
X.adult$marital_status[X.adult$marital_status=="Divorced"] = "Not-Married"
X.adult$marital_status[X.adult$marital_status=="Widowed"] = "Widowed"

X.adult$native_country[X.adult$native_country=="Cambodia"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Canada"] = "British-Commonwealth"     
X.adult$native_country[X.adult$native_country=="China"] = "China"        
X.adult$native_country[X.adult$native_country=="Columbia"] = "South-America"     
X.adult$native_country[X.adult$native_country=="Cuba"] = "Other"         
X.adult$native_country[X.adult$native_country=="Dominican-Republic"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Ecuador"] = "South-America"      
X.adult$native_country[X.adult$native_country=="El-Salvador"] = "South-America"  
X.adult$native_country[X.adult$native_country=="England"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="France"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Germany"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Greece"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Guatemala"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Haiti"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Holand-Netherlands"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Honduras"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Hong"] = "China"
X.adult$native_country[X.adult$native_country=="Hungary"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="India"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="Iran"] = "Other"
X.adult$native_country[X.adult$native_country=="Ireland"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="Italy"] = "Euro_1"
X.adult$native_country[X.adult$native_country=="Jamaica"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Japan"] = "Other"
X.adult$native_country[X.adult$native_country=="Laos"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Mexico"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Nicaragua"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Peru"] = "South-America"
X.adult$native_country[X.adult$native_country=="Philippines"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Poland"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Portugal"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Puerto-Rico"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="Scotland"] = "British-Commonwealth"
X.adult$native_country[X.adult$native_country=="South"] = "Euro_2"
X.adult$native_country[X.adult$native_country=="Taiwan"] = "China"
X.adult$native_country[X.adult$native_country=="Thailand"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Trinadad&amp;Tobago"] = "Latin-America"
X.adult$native_country[X.adult$native_country=="United-States"] = "United-States"
X.adult$native_country[X.adult$native_country=="Vietnam"] = "SE-Asia"
X.adult$native_country[X.adult$native_country=="Yugoslavia"] = "Euro_2"

X.adult$workclass = gsub("^Federal-gov","Federal-Govt",X.adult$workclass)
X.adult$workclass = gsub("^Local-gov","Other-Govt",X.adult$workclass)
X.adult$workclass = gsub("^State-gov","Other-Govt",X.adult$workclass)
X.adult$workclass = gsub("^Private","Private",X.adult$workclass)
X.adult$workclass = gsub("^Self-emp-inc","Self-Employed",X.adult$workclass)
X.adult$workclass = gsub("^Self-emp-not-inc","Self-Employed",X.adult$workclass)
X.adult$workclass = gsub("^Without-pay","Not-Working",X.adult$workclass)
X.adult$workclass = gsub("^Never-worked","Not-Working",X.adult$workclass)

X.adult$occupation = gsub("^Adm-clerical","Admin",X.adult$occupation)
X.adult$occupation = gsub("^Armed-Forces","Military",X.adult$occupation)
X.adult$occupation = gsub("^Craft-repair","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Exec-managerial","White-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Farming-fishing","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Handlers-cleaners","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Machine-op-inspct","Blue-Collar",X.adult$occupation)
X.adult$occupation = gsub("^Other-service","Service",X.adult$occupation)
X.adult$occupation = gsub("^Priv-house-serv","Service",X.adult$occupation)
X.adult$occupation = gsub("^Prof-specialty","Professional",X.adult$occupation)
X.adult$occupation = gsub("^Protective-serv","Other-Occupations",X.adult$occupation)
X.adult$occupation = gsub("^Sales","Sales",X.adult$occupation)
X.adult$occupation = gsub("^Tech-support","Other-Occupations",X.adult$occupation)
X.adult$occupation = gsub("^Transport-moving","Blue-Collar",X.adult$occupation)

X.adult$race[X.adult$race=="White"] = "White"
X.adult$race[X.adult$race=="Black"] = "Black"
X.adult$race[X.adult$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
X.adult$race[X.adult$race=="Asian-Pac-Islander"] = "Asian"
X.adult$race[X.adult$race=="Other"] = "Other"

X.adult$marital_status = factor(X.adult$marital_status)
X.adult$native_country = factor(X.adult$native_country)
X.adult$workclass = factor(X.adult$workclass)
X.adult$occupation = factor(X.adult$occupation)
X.adult$race = factor(X.adult$race)
X.adult$sex = factor(X.adult$sex)
X.adult$relationship = factor(X.adult$relationship)

par(mfrow = c(2,4))
barplot(prop.table(table(X.adult$workclass)), main = "barplot workclass")
barplot(prop.table(table(X.adult$marital_status)), main = "barplot marital_status")
barplot(prop.table(table(X.adult$occupation)), main = "barplot occupation")
barplot(prop.table(table(X.adult$relationship)), main = "barplot relationship")
barplot(prop.table(table(X.adult$race)), main = "barplot race")
barplot(prop.table(table(X.adult$sex)), main = "barplot sex")
barplot(prop.table(table(X.adult$native_country)), main = "barplot native_country")
barplot(prop.table(table(X.adult$income_class)), main = "barplot income_class")

#################################################################################################
#################################################################################################

# 插补缺失值
#library(lattice) #调入函数包
#library(MASS)
#library(nnet)
summary(X.adult[,c(2,5,12)])
library(mice)
X.adult.mice <- mice(X.adult, m=5, maxit=50, meth='pmm', seed=500)
X.adult.complete = complete(X.adult.mice)
summary(X.adult.complete)

#################################################################################################
#################################################################################################

# 可视化
library(FactoMineR)
PCA = PCA(X = X.adult.complete, quali.sup = c(2,4,5,6,7,8,12,13))
biplot(PCA$quali.sup$coord[c("<=50K", ">50K"),],PCA$var$coord, xlim = c(-0.6,0.6), ylim = c(-0.6,0.6), main = "biplot of PCA")
abline(h=0,v=0,col="gray")
MCA = MCA(X = X.adult.complete[,c(2,4,5,6,7,8,12,13)], quali.sup = 8)

#################################################################################################
#################################################################################################

n <- nrow(X.adult.complete)
#split <- 1:N
ntrain <- round(n*3/4)
ntest <-n-ntrain
train <- sample(n,ntrain)
X.adult.train <- X.adult.complete[train,]
X.adult.test <- X.adult.complete[-train,]

#################################################################################################
#################################################################################################

# Naive Bayes
library(naivebayes)
fit.nb<- naive_bayes(as.factor(income_class)~.,data=X.adult.train)
pred.nb<-predict(fit.nb,newdata=X.adult.test,type="class")
perf <-table(X.adult.test$income_class,pred.nb)
print(perf)
err.nb <-1-sum(diag(perf))/ntest  # error rate

#################################################################################################
#################################################################################################

# LDA
library(MASS)
fit.lda<- lda(income_class~.,data=X.adult.train)
pred.lda<-predict(fit.lda,newdata=X.adult.test)
perf <- table(X.adult.test$income_class,pred.lda$class)
print(perf)
err.lda<-1-sum(diag(perf))/ntest  # error rate

#################################################################################################
#################################################################################################

# Logistic regression
fit.logreg<- glm(as.factor(income_class)~.,data=X.adult.train,family=binomial)
pred.logreg<-predict(fit.logreg,newdata=X.adult.test,type='response')
perf <-table(X.adult.test$income_class,pred.logreg>0.5)
print(perf)
err.logreg <-1-sum(diag(perf))/ntest  # error rate

#################################################################################################
#################################################################################################

# Random Forest
library(randomForest)
fit.RF<-randomForest(as.factor(income_class) ~ .,data=X.adult.train,mtry=4,importance=TRUE)
pred.RF<-predict(fit.RF,newdata=X.adult.test,type="class")
perf <-table(X.adult.test$income_class,pred.RF)
print(perf)
err.RF <-1-sum(diag(perf))/ntest  # error rate
#err.RF<-1-mean(X.adult.test$income_class==pred.RF)
#print(err.RF)

#################################################################################################
#################################################################################################

# Neural Networks
library(nnet)
set.seed(20210606)
fit.nnet <- nnet(as.factor(income_class) ~ ., data = X.adult.train , size=5, maxit=6000, decay=0.04)
pred.nnet = predict(fit.nnet, X.adult.test, type = "class")
perf <- table(Truth=X.adult.test$income_class, Preds=pred.nnet)
print(perf)
err.nnet = 1 - sum(perf[row(perf)==col(perf)])/sum(perf)

#################################################################################################
#################################################################################################

#Decision Tree
library(rpart)
tree.dec<-rpart(income_class~.,data=X.adult.train,subset=train,
                method="class" )

plot(tree.dec,margin = 0.1)
text(tree.dec,pretty=0,cex=0.8)

pred<-predict(tree.dec,newdata=X.adult.test,type="class")
y.test <- X.adult.test$income_class
table(y.test,pred)
err.dt<-1-mean(y.test==pred)
#print(err.dt)

#################################################################################################
#################################################################################################

# 错误率可视化
print(c(err.nb,err.lda,err.logreg,err.RF,err.nnet,err.dt))
H <- c(err.nb,err.lda,err.logreg,err.RF,err.nnet,err.dt)
M <- c("err.nb","err.lda","err.logreg","err.RF","err.nnet","err.dt")
bar<-barplot(H,names.arg=M,xlab="method",ylab="error rate",col=heat.colors(6),main="error rate of different methods",border="white")

#################################################################################################
#################################################################################################

# 对random forest进行10折交叉验证，得到更真实的误差
library(TunePareto)
k <- 10
CV.folds <- generateCVRuns(X.adult.complete$income_class, ntimes=1, nfold=k, stratified=TRUE)

#准备结构以存储部分结果
# "TR error” traing error
# "VA error" validation error
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k

# let's compute the 10-fold CV errors
for (j in 1:k)
{
  # get VA data
  va <- unlist(CV.folds[[1]][[j]])
  # train on TR data
  fit.RF <- randomForest(as.factor(income_class) ~ ., data = X.adult.complete[-va,], ntree=300, proximity=FALSE)
  # predict TR data
  pred.va = predict(fit.RF)
  tab <- table(X.adult.complete[-va,]$income_class, pred.va)
  cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  # predict VA data
  pred.va <- predict(fit.RF, newdata = X.adult.complete[va,])
  tab <- table(X.adult.complete[va,]$income_class, pred.va)
  cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  cv.results[j,"fold"] <- j
}
cv.results
plot(x = cv.results[,2], y = cv.results[,3], ylim = c(0,0.1), ylab = "error", xlab = "kfold", col = 2)
lines(x = cv.results[,2], y = cv.results[,3], col = 2)
par(new=T)
plot(x = cv.results[,2], y = cv.results[,4], ylim = c(0,0.1), ylab = "error",  xlab = "kfold", col = 3)
lines(x = cv.results[,2], y = cv.results[,4], col = 3)
legend("bottomright", c("TR error","VA error"), pch=20, col=c(2:3))

# 求出平均值
(VA.error <- mean(cv.results[,"VA error"]))

## Now a 95% CI around it
dev <- sqrt(VA.error*(1-VA.error)/ntest)*1.967
sprintf("(%f,%f)", VA.error-dev,VA.error+dev)

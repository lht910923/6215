# 6215
---
title: "Multivariate_analysis"
author: "Chunmei Gao"
date: "November 29, 2015"
output: html_document
---

```{r}
library(corrplot)
twin <- read.csv("~/Documents/6215-Applied Multivariance Model/project/nmtwins.csv")
#View(twin)
str(twin)
dim(twin)
```

```{r}
#seperate the data into two groups
identical <- twin[twin$zygosity==1,]
fraternal <- twin[twin$zygosity==2,]

# describe the data:

hist(twin$moed)
hist(twin$faed)
hist(twin$faminc)
hist(twin$english)

# checking the fraternal twin sex
for(i in 1:839)
twin[which(twin$zygosity==2),2]

# data cleaning
# missing value:


```

```{r}
# test multivariate normal distribution: QQ plot(for sigle variable)  Chi-square (for multi variables)

source("http://www.stat.wmich.edu/wang/561/codes/R/chisqplot.R")
chisqplot(twin[,7:11]) 
# a few points above the line indicate large distances or outlying observations

qqnorm(twin$english)
qqline(twin$english, col="red")

qqnorm(twin$math)
qqline(twin$math, col="red")

qqnorm(twin$socsci)
qqline(twin$socsci, col="red")

qqnorm(twin$natsci)
qqline(twin$natsci, col="red")

qqnorm(twin$vocab)
qqline(twin$vocab, col="red")
# from the qq plot for each score variables, we didint see a significant diviation from normal distribution.
## ??why the multi-normal has some divations? correlation? outlier?

```


```{r}
# PCA
#library(corrplot)
corrplot(cor(twin[,7:11]),method="number")
fit <- princomp(twin[,7:11], cor=TRUE)
summary(fit)
loadings(fit)
fit$scores[1:10,]
plot(fit,type="lines")
```
#================================================================================
#LiHongtao's Code
#================================================================================
setwd("/Users/Lihongtao/Desktop/6215 Multivariate Statistical Analysis/Project")
data1 = read.csv("nmtwins.csv")  # read csv file 
score = read.csv("scores.csv")
family=data1[,2:6]
attach(data1)
summary(data1)
dim(data1)
n=1678
mean=apply(score,2,mean)
#================================================================================

# Detecting Outliers
# step1. One dimenstional dot diagram
dotchart(data1$english,labels=row.names(english),cex=.7,main="Dot Polt", xlab="")
stripchart(score)
#--------------------------------------------------------------------------------

# step2. Sctplot matrix 
plot(score,main="Simple Scatterplot Matrix") # Matrix
#--------------------------------------------------------------------------------

# step3. z
z.eng=rep(NA,1678)
for (i in 1:n)
  {z.eng[i]=(english[i]-mean(english))/sqrt(sd(english))}
z.eng

z.math=rep(NA,1678)
for (i in 1:n)
{z.math[i]=(math[i]-mean(math))/sqrt(sd(math))}
z.math

z.soc=rep(NA,1678)
for (i in 1:n)
{z.soc[i]=(socsci[i]-mean(socsci))/sqrt(sd(socsci))}
z.soc

z.nat=rep(NA,1678)
for (i in 1:n)
{z.nat[i]=(natsci[i]-mean(natsci))/sqrt(sd(natsci))}
z.nat

z.vocab=rep(NA,1678)
for (i in 1:n)
{z.vocab[i]=(vocab[i]-mean(vocab))/sqrt(sd(vocab))}
z.vocab
#--------------------------------------------------------------------------------

# step4. squared generalized distances
S=cov(score)
S.inv=solve(S)
ssd=matrix(1:n,nrow=1)
for(i in 1:n)
{ssd[i]=as.matrix(score[i,]-mean)%*%as.matrix(S.inv)%*%t(score[i,]-mean)}
ssd
ssdsort=sort(ssd)
# Chi-square plots
qchi=c(1:n)
for(i in 1:n)
{qchi[i]=qchisq(p=((i-1/2)/n),df=5)}
plot(ssdsort~qchi)
#================================================================================

#CI
source("http://www.stat.wmich.edu/wang/561/codes/R/ci.R")
confidence(n=1678,xbar=mean,S=S, conf.region=T,alpha=.05)


score=as.matrix(score)
family=as.matrix(family)
fit=lm(score~family)
summary(fit)

library(outliers)
plot(fit,which=4)

chisq.out.test(score,variance=var(score), opposite=FALSE)
#================================================================================

#*************************************************
#*******  R语言多元统计分析案例分析(第五版) ******
#*******      数据来自: mvcase5.xls'    **********
#*******       王斌会:   2020.1          *********
#*************************************************

##########案例1 如何使用Rstudio及做作业##########
#系统设置
options(digits=4)                 #输出结果位数
par(mar=c(4,4,2,1)+0.1,cex=0.8)   #输出图形修饰
#setwd('F:/R/mvstats5')           #设置工作目录
source('msaR.r')                  #调入自定义函数

#随机生成身高数据
set.seed(1)
X=rnorm(60,170,8);X
summary(X)
hist(X) 

library(openxlsx)                #加载读取数据包
Case1=read.xlsx('mvcase5.xlsx','Case1')
summary(Case1)
hist(Case1$height)

##########案例2 多元数据的基本统计分析############
Case2=read.xlsx('mvcase5.xlsx','Case2'); head(Case2)
summary(Case2)
T1=table(Case2$地区);T1
barplot(T1,col=1:4)
f=hist(Case2$月收入);f
boxplot(月收入~性别,Case2)
t.test(月收入~性别,Case2)
T2=table(Case2$性别,Case2$观点);T2
barplot(T2,beside=T,horiz = T,col=1:2)
T3=ftable(Case2$性别,Case2$教育程度,Case2$观点);T3
barplot(T3,beside=T,col=1:3)
T4=ftable(Case2$教育程度,Case2$性别,Case2$观点);T4
barplot(T4,beside=T,col=1:3)
rm(f,T1,T2,T3,T4)

##########案例3 城市现代化水平的直观分析##########
Case3=read.xlsx('mvcase5.xlsx','Case3',rowNames = T);head(Case3)
summary(Case3)
boxplot(Case3)
rm=apply(Case3,1,mean);rm
barplot(rm,las=3) #按行做均值图
cm=apply(Case3,2,mean);cm
barplot(cm,horiz=T) #按列做均值图
stars(Case3,key.loc=c(8,2))
stars(Case3,draw.segments=T,key.loc=c(8,2))
library(aplpack)  
faces(Case3)
#library(msaR)
msa.andrews(Case3)

##########案例4 财政收入的相关与回归分析##########
Case4=read.xlsx('mvcase5.xlsx','Case4');
summary(Case4)
cor(Case4)   #相关分析
plot(Case4,gap=0)  #矩阵散点图
msa.cor.test(Case4)
fm=lm(y~.,data=Case4);summary(fm)
sfm=step(fm);summary(sfm)
plot(Case4$y);lines(sfm$fitted)


##########案例5 广义与一般线性模型及应用##########
Case5=read.xlsx('mvcase5.xlsx','Case5');head(Case5)
fm=glm(y~sex+age,family=binomial,data=Case5)
summary(fm)
fv=fm$fitted.values
P=exp(fv)/(1+exp(fv))
cbind(Case5$y,P)
plot(Case5$age,P)

##########案例6 企业财务状况的判别分析##########
Case6=read.xlsx('mvcase5.xlsx','Case6');Case6
plot(Case6[,2:5],gap=0)
library(MASS)
ld=lda(G~.,data=Case6);ld  #线性判别
plot(ld)
Zld=predict(ld)
data.frame(Case6$G,Zld$class,round(Zld$x,3))
tab1=table(Case6$G,Zld$class);tab1
sum(diag(tab1))/sum(tab1)
addmargins(tab1)

qd=qda(G~.,data=Case6);qd  #二次判别
Zqd=predict(qd)
#data.frame(Case5$G,Zqd$class,round(Zqd$post,3)*100)
tab2=table(Case6$G,Zqd$class);tab2
sum(diag(tab2))/sum(tab2)
addmargins(tab2)

##########案例7 区域经济区划的聚类分析##########
Case7=read.xlsx('mvcase5.xlsx','Case7',rowNames=TRUE);head(Case7)
summary(Case7)
Z=scale(Case7)             #数据标准化
boxplot(Z)
hc=hclust(dist(Z),'ward.D2')      #系统聚类法     
plot(hc);rect.hclust(hc,2);cutree(hc,2)#分2类
plot(hc);rect.hclust(hc,3);cutree(hc,3)#分3类
plot(hc);rect.hclust(hc,4);cutree(hc,4)#分4类

# kmenas聚类法
kmeans(Z,2)$cluster   #分2类
kmeans(Z,3)$cluster   #分3类
kmeans(Z,4)$cluster   #分4类


##########案例8 电信业发展的主成分分析##########
Case8=read.xlsx('mvcase5.xlsx','Case8',rowNames = TRUE);Case8
plot(hclust(dist(scale(Case8)))) #系统聚类图     
#library(msaR) #source('msaR.r')
msa.pca(Case8,cor=T) #主成分分析   

##########案例9 上市公司经营业绩评价的因子分析########
Case9=read.xlsx('mvcase5.xlsx','Case9',rowNames = TRUE);
(FA0=factanal(Case9,4,rotation="none")) #因子不旋转  
pairs(FA0$loadings)
(FA1=factanal(Case9,4,rot="varimax")) #varimax法旋转
pairs(FA1$loadings)
FA2=msa.fa(Case9,4);FA2
pairs(FA2$scores)
biplot(FA2$scores,FA2$loadings) #前2个因子信息重叠图

##########案例10：对应分析在农民收入中的应用 ########
Case10=read.xlsx('mvcase5.xlsx','Case10',rowNames=T);Case10
C10d.1=Case10[1:6,]   #文化程度数据
chisq.test(C10d.1) 
C10d.2=Case10[7:13,]  #总收入数据
chisq.test(C10d.2) 

library(ca)
Ca1=ca(C10d.1);summary(Ca1)
Ca1$rowcoord[,1:2]
Ca1$colcoord[,1:2]
plot(Ca1)

Ca2=ca(C10d.2);summary(Ca2)
Ca2$rowcoord[,1:2]
Ca2$colcoord[,1:2]
plot(Ca2)

##########案例11 农村居民收入和支出典型相关分析########
Case11=read.xlsx('mvcase5.xlsx','Case11')
round(cor(Case11),3)
plot(Case11,gap=0)
Z=scale(Case11)
ca=cancor(Z[,1:4],Z[,5:9]);ca
#library(msaR)
msa.cancor(Z[,1:4],Z[,5:9],plot=T)

##########案例12 各地区工资水平的多维标度分析##########
Case12=read.xlsx('mvcase5.xlsx','Case12',rowNames = T)
D=dist(Case12);
mds=isoMDS(D,k=2);mds
plot(mds$points);abline(h=0,v=0,lty=3)
text(mds$points,row.names(Case12),cex=0.8)

##########案例13 区域自主创新能力的层次分析##########
### 权重计算-AHP法
A=c(1,3,3,1,1/3,1,2,1/3,1/3,1/2,1,1/3,1,3,3,1)
(A_W=msa.AHP(A))   #A的权重
B1=c(1,1,1,2,1,1,1,1,1,1,1/2,2,1,2,1,1,1,1/2,1,
     2,2,1/2,2,2,1,1,1,1,1,1/2,1,1,1,2,2,1,1,
     1/2,1,1/2,1,2,1,1/2,1/2,1,1/2,1/2,1)
(B1_W=msa.AHP(B1)) #B1的权重
B2=c(1,1,1,2,1,1,1,1,1,1/2,2,1,1,1,1,1,1,2,
     1/2,2,1,1,1,1,1,1/2,1,1,1,2,1,1,1/2,1,1/2,1)
(B2_W=msa.AHP(B2)) #B2的权重
B3=c(1,1,2,2,1,1,1,1,2,2,1/2,1,1,1/2,1/2,
     1/2,1/2,2,1,1/3,1,1/2,2,3,1)
(B3_W=msa.AHP(B3)) #B3的权重
B4=c(1,1/2,1,1,1,1/2,2,1,1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1)
(B4_W=msa.AHP(B4)) #B4的权重
### 综合评价---层次分析
Case13=read.xlsx('mvcase5.xlsx','Case13',rowNames=T);
fx<-function(x,a=40,b=60) (x-min(x))/(max(x)-min(x))*b+a   
Z=apply(Case13,2,fx)
S1=Z[,1:7]%*%B1_W
S2=Z[,8:13]%*%B2_W
S3=Z[,14:18]%*%B3_W
S4=Z[,19:24]%*%B4_W
S=cbind(S1,S2,S3,S4)%*%A_W;
R=rank(-S)
data.frame(S1=S1,R1=rank(-S1),S2=S2,R2=rank(-S2),
           S3=S3,R3=rank(-S3),S4=S4,R4=rank(-S4),S=S,R=R)
B=barplot(S[,1],las=3);text(B,R,labels=R,pos=3)



#**************************************************
#********  多元统计分析及R语言建模（第五版）*******
#********  代码文件: mvstats5.R"            *******
#********  数据文件: mvstats5.xlsx"         *******
#********  修改时间：王斌会 2020.2.1      *******
#**************************************************
setwd("E:/mvstats5") #设置目录

source('msaR.R')                   #加载自定函数
#library(msaR)                     #加载自定义包  
library(openxlsx)                  #加载读取Excel数据包

#【输出设置】
options(digits=4)
par(mar=c(5,5,2,1))

# 第1章 多元统计分析概述 -----------------------
A=matrix(1:20,5,4); A              #生成5行4列的矩阵
X=rnorm(50); round(X,4)            #产生50个标准正态随机数
.iplot(); hist(X,prob=TRUE)        #做数据的直方图
lines(density(X),col='red')        #添加密度函数曲线 

# 第2章 多元数据的数学表达 ---------------------
## —— 2.3  数据矩阵及R表示 ----
#### 创建一个向量
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)
length(x1)                         #向量的长度
mode(x1)                           #数据的类型
a=1:9; a
b=c(1,3,6:4,9); b

A=matrix(c(1,4,2,5,3,6),nrow=2,ncol=3); A   #生产2行3列的矩阵
#A=matrix(c(1,4,2,5,3,6),2,3); A   
B=matrix(c(1,2,3,4,5,6),3,2); B             #生产3行2列的矩阵
t(A)            #求矩阵转置

A[,1:2]+B[1:2,] #矩阵加法
A[,2:3]+B[2:3,] #矩阵减法
C=A%*%B;C       #矩阵相乘
D=B%*%A;D       #矩阵相乘

diag(D)         #获得矩阵对角线元素
I=diag(3);I     #创建3阶单位矩阵
solve(C)        #求矩阵的逆

D.e=eigen(D,symmetric=T); D.e  #求矩阵的特征值与特征向量

D.e$vectors%*%diag(D.e$values)%*%t(D.e$vectors)
#特征向量矩阵U和特征值矩阵D与原矩阵A的关系A=UDU'
(A.c=chol(A))            #矩阵的Choleskey分解
t(A.c)%*%A.c             #Choleskey分解矩阵V与原矩阵A.c的关系A.c=V'V
(A=matrix(1:18,3,6))     #创建矩阵
(A.s=svd(A))             #矩阵的奇异值分解
A.s$u%*%diag(A.s$d)%*%t(A.s$v) #矩阵的奇异值分解结果与原矩阵A的关系A=UDV'
(A=matrix(1:16,4,4))     #创建矩阵
qr(A)                    #矩阵的QR分解
(A=matrix(1:4,2,2))      #创建矩阵
(B=matrix(rep(1,4),2,2)) #创建矩阵
kronecker(A,B)           #矩阵的kronecker积
A=matrix(1:12,3,4)       #创建矩阵
dim(A)      #矩阵的维数
nrow(A)     #矩阵的行数
ncol(A)     #矩阵的列数
sum(A)      #矩阵求和
mean(A)     #矩阵求均值
rowSums(A)  #矩阵按行求和
rowMeans(A) #矩阵按行求均值
colSums(A)  #矩阵按列求和
colMeans(A) #矩阵按列求均值
## —— 2.4  数据框及R表示 ----
X=data.frame(x1,x2);X               #产生由X1和X2构建的数据框
Y=data.frame('身高'=x1,'体重'=x2);Y #赋予数据框新的列标签
rbind(x1,x2) #按行合并
cbind(x1,x2) #按列合并
head(X)      #输出X的前6行
tail(X)      #输出X的后6行

Xrs=apply(X,1,sum);Xr #按行求和
Xcs=apply(X,2,sum);Xc #按列求和
cbind(X,'行合计'=Xrs) #列合并
rbind(X,'列合计'=Xcs) #行合并 
## —— 2.5  多元数据的R调用 ----
#在Excel文件mvstats5.xlsx的表单d2.1中选择A1:G515，并复制到剪切板
data=read.table("clipboard",header=T) #将剪切板数据读入数据框data中

write.csv(X,file = 'test1.csv')        #保存数据
read.csv("test1.csv")                  #读取数据  
write.csv(X,file = 'test2.csv',row.names = F)  #保存数据
read.csv("test2.csv")                  #读取数据
## —— 2.6  多元数据简单R分析 ----
#d2.1=read_excel('mvstats5.xlsx',sheet='d2.1') #读取mvstats5.xlsx表格d2.2数据
d2.1=read.xlsx('mvstats5.xlsx','d2.1') #读取mvstats5.xlsx表格d2.1数据
head(d2.1);tail(d2.1)                  #显示数据的前6行和后6行

hist(x1)      #做出身高的直方图
plot(x1,x2)   #做出身高和体重的散点图

attach(d2.1)  #绑定数据
table(年龄)   #一维列联表
barplot(table(年龄),col=1:7)   #条形图
pie(table(结果))               #饼图
table(年龄,性别)               #二维列联表
barplot(table(年龄,性别),beside=T,col=1:7)  #以性别分组的年龄条图
barplot(table(性别,年龄),beside=T,col=1:2)  #以年龄分组的性别条图
ftable(年龄,性别,结果)       #以年龄、性别排列的结果频数三维列联表
ftable(性别,年龄,结果)       #以性别、年龄排列的结果频数三维列联表
(ft=ftable(性别,结果,年龄))  #显示以性别、结果排列的年龄频数三维列联表
rowSums(ft)  #求行和
colSums(ft)  #求列和
sum(ft)      #求总和

# 第3章 多多元数据直观表示 -------------------------
## —— 3.1  直观分析 ----
## 数据整理 
d3.1=read.xlsx('mvstats5.xlsx','d3.1',rowNames=T);d3.1 #读取mvstats5.xlsx表格d3.1数据
## —— 3.2  条图及R使用 ----
barplot(apply(d3.1,1,mean))         #按行做均值条形图
barplot(apply(d3.1,1,mean),las=3)   #按行做均值条形图
barplot(apply(d3.1,2,mean))         #按列做均值图条形图
barplot(apply(d3.1,2,mean),col=1:8) #按列做彩色均值图条形图

barplot(apply(d3.1[,2:8],2,mean))      #去掉第一列后的数据按列做均值条形图
barplot(apply(d3.1,2,median))          #按列取色
barplot(apply(d3.1,2,median),col=1:8)  #按列取色
## —— 3.3  箱尾图及R使用 ----
boxplot(d3.1)                    #按列做箱线图
boxplot(d3.1,horizontal=T,las=1) #箱线图中图形按水平放置
## —— 3.4  星相图及R使用 ----
stars(d3.1)                      #具有图例的360度星相图
stars(d3.1,key.loc=c(13,1.8))    #具有图例的360度星相图
stars(d3.1,full=F,key.loc=c(13,1.5))                 #具有图例的180度星相图
stars(d3.1,draw.segments=T,key.loc=c(13,1.7))        #具有图例的360度彩色圆形星相图
stars(d3.1,full=F,draw.segments=T,key.loc=c(13,1.5)) #具有图例的180度彩色圆形星相图
## —— 3.5  脸谱图及R使用 ----
library(aplpack)                 #加载aplpack包
faces(d3.1,ncol.plot=7)          #做脸谱图
faces(d3.1[,2:8],ncol.plot=7)    #去掉第一个变量按每行7个做脸谱图
faces(d3.1[c(1,9,19,28,29,30),]) #选择第1,9,19,28,29,30个观测的多元数据做脸谱图
# install.packages("TeachingDemos") #安装TeachingDemos包
library("TeachingDemos")         #加载TeachingDemos包
faces2(d3.1,ncols=7)             #TeachingDemos::faces(d3.1)
## —— 3.6  调和曲线及R使用 ----
# install.packages("andrews")    #安装andrews包
library(andrews)                 #加载andrews包
andrews(d3.1,clr=5,ymax=6)       #绘制调和曲线图
#选择第1,9,19,28,29,30个观测的多元数据做调和曲线图
andrews(d3.1[c(1,9,19,28,29,30),],clr=5,ymax=6) 

library(msaR)        #加载自定义msaR
msa.andrews(d3.1)   #绘制调和曲线图
#选择第1,9,19,28,29,30个观测的多元数据做调和曲线图
plot.andrews(d3.1[c(1,9,19,28,29,30),])

# 第4章 多元相关与回归分析 ------------------------
## —— 4.1  变量之间的关系分析 ----
### ———— 4.1.1 简单相关分析的R计算----
x=c(171,175,159,155,152,158,154,164,168,166,159,164)   #身高
y=c(57,64,41,38,35,44,41,51,57,49,47,46)               #体重
par(mar=c(5,4,2,1))   #设定图距离画布边缘的距离：下5，左4，上2，右1
plot(x,y)             #做散点图
lxy<-function(x,y) sum(x*y)-sum(x)*sum(y)/length(x)  #建立离均差乘积和函数
lxy(x1,x1)    #x1的离均差平方和
lxy(x2,x2)    #x1的离均差平方和
lxy(x1,x2)    #x1的离均差乘积和
(r=lxy(x1,x2)/sqrt(lxy(x1,x1)*lxy(x2,x2))) #显示用离均差乘积和计算的相关系数
cor(x,y)      #计算相关系数

n=length(x1)  #向量的长度
tr=r/sqrt((1-r^2)/(n-2))  #相关系数假设检验t统计量
tr
cor.test(x,y) #相关系数假设检验
### ———— 4.1.2 简单回归分析的R计算----
x=x1  #自变量,数据来自例2.2
y=x2  #因变量,数据来自例2.2
b=lxy(x,y)/lxy(x,x)    #线性回归方程斜率
a=mean(y)-b*mean(x)    #线性回归方程截距
c(a=a,b=b)             #显示线性回归方程估计值
plot(x,y)              #做散点图
lines(x,a+b*x)         #添加估计方程线
SST=lxy(y,y)           #因变量的离均差平方和
SSR=b*lxy(x,y)         #回归平方和
SSE=SST-SSR            #误差平方和
MSR=SSR/1              #回归均方
MSE=SSE/(n-2)          #误差均方
F= MSR/MSE             #F统计量
c(SST=SST,SSR=SSR,SSE=SSE,MSR=MSR,MSE=MSE,F=F) #显示结果
sy.x=sqrt(MSE)               #估计标准差
sb=sy.x/sqrt(lxy(x,x))       #离均差平方和
t=b/sb                       #t统计量
ta=qt(1-0.05/2,n-2)          #t分位数
c(sy.x=sy.x,sb=sb,t=t,ta=ta) #显示结果

d4.3=read.xlsx('mvstats5.xlsx','d4.3',rowNames=T);d4.3  #读取mvstats5.xlsx表格d4.3数据
fm=lm(y~x,data=d4.3)  #一元线性回归模型
fm                    #显示回归结果
summary(lm(x2~x1))    #左回归检验和系数检验
plot(y~x,data=d4.3)   #做散点图
abline(fm)            #添加回归线
anova(fm)             #模型方差分析
summary(fm)           #回归系数t检验
## —— 4.2  多元线性回归分析 ----
### ———— 4.2.1 多元线性回归模型的建立----
d4.4=read.xlsx('mvstats5.xlsx','d4.4',rowNames=T);d4.4  #读取mvstats5.xlsx表格d4.4数据
plot(d4.4,gap=0)      #两两变量的散点图，参数gap为每个小图形之间的距离
pairs(d4.4,gap=0)
fm=lm(y~x1+x2+x3+x4,data=d4.4);fm   #建立多元回归模型

coef.sd<-function(fm){  #标准回归系数
  b=fm$coef;b
  si=apply(fm$model,2,sd);si
  bs=b[-1]*si[-1]/si[1]
  bs
}
coef.sd(fm)       #标准回归系数
### ———— 4.2.2 多元线性回归模型检验----
anova(fm)           #多元线性回归模型方差分析
summary(fm)         #多元线性回归系数t检验
data.frame(summary(fm)$coef,bstar=c(NA,coef.sd(fm)))    
summary(fm)$fstat   #提取F统计量的相关信息

## —— 4.3  多元相关分析 ----
### ———— 4.3.1 矩阵相关分析----
cor(d4.4)           #多元数据相关系数矩阵
pairs(yX)           #多元数据散点图
msa.cor.test(d4.4)  #多元数据相关系数检验
### ———— 4.3.2 复相关分析----
(R2=summary(fm)$r.sq) #显示多元线性回归模型决定系数
(R=sqrt(R2))          #显示多元数据复相关系数
## —— 4.4  回归变量的选择方法 ----
### ———— 4.4.1 变量选择标准----
library(leaps)    #加载leaps包
varsel=regsubsets(y~x1+x2+x3+x4,data=yX)  #多元数据线性回归变量选择模型
result=summary(varsel)   #变量选择方法结果           
data.frame(result$outmat,RSS=result$rss,R2=result$rsq)  #RSS和决定系数准则结果展示 
data.frame(result$outmat,adjR2=result$adjr2,Cp=result$cp,BIC=result$bic)
#调整决定系数,Cp和BIC准则结果展示
### ———— 4.4.1 逐步回归分析----
fm=lm(y~x1+x2+x3+x4,data=d4.4)         #多元数据线性回归模型
fm.step=step(fm,direction="forward")   #向前引入法变量选择结果
fm.step=step(fm,direction="backward")  #向后剔除法变量选择结果
fm.step=step(fm,direction="both")      #逐步筛选法变量选择结果
#summary(lm(y~x1+x2+x3+I(x3^2)+x4+I(x4^2),data=yX))
summary(fm.step)                       

# 第5章 广义及一般性模型 ##########################
## —— 5.2  广义线性模型 ----
logistic <- function(x){ #定义logistic函数
  exp(x)/(1+exp(x))
}
par(mar=c(4,4,2,1))  #设定图距离画布边缘的距离：下4，左4，上2，右1
# 画logistic曲线图
curve(logistic,-4,4,col = 'green',lwd= 2, yaxt="n", xaxt="n", cex.axis = 0.9, xlab = NA, ylab = NA ) 
axis(side = 2, at = seq(0,1,by=0.1), labels = seq(0,1,by=0.1), cex.axis = 0.9,las=2)  # 修改Y坐标轴
axis(side = 1, at = seq(-4,4,by=1), labels = seq(-4,4,by=1), cex.axis = 0.9)          # 修改X坐标轴

## ———— 5.2.2  Logistic模型 ----
d5.1=read.xlsx('mvstats5.xlsx','d5.1');head(d5.1)     #读取mvstats5.xlsx表格d5.1数据
logit.glm<-glm(y~x1+x2+x3,family=binomial,data=d5.1)  #Logistic回归模型
summary(logit.glm)                                    #Logistic回归模型结果
logit.step<-step(logit.glm,direction="both")          #逐步筛选法变量选择
summary(logit.step)                                   #逐步筛选法变量选择结果
pre1<-predict(logit.step,data.frame(x1=1))            #预测视力正常司机Logistic回归结果
p1<-exp(pre1)/(1+exp(pre1))                           #预测视力正常司机发生事故概率
pre2<-predict(logit.step,data.frame(x1=0))            #预测视力有问题的司机Logistic回归结果
p2<-exp(pre2)/(1+exp(pre2))                           #预测视力有问题的司机发生事故概率
c(p1,p2)                                              #结果显示
## ———— 5.2.3 对数线性模型 -----------------------
d5.2=read.xlsx('mvstats5.xlsx','d5.2');head(d5.2)        #读取mvstats5.xlsx表格d5.2数据
#log.glm<-glm(y~x1*x2,family=poisson(link=log),data=d5.2) #多元对数线性模型
log.glm<-glm(y~factor(x1)*factor(x2),family=poisson,data=d5.2) #多元对数线性模型
summary(log.glm)                               #多元对数线性模型结果

library(MASS)
Lm=loglm(y~x1+x2,data=d5.2);
summary(Lm)

## —— 5.3  一般线性模型 ----
## ———— 5.3.1 完全随机设计模型 ----------------------
d5.3=read.xlsx('mvstats5.xlsx','d5.3');head(d5.3) #读取mvstats5.xlsx表格d5.3数据
anova(lm(Y~factor(A),data=d5.3))                  #模型的方差分析
## ———— 5.3.2 随机区组设计模型 ---------------------
d5.4=read.xlsx('mvstats5.xlsx','d5.4');head(d5.4) #读取mvstats5.xlsx表格d5.4数据
anova(lm(Y~factor(A)+factor(B),data=d5.4))        #模型的方差分析
## ———— 5.3.3 析因设计模型 ------------------------
d5.5=read.xlsx('mvstats5.xlsx','d5.5');head(d5.5) #读取mvstats5.xlsx表格d5.4数据
anova(lm(Y~factor(A)*factor(B),data=d5.5))        #模型的方差分析
## ———— 5.3.5 正交设计模型 ------------------------
d5.6=read.xlsx('mvstats5.xlsx','d5.6');d5.6       #读取mvstats5.xlsx表格d5.4数据
anova(lm(Y~factor(A)*factor(B),data=d5.6))        #模型的方差分析

# 第6章 判别分析 ----------------------------------
#——6.2线性判别分析----
par(mar=c(1,1,1,1)) #设置画图参数
n1=seq(-4,4,length=1000) #在[-4，4]产生等距1000个数
n2=n1+4
y=dnorm(n1,0,1)  #在n1的基础上生产均值为0方差为1的密度
plot(n1,y,type='l',xlim=c(-4,8),ylim=c(-0.05,0.4),bty='n',xaxt='n',yaxt='n',xlab='',ylab='')#画密度线图
lines(n2,y)#添加第二条密度线
abline(h=0) #添加水平线y=0
lines(c(0,0),c(0,1));lines(c(4,4),c(0,1));lines(c(2,2),c(0,0.1),lty=3) #添加左右两条垂直线
text(0,-0.01,labels=expression(mu[1])) #标注中心
text(4,-0.01,labels=expression(mu[2]))#标注中心
text(-1,0.35,labels=expression(G[1])) #标注线条名称
text(3,0.35,labels=expression(G[2]))#标注线条名称

#——6.2.2 Fisher线性判别求解----
d6.1=read.xlsx('mvstats5.xlsx','d6.1');d6.1 #读取数据
attach(d6.1)       #绑定数据
par(mar=c(3,4,1,1),mfrow=c(1,2))
boxplot(x1~G,d6.1,ylab='湿温差(x1)')
boxplot(x2~G,d6.1,ylab='气温差(x2)') 
plot(x1,x2); text(x1,x2,G,adj=-0.5) #标识点所属类别G
library(MASS)
ld=lda(G~x1+x2,data=d6.1);ld  #线性判别模型
Z=predict(ld)      #根据线性判别模型预测所属类别

newG=Z$class       #预测的所属类别结果
cbind(G,Z$x,newG)  #显示结果
(tab=table(G,newG))#混淆矩阵        
sum(diag(prop.table(tab)))#判对率   
a1=ld$scaling[1,]; a2=ld$scaling[2,] #
#y=a1x1+a2x2 ===> x2=y0/a2-a1/a2x1  
a = 1/a2*(a1*mean(x1)+a2*mean(x2)) #求判别线系数
b =-a1/a2    #求判别线系数
abline(a,b)  #画出判别线
text(x1,x2,newG,adj=-0.5)  #标记出图上点的原始分类结果
plot(ld,type='both')       #画条形图和密度线

#——6.3.2多总体距离判别----
d6.3=read.xlsx('mvstats5.xlsx','d6.3');d6.3 #读取数据
attach(d6.3)  #绑定数据
par(mar=c(4,4,2,1),cex=0.75)    #设置画图参数
plot(Q,C);text(Q,C,G3,adj=-0.5) #画变量Q,C散点图及添加标识
plot(Q,P);text(Q,P,G3,adj=-0.5) #画变量Q,P散点图及添加标识
plot(C,P);text(C,P,G3,adj=-0.5) #画变量C,P散点图及添加标识
library(MASS) #调用R包
qd=qda(G3~Q+C+P);qd #拟合
cbind(G3,newG=predict(qd)$class) #类别预测
predict(qd,data.frame(Q=8,C=7.5,P=65)) #给点Q,C预测类别

ld=lda(G3~Q+C+P);ld #线性拟合
W=predict(ld) #预测
cbind(G3,Wx=W$x,newG=W$class) #合并数据与新类别
predict(ld,data.frame(Q=8,C=7.5,P=65)) #给点Q,C预测类别
options(digits=3) #设定有效数位数

#读取例6.3数据 
d6.3=read.xlsx('mvstats5.xlsx','d6.3');d6.3 #读取数据
attach(d6.3)#绑定数据
plot(Q,C);text(Q,C,G3,adj=-0.5,cex=0.75) #画变量Q,C散点图及添加标识
plot(Q,P);text(Q,P,G3,adj=-0.5,cex=0.75) #画变量Q,P散点图及添加标识       
plot(C,P);text(C,P,G3,adj=-0.5,cex=0.75) #画变量Q,P散点图及添加标识      

ld3=lda(G3~Q+C+P); ld3 #线性拟合
lp3=predict(ld3) #预测
lG3=lp3$class #类别
data.frame(G3,lG3) #生成数据框
(ltab3=table(G3,lG3)) #对比新类别和原始类别
diag(prop.table(ltab3,1)) #准确率
sum(diag(prop.table(ltab3))) #总准确率
plot(lp3$x) #画散点图
text(Z$x[,1],Z$x[,2],G3,adj=-0.8,cex=0.75)
predict(ld,data.frame(Q=8,C=7.5,P=65)) #判定

qd=qda(G2~Q+C+P); qd#线性拟合
Z=predict(qd) # 预测
newG=Z$class #判定类别
cbind(G2,newG)  #比较原类别和判定类别
(tab=table(G2,newG)) #生成原始类别和新类别列联表
sum(diag(prop.table(tab))) #求判定准确率
predict(qd,data.frame(Q=8,C=7.5,P=65)) #判定

(ld1=lda(G2~Q+C+P,prior=c(1,1,1)/3))#先验概率相等的Bayes判别模型
(ld2=lda(G2~Q+C+P,prior=c(5,8,7)/20))#先验概率不相等的Bayes判别模型        
Z1=predict(ld1)#预测所属类别        
cbind(G2,round(Z1$x,3),newG=Z1$class)#显示结果 
Z2=predict(ld2)#预测所属类别
cbind(G2,round(Z2$x,3),newG=Z2$class)#显示结果
table(G2,Z1$class)#混淆矩阵
table(G2,Z2$class)#混淆矩阵
round(Z1$post,3) #ld1模型后验概率
round(Z2$post,3) #ld2模型后验概率
predict(ld1,data.frame(Q=8,C=7.5,P=65))  # ld1模型的判定
predict(ld2,data.frame(Q=8,C=7.5,P=65))  # ld2模型的判定

# 第7章 聚类分析 -----------------------
#——7.2.1距离----
x1=c(2.5,3.0,6.0,6.6,7.2,4.0,4.7,4.5,5.5) #生成向量x1
x2=c(2.1,2.5,2.5,1.5,3.0,6.4,5.6,7.6,6.9) #生成向量x2
plot(x1,x2,xlim=c(1,8),ylim=c(1,8)); text(x1,x2,labels=c(1:9),adj=-0.5) #画x1,x2散点图
draw.circle(x=4.7,y=6.5,r=1,lty=1,border=4) #添加圆圈
draw.circle(x=2.8,y=2.2,r=0.4,lty=1,border=4)#添加圆圈
draw.circle(x=6.7,y=2.5,r=1,lty=1,border=4)#添加圆圈

plot(x1,x2); text(x1,x2,labels=c(1:9),adj=-0.5)  #画x1,x2散点图并添加标识 
X=cbind(x1,x2); #形成数据矩阵
options(digits=3) #设定有效位数
dist(X) #默认为euclidean距离
dist(X,diag=TRUE)  #添加主对角线距离
dist(X,upper=TRUE) #添加上三角距离 
dist(X,diag=TRUE,upper=TRUE) #全矩阵距离

dist(X,method="manhattan")     #manhattan距离
dist(X,method="minkowski",p=1) #manhattan距离
dist(X,method="minkowski",p=2) #euclidean距离

D=dist(X);D #求距离阵
min(D) #求最小距离

#——7.3.3系统积累基本步骤----
hc<-hclust(D);hc #默认最长距离法
cbind(hc$merge,hc$height) #分类过程
par(mar=c(1,4,1,1))
plot(hc) #聚类图
rect.hclust(hc,3) #加3分类框
cutree(hc,1:3) #显示分类结果

hc<-hclust(D,"single");hc #最短距离法
names(hc)
cbind(hc$merge,hc$height) #分类过程
plot(hc) #聚类图

hc<-hclust(dist(X),"ward.D") #ward距离法 
cbind(hc$merge,hc$height) #分类过程
plot(hc) #聚类图        

d3.1=read.xlsx('mvstats5.xlsx','d3.1',rowNames=TRUE);d3.1 #读取数据
plot(d3.1,gap=0) #矩阵散点图
D=dist(d3.1);D
plot(hclust(D,'single')) #最短距离法
plot(hclust(D,'complete')) #最长距离法
plot(hclust(D,'median')) #中间距离法 
plot(hclust(D,'average')) #类平均法        
plot(hclust(D,'centroid')) #重心法        
plot(hclust(D,'ward.D')) #ward.D法
plot(hclust(D,'ward.D2')) #ward.D2法

H=hclust(D,'ward.D2');H
plot(H); rect.hclust(H,2); cutree(H,2) #分两类
plot(H); rect.hclust(H,3); cutree(H,3) #分三类
plot(H); rect.hclust(H,4); cutree(H,4) #分四类

set.seed(123) #设定种子数
#产生均值0,标准差为0.3的100x10的正态随机数矩阵
x1=matrix(rnorm(1000,0,0.3),ncol=10)
#产生均值1,标准差为0.3的100x10的正态随机数矩阵
x2=matrix(rnorm(1000,1,0.3),ncol=10)
#形成200x10的正态随机数矩阵
X=rbind(x1,x2); #按行合并
summary(X) #基本统计

H=hclust(dist(X)) #系统聚类
plot(H); rect.hclust(H,2); #分两类
cutree(H,2) #画两类框
#——7.4快速聚类法----
km=kmeans(X,2)     #kmeans聚类
km$cluster         #分类结果
par(mar=c(4,4,1,1)) #设置画图参数
plot(X,pch=km$cluster) #画分类图

set.seed(123) #设定种子数
x1=matrix(rnorm(10000,0,0.25),ncol=10) #生成矩阵1
x2=matrix(rnorm(10000,1,0.25),ncol=10) #生成矩阵2
X=rbind(x1,x2) #按行合并
km=kmeans(X,2) #kmeans聚类
kc=km$cluster;kc #kmeans聚类结果
plot(X,pch=kc,col=kc) #画聚类结果图

km=kmeans(d3.1,3)     #kmeans聚类
km$cluster         #分类结果

kmeans

# 第8章 主成分分析 ----------------------------------
#——8.1主成分直观解释----
x1=c(147,171,175,159,155,152,158,154,164,168,166,159,164,177) #生成向量x1
x2=c(32,57,64,41,38,35,44,41,54,57,49,47,46,63) #生成向量x2
plot(x1,x2,xlim=c(145,180),ylim=c(25,75)) #画散点图
library(shape) #调用R包
lines(getellipse(24,3,mid=c(162,48),angle=48),lty=3) #添加椭圆边框
lines(c(146,178),c(30,66)); #主成分1
lines(c(162,165),c(53,46)); #主成分2   
cov(x1,x2) #求协方差
cor(x1,x2) #求相关系数

#——8.2.2主成分推导----
X=cbind(x1,x2);X  #按列合并
S=cov(X);S #求协方差阵
R=cor(X);R #求相关系数阵

#——8.2.3主成分性质----
pc=princomp(X);pc #主成分分析
names(pc)  #输出变量名称
pc$sdev^2  #主成分方差
#——8.2.4主成分得分----
plot(pc$scores,asp=1);abline(h=0,v=0,lty=3) #画主成分得分散点图
biplot(pc$scores,pc$loadings);abline(h=0,v=0,lty=3) #画主成分得分散点图和方向

summary(pc) #主成分分析结果展示
pc$loadings # 输出结果

#——8.3主成分分析步骤----

d3.1=read.xlsx('mvstats5.xlsx','d3.1',rowNames=T);d3.1 #读取数据
PC=princomp(d3.1,cor=T)
summary(PC)
PC$loadings[,1:3]

pca=msa.pca(d3.1,cor=TRUE);pca #主成分分析
par(mar=c(5,4,3,1)) #设置画图参数
biplot(pca$scores,pca$loadings);abline(h=0,v=0,lty=3) #画主成分得分散点图

# 第9章 因子分析 -------------------------------
#——9.2.2因子分析模型----
d9.1=read.xlsx('mvstats5.xlsx','d9.1',rowNames=TRUE); d9.1 #读取数据
cor(d9.1) #相关系数阵
#——9.3.1极大似然估计法----
factanal(d9.1,6,rotation='none') #因子未旋转
Fa1=factanal(d9.1,3,rotation='none') #因子未旋转
round(Fa1$loadings[1:6,],4)   #极大似然法方差贡献
1-Fa1$uniquenesses  #极大似然法共同度

#——9.3.2主因子法----
#library(mvstats); source("factpc.R")
factpc(d9.1,6,rotation='none')$loadings #因子得分
Fp1=factpc(d9.1,3,rotation='none') #因子未旋转
Fp1$loadings #因子得分
Fp1$vars  #主因子法方差贡献
Fp1$common  #主因子法共同度

Fp2=factpc(d9.1,3,rotation='varimax'); #因子旋转
Fp2$vars #方差贡献
Fp2$loadings #因子得分

Fa2=factanal(d9.1,3,rotation='varimax')  #因子旋转求解
round(Fa2$loadings[1:6,],4) #因子得分保留4位小数

#——9.4因子旋转----
Fp1$scores ;Fp1 #旋转前因子得分
Fp2$scores; Fp2 #旋转后因子得分

(Fac=factpc(d9.1,3))#主成份法因子分析
(Fa1=factanal(d9.1,3,rot="varimax")) #varimax法旋转因子分析
Fa1=factanal(d9.1,3,scores="regression")#使用回归估计法的极大似然法因子分析
Fa1$scores
Fac1=factpc(d9.1,3,scores="regression")#使用回归估计法的主成份法因子分析
Fac1$scores
factanal.rank(Fa1,plot=T)#因子得分作图与排名
plot(Fp1$scores,asp=1); abline(h=0,v=0,lty=3) #画因子得分图
text(Fp1$scores,labels=rownames(X))#添加标识
plot(Fp2$scores,asp=1); abline(h=0,v=0,lty=3) #画因子得分图
text(Fp2$scores,labels=rownames(X))#添加标识
biplot(Fp2$scores,Fp2$loadings) #前2个因子信息重叠图        

d9.1=read.xlsx('mvstats5.xlsx','d9.1',rowNames=TRUE); d9.1 #读取数据
#library(mvstats)
Fo=msa.fa(d9.1,3,rotation="none"); Fo      #因子未旋转
Fr=msa.fa(d9.1,3,rotation="varimax"); Fr   #因子旋转

#——9.5.2因子得分信息图----
plot(Fr$scores,asp=1);abline(h=0,v=0,lty=3) #画因子得分图
text(Fr$scores,labels = rownames(d9.1)) #添加标识
biplot(Fr$scores,Fr$loading)#信息重叠图
abline(h=0,v=0,lty=3) #添加水平和竖直直线

Fo=msa.fa(d3.1,3,rotation="none"); Fo      #因子未旋转
Fr=msa.fa(d3.1,3,rotation="varimax"); Fr    #因子旋转
data.frame(Fo$scores,apply(-Fo$scores,2,rank))
data.frame(Fr$scores,apply(-Fr$scores,2,rank))

biplot(Fr$scores,Fr$loading)#信息重叠图
biplot(Fr$scores[,1:2],Fr$loading[,1:2]) #信息重叠图
biplot(Fr$scores[,2:3],Fr$loading[,2:3])#信息重叠图
biplot(Fr$scores[,c(1,3)],Fr$loading[,c(1,3)])#信息重叠图

# 第10章 对应分析 ------------------------
# ——10.2.1 列联表分析----
d10.1=read.xlsx('mvstats5.xlsx','d10.1',rowNames=TRUE); d10.1 #读取数据
chisq.test(d10.1) #卡方检验

#——10.3.2对应分析图----
library(ca)
ca1=ca(d10.1);ca1
summary(ca1)
ca1$rowcoord
ca1$colcoord
plot(ca1)

library(MASS)#加载MASS包        
ca1=corresp(d10.1,nf=2) #对应分析        
ca1#对应分析结果
par(mar=c(4,4,1,1))     #设置画图参数
plot(ca1,xlim=c(-0.6,0.6),ylim=c(-0.3,0.3)) #双坐标轴图
abline(v=0,h=0,lty=3) #添加轴线
lines(getellipse(0.1,0.2,mid=c(-0.55,-0.05),angle=90),lty=3)  #添加椭圆
lines(getellipse(0.1,0.2,mid=c(0.1,0.1),angle=90),lty=3)      #添加椭圆
lines(getellipse(0.1,0.3,mid=c(0.35,-0.15),angle=-100),lty=3) #添加椭圆

d10.2=read.xlsx('mvstats5.xlsx','d10.2',rowNames=TRUE); d10.2 #读取数据
ca2=corresp(d10.2,nf=2)  #对应分析 
ca2  #对应分析结果
par(mar=c(4,4,1,1),cex=1)     #设置画图参数
plot(ca2,xlim=c(-1.5,0.6),ylim=c(-1,0.5)) #双坐标轴图
abline(v=0,h=0,lty=3)                     #添加轴线
lines(getellipse(0.3,0.2,mid=c(-1.35,0.25),angle=90),lty=3) #添加椭圆
lines(getellipse(0.5,0.2,mid=c(-0.5,0),angle=0),lty=3)      #添加椭圆
lines(getellipse(0.5,0.3,mid=c(-0.25,-0.65),angle=0),lty=3) #添加椭圆
lines(getellipse(0.4,0.13,mid=c(-0.1,-0.3),angle=0),lty=3)  #添加椭圆
lines(getellipse(0.4,0.13,mid=c(0.25,0.1),angle=40),lty=3)  #添加椭圆


# 第11章 典型相关分析 ---------------------------
### —11.2 相关分析基本架构----------------
### ——一、简单相关分析----------------
d11.1=read.xlsx('mvstats5.xlsx','d11.1'); d11.1 #读取例题数据
cor(d11.1)     #计算变量间相关系数
plot(d11.1,gap=0)    #变量散点图矩阵
### ——二、多变量相关分析----------------
summary(lm(y1~x1+x2+x3,data=d11.1))  #多元回归计算复相关系数

### —11.3 典型相关分析原理----------------
xy=scale(d11.1);   #数据标准化 
ca=cancor(xy[,1:3],xy[,4:6])  #典型相关分析 
ca$cor             #提取典型相关系数
ca$xcoef           #提取x组的典型变量系数
ca$ycoef           #提取y组的典型变量系数

### —11.4 典型相关分析步骤----------------
library(mvstats)      #加载自定义包        
msa.cancor
(d11.1[,1:3],d11.1[,4:6])      #利用自定义函数进行典型相关分析 
#cancor.test(xy[,1:3],xy[,4:6])      

d11.2=read.xlsx('mvstats5.xlsx','d11.2'); d11.2    #读取例题数据
msa.cancor(d11.2[,1:4],d11.2[,5:10],pq=2,plot=T)   #利用自定义函数进行典型相关分析

# 第12章 多维标度分析 -----------------------
### —12.2 MDS 的古典解----------------
#距离矩阵
D=matrix(c(0,1,sqrt(3),2,sqrt(3),1,1,
           1,0,1,sqrt(3),2,sqrt(3),1,
           sqrt(3),1,0,1,sqrt(3),2,1,
           2,sqrt(3),1,0,1,sqrt(3),1,
           sqrt(3),2,sqrt(3),1,0,1,1,
           1,sqrt(3),2,sqrt(3),1,0,1,
           1,1,1,1,1,1,0),nrow=7,ncol=7)      
D
round(cmdscale(D),3)  #多维标度法古典解，保留3位小数
#sqrt(3)/2=0.866

d12.1=read.xlsx('mvstats5.xlsx','d12.1',rowNames=T); d12.1   #读取例题数据
MDS1=cmdscale(d12.1);MDS1  #多维标度法古典解
#x=MDS1[, 1];y=MDS1[, 2]    #降维后的两维度数据
#plot(x,y,type='n',asp=1)   #降维数据作散点图 # asp=1确保欧氏距离正确显示
plot(MDS1[,1],MDS1[,2],type='n',asp=1)           # asp=1确保欧氏距离正确显示
text(MDS1[,1],MDS1[,2],labels=rownames(d12.1))
#text(x,y,labels=rownames(d12.1))  #标记各点对应地区

### —12.3 MDS 非度量方法----------------
library(MASS)   #加载MASS包
D=as.matrix(d12.1)   #将例题数据转化为矩阵
MDS2=isoMDS(D,k=2); MDS2    #多维标度的非度量法求解
x=MDS2$points[,1]; y=MDS2$points[,2]   #降维后的两维度数据
plot(x,y,type="n")    #降维数据作散点图
text(x,y,labels=rownames(D))   #标记各点对应地区

### —12.4 多维标度计算过程----------------
d12.4=read.xlsx('mvstats5.xlsx','d12.4',rowNames=T); d12.4   #读取例题数据
D=dist(d12.4);D    #计算距离矩阵
MDS=isoMDS(D);MDS   #多维标度的非度量法求解
x=MDS$points[,1];y=MDS$points[,2]   #降维后的两维度数据
plot(x,y); abline(h=0,v=0,lty=3)    #降维数据作散点图
text(x,y,adj=0.5,labels=rownames(d12.4))   #标记各点对应地区

# 第13章 综合评价方法 ----------------------

### —13.3 评价指标权重确定----------------
library(mvstats)    #加载mvstats包
A=matrix(c(1,1/3,1/7,3,1,1/3,7,3,1),3,3); A    #构造的判断矩阵
A_W=msa.AHP(A);A_W                             #一致性分析与各级权重计算

### —13.4 综合评价方法及应用----------------
d13.1=read.xlsx('mvstats5.xlsx','d13.1',rowNames=T); d13.1   #读取例题数据
A1=d13.1[,1:6];A1    #提取前六个变量
zf<-function(x){ z=(x-min(x))/(max(x)-min(x))*60+40; z }  
A1_Z=apply(A1,2,zf); A1_Z


X_Z<-function(X){    #自编规格化变换函数
  
  Z=apply(X,2,zf)
  Z
}  

A1_Z=X_Z(A1); A1_Z  #对例题数据规格化变换

### 综合评分法
A1_S1=apply(A1_Z,1,mean);             #按行求综合得分
cbind(A1_Z,A1_S1,A1_R1=rank(-A1_S1))  #按综合得分排名 

### 层次分析法
B1=matrix(c(1,4,5,3,6,7,1/4,1,2,1/2,3,4,1/5,1/2,1,1/3,2,3,1/3,  2,  3,  1,  4,5,
            1/6,1/3,1/2,1/4,1,2,1/7,1/4,1/3,1/5,1/2,1),6,6,byrow=T); B1  #构造B1的判断矩阵
B1_W=msa.AHP(B1);B1_W    #B1的权重

A1_S2=A1_Z%*%B1_W        #层次法综合得分
data.frame(A1_Z,A1_S2,A1_R2=rank(-A1_S2))  #层次法综合排名

data.frame('综合评分'=A1_S1,'综合排名'=rank(-A1_S1),
           '层次得分'=A1_S2,'层次排名'=rank(-A1_S2))  #构建综合得分和排名数据框 

B2=matrix(c(1,4,5,7,8,9,1/4,1,2,4,5,6,1/5,1/2,1,3,4,5,1/7,1/4,1/3,1,2,3,1/8,1/5,
            1/4,1/2,1,2,1/9,1/6,1/5,1/3,1/2,1),6,6,byrow=T); B2 #构造B2的判断矩阵
B2_W=msa.AHP(B2);B2_W        #B2的权重
round(B2_W,4)

B3=matrix(c(1,5,2,6,2,6,1,1/5,1,1/4,2,1/4,2,0.2,1/2,5,1,5,1,5,1/2,1/6,1/2,1/5,
            1,1/5,1,1/6,1/2,4,1,5,1,5,1/2,1/6,1/2,1/5,1,1/5,1,1/6,1,5,2,2,2,6,1),7,7,byrow=T)
#构造B3的判断矩阵
B3_W=msa.AHP(B3);B3_W        #B3的权重

A2=d13.1[,8:13]              #A2组数据
A3=d13.1[,14:20]             #A3组数据
A2_S=X_Z(A2)%*%B2_W          #A2得分
A3_S=X_Z(A3)%*%B3_W          #A3得分
A_d=cbind(A1_S,A2_S,A3_S)    #A数据框
A_S=A_d%*%A_W; A_S           #A得分
data.frame('A1得分'=A1_S,'A1排名'=rank(-A1_S),'A2得分'=A2_S,'A2排名'=rank(-A2_S),
           'A3得分'=A3_S,'A3排名'=rank(-A3_S),'综合得分'=A_S,'综合排名'=rank(-A_S))  #构建综合得分和排名数据框 

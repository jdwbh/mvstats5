#***********************************************
#******  多元统计分析及R语言建模（第五版）******
#******  自定义函数: msaR.R               ******
#******  调用方式 source('msaR.R')        ******
#******  修改时间：王斌会 2020.2.1        ******
#***********************************************

options(digits=4)  #设置4位为小数位数 
par(mar=c(4,4,2,1),cex=0.8) #设置图形边际和字体大小

#将数据框第一列设置为数据框行名
msa.X<-function(df){ 
	X=df[,-1]; 
	rownames(X)=df[,1]; 
	X 
}

msa.andrews<-function(x){
   # x is a matrix or data frame of data
   if (is.data.frame(x)==TRUE)
      x<-as.matrix(x)
   t<-seq(-pi, pi, pi/30)
   m<-nrow(x); n<-ncol(x)
   f<-array(0, c(m,length(t)))
   for(i in 1:m){
      f[i,]<-x[i,1]/sqrt(2)
      for( j in 2:n){
          if (j%%2==0) 
             f[i,]<-f[i,]+x[i,j]*sin(j/2*t)
          else
             f[i,]<-f[i,]+x[i,j]*cos(j%/%2*t)
      } 
  }
  #plot(c(-pi,pi), c(min(f),max(f)), type="n", xlab="t", ylab="f(t)")
  plot(c(-pi,pi), c(min(f),max(f)), type="n", xlab="", ylab="")
  for(i in 1:m) lines(t, f[i,] , col=i)
  legend(2,max(f),rownames(x),col=1:nrow(x),lty=1:nrow(x),bty='n',cex=0.8)
}

msa.coef.sd<-function(fm){  #标准回归系数
  b=fm$coef;b
  si=apply(fm$model,2,sd);si
  bs=b[-1]*si[-1]/si[1]
  bs
}


msa.cor.test<-function(X,diag=TRUE){
  p=ncol(X);
  if(diag){
    tp=matrix(1,p,p);
    for(i in 1:p){
      for(j in 1:i) tp[i,j]=cor.test(X[,i],X[,j])$stat;
      for(j in i:p) tp[i,j]=cor.test(X[,i],X[,j])$p.value;
    }
    cat("corr test: \n"); 
    tp=round(matrix(tp,p,dimnames=list(names(X),names(X))),4)
    print(tp)
    #return(tp)
    cat("lower is t value, upper is p value \n")
  } else {
    cat("\n corr test: t value, p value \n"); 
    if(is.matrix(X)) var=1:p
    else var=names(X);
    for(i in 1:(p-1)){
       for(j in (i+1):p) 
          cat(' ',var[i],'-',var[j],cor.test(X[,i],X[,j])$stat,cor.test(X[,i],X[,j])$p.value,"\n")
    }
  }
} 

msa.pca<-function(X,cor=FALSE,m=2,scores=TRUE,ranks=TRUE,sign=TRUE,plot=TRUE){  
	if(m<1) return
	PC=princomp(X,cor=cor)
	Vi=PC$sdev^2
	Vari=data.frame('Variance'=Vi[1:m],'Proportion'=(Vi/sum(Vi))[1:m],
									'Cumulative'=(cumsum(Vi)/sum(Vi))[1:m])
	cat("\n")
	Loadi=as.matrix(PC$loadings[,1:m])
	Compi=as.matrix(PC$scores[,1:m])
	if(sign) 
		for (i in 1:m)  
			if(sum(Loadi[,i])<0){
				Loadi[,i] = -Loadi[,i] 
				Compi[,i] = -Compi[,i] 
			}
	pca<-NULL
	pca$vars=Vari
	if(m<=1) pca$loadings = data.frame(Comp1=Loadi) 
	else pca$loadings = Loadi;
	if(scores & !ranks) pca$scores=round(Compi,4)
	if(scores & plot){
		plot(Compi);abline(h=0,v=0,lty=3)
		text(Compi,row.names(X)) 
	#	par(mar=c(4,4,2,3))
	#	biplot(Compi,Loadi); abline(h=0,v=0,lty=3)
	#	par(mar=c(4,4,1,1))
	}
	if(scores & ranks){
		pca$scores=round(Compi,4)
		Wi=Vi[1:m];Wi
		Comp=Compi%*%Wi/sum(Wi)
		Rank=rank(-Comp)
		pca$ranks=data.frame(Comp=round(Comp,4),Rank=Rank)
	}
	pca
}

msa.fa<-function(X,m=2,scores=TRUE,rotation="varimax",common=TRUE,ranks=TRUE){  
	if(m<1) return
	cat("\n")
	S=cor(X); 
	p<-nrow(S); diag_S<-diag(S); sum_rank<-sum(diag_S)
	rowname = names(X)
	colname<-paste("Factor", 1:p, sep="")
	A<-matrix(0, nrow=p, ncol=p, dimnames=list(rowname, colname))
	eig<-eigen(S)
	for (i in 1:p)
		A[,i]<-sqrt(eig$values[i])*eig$vectors[,i]
	for (i in 1:p) { if(sum(A[,i])<0) A[,i] = -A[,i] }
	h<-diag(A%*%t(A))
	rowname<-c("Variance","Proportion","Cumulative")
	B<-matrix(0, nrow=3, ncol=p, dimnames=list(rowname, colname))
	for (i in 1:p){
		B[1,i]<-sum(A[,i]^2)
		B[2,i]<-B[1,i]/sum_rank
		B[3,i]<-sum(B[1,1:i])/sum_rank
	}
	W=B[2,1:m]*100; 
	Vars=data.frame('Variance'=B[1,],'Proportion'=B[2,]*100,
									'Cumulative'=B[3,]*100)
	A=A[,1:m]
	if(rotation == "varimax" & m>1){   
		#cat("\n Factor Analysis for Princomp in Varimax: \n\n");
		VA=varimax(A); A=VA$loadings; 
		s2=apply(A^2,2,sum); 
		k=rank(-s2); s2=s2[k]; 
		W=s2/sum(B[1,])*100; 
		Vars=data.frame('Variance'=s2,'Proportion'=W,'Cumulative'=cumsum(W))
		rownames(Vars) <- paste("Factor", 1:m, sep="")
		A=A[,k]
		for (i in 1:m) { if(sum(A[,i])<0) A[,i] = -A[,i] }
		A=A[,1:m]; 
		colnames(A) <- paste("Factor", 1:m, sep="")
	}
	fit<-NULL
	fit$vars<-round(Vars[1:m,],3)
	if(m<=1) fit$loadings <- data.frame("Factor1"=round(A,4))
	else fit$loadings <- round(A,4)
	if(common){   
		fit$common <- round(apply(A^2,1,sum),4)
	} 
	Z=as.matrix(scale(X));
	PCs=Z%*%solve(S)%*%A
	fit$scores <- round(PCs,4)
	if(ranks){
		W=apply(fit$loadings^2,2,sum)
		Wi=W/sum(W);
		F=PCs%*%Wi 
		fit$ranks=data.frame(Factor=round(F,4),Rank=rank(-F))
	} 
	fit
}

msa.KMO<-function(r){
	cl <- match.call()
	if (nrow(r) > ncol(r)) 
		r <- cor(r, use = "pairwise")
	Q <- try(solve(r))
	if (class(Q) == as.character("try-error")) {
		message("matrix is not invertible, image not found")
		Q <- r
	}
	S2 <- diag(1/diag(Q))
	IC <- S2 %*% Q %*% S2
	Q <- Image <- cov2cor(Q)
	diag(Q) <- 0
	diag(r) <- 0
	sumQ2 <- sum(Q^2)
	sumr2 <- sum(r^2)
	MSAi <- colSums(r^2)/(colSums(r^2) + colSums(Q^2))
	kmo <- sumr2/(sumr2 + sumQ2)
		ans <- list(MSAi = MSAi, KMO = kmo,result = test)
	return(ans)
}

msa.bartlett<-function(R, n = NULL, diag = TRUE){
	if (dim(R)[1] != dim(R)[2]) {
		n <- dim(R)[1]
		message("R was not square, finding R from data")
		R <- cor(R, use = "pairwise")
	}
	p <- dim(R)[2]
	if (!is.matrix(R)) 
		R <- as.matrix(R)
	if (is.null(n)) {
		n <- 100
		warning("n not specified, 100 used")
	}
	if (diag) 
		diag(R) <- 1
	detR <- det(R)
	#蠂2=-[n-(2p+11)/6]ln|R|;   df=p(p-1)/2
	statistic <- -log(detR) * (n - 1 - (2 * p + 5)/6)
	df <- p * (p - 1)/2
	pval <- pchisq(statistic, df, lower.tail = FALSE)
	bartlett <- list(chisq = statistic, df = df, p.value = pval)
	return(bartlett)
}

msa.cor<-function (X, diag = TRUE){
	options(digits = 4)
	p = ncol(X)
	if (diag) {
		tp = matrix(1, p, p)
		for (i in 1:p) {
			for (j in 1:i) tp[i, j] = cor.test(X[, i], X[, j])$stat
			for (j in i:p) tp[i, j] = cor.test(X[, i], X[, j])$p.value
		}
		cat("corr test: \n")
		tp = round(matrix(tp, p, dimnames = list(names(X), names(X))), 
							 4)
		print(tp)
		cat("lower is t value, upper is p value \n")
	}
	else {
		cat("\n corr test: t value, p value \n")
		if (is.matrix(X)) 
			var = 1:p
		else var = names(X)
		for (i in 1:(p - 1)) {
			for (j in (i + 1):p) cat(" ", var[i], "-", var[j], 
				 cor.test(X[,i],X[,j])$stat,cor.test(X[,i],X[,j])$p.value, "\n")
		}
	}
}

msa.cancor<-function (x, y, pq=min(ncol(x),ncol(y)), plot = FALSE){
	x = scale(x)
	y = scale(y)
	n = nrow(x)
	p = ncol(x)
	q = ncol(y)
	ca = cancor(x, y)
	#cat("\n");	print(ca)
	r = ca$cor
	m <- length(r)
	Q <- rep(0, m)
	P = rep(0, m)
	lambda <- 1
	for (k in m:1) {
		lambda <- lambda * (1 - r[k]^2)
		Q[k] <- -log(lambda)
	}
	s <- 0
	i <- m
	for (k in 1:m) {
		Q[k] <- (n - k + 1 - 1/2 * (p + q + 3) + s) * Q[k]
		P[k] <- 1 - pchisq(Q[k], (p - k + 1) * (q - k + 1))
	}
	#cat("\n cancor test: \n")
	#print(round(data.frame(r, Q, P),4))
	cr=round(data.frame(CR=r, Q, P),4)
	cat("\n")
	u=as.data.frame(ca$xcoef[,1:pq]); colnames(u)=paste('u',1:pq,sep='')
	#print(round(u,4))
	v=as.data.frame(ca$ycoef[,1:pq]); colnames(v)=paste('v',1:pq,sep='')
	#print(round(v,4))
	if (plot) {
		u1 = as.matrix(x) %*% u[,1]
		v1 = as.matrix(y) %*% v[,1]
		plot(u1, v1, xlab = "u1", ylab = "v1")
		abline(lm(u1 ~ v1))
	}
	list(cor=cr,xcoef=t(round(u,4)),ycoef=t(round(v,4)))
}

msa.AHP<-function(B){
  A=matrix(B,nrow=sqrt(length(B)),ncol=sqrt(length(B)),byrow=TRUE)
  print(A)
	m=ncol(A)
	ai=apply(A,1,prod)^(1/m)
	W=ai/sum(ai); 
	if(m>2){
		AW=A%*%W
		L_max=sum(AW/W)/m; 
		CI=(L_max-m)/(m-1); 
		RI=c(0,0,0.58,0.90,1.12,1.24,1.32,1.41,1.45,1.49,1.51)
		CR=CI/RI[m]
		cat('\n    L_max=',L_max,'\n')
		cat('    CI=',CI,'\n')
    cat('    CR=',CR,'\n')
    if(CR<=0.1) cat('  Consistency test is OK!\n\n')
    else cat('    Please adjust the judgment matrix!\n')
	}
	return(W)
}

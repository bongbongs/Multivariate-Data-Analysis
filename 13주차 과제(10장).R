#연습문제 10.2
G11 <- matrix(c(-2,5),2,1)
G12 <- matrix(c(0,3),2,1)
G13 <- matrix(c(-1,1),2,1)
G21 <- matrix(c(0,6),2,1)
G22 <- matrix(c(2,4),2,1)
G23 <- matrix(c(1,2),2,1)
G31 <- matrix(c(1,-2),2,1)
G32 <- matrix(c(0,0),2,1)
G33 <- matrix(c(-1,4),2,1)

G1 <- matrix(c(G11,G12,G13),2,3)
G2 <- matrix(c(G21,G22,G23),2,3)
G3 <- matrix(c(G31,G32,G33),2,3)

#(a)
#표본 평균벡터
X1 <- matrix(c(rowMeans(G1)),2,1)
X2 <- matrix(c(rowMeans(G2)),2,1)
X3 <- matrix(c(rowMeans(G3)),2,1)

#표본공분산행렬
Cov_G1 <- 1/2*((G11-X1)%*%t(G11-X1)+(G12-X1)%*%t(G12-X1)+(G13-X1)%*%t(G13-X1))
Cov_G2 <- 1/2*((G21-X2)%*%t(G21-X2)+(G22-X2)%*%t(G22-X2)+(G23-X2)%*%t(G23-X2))
Cov_G3 <- 1/2*((G31-X3)%*%t(G31-X3)+(G32-X3)%*%t(G32-X3)+(G33-X3)%*%t(G33-X3))

#(b)
det(Cov_G1)
det(Cov_G2)
det(Cov_G3)

solve(Cov_G1)
solve(Cov_G2)
solve(Cov_G3)

p1=p2=0.25
p3=0.5

X <- matrix(c('x1','x2'),2,1)
X

#Q1X <- log(p1)-1/2*log(det(Cov_G1))-1/2*t(X1)%*%solve(Cov_G1)%*%X1-1/2*t(X)%*%solve(Cov_G1)%*%X+t(X1)%*%solve(Cov_G1)%*%X

#Q2X <- log(p2)-1/2*log(det(Cov_G2))-1/2*t(X2)%*%solve(Cov_G2)%*%X2-1/2*t(X)%*%solve(Cov_G2)%*%X+t(X2)%*%solve(Cov_G2)%*%X

#Q3X <- log(p3)-1/2*log(det(Cov_G3))-1/2*t(X3)%*%solve(Cov_G3)%*%X3-1/2*t(X)%*%solve(Cov_G3)%*%X+t(X3)%*%solve(Cov_G3)%*%X

#(c)
X0 <- matrix(c(-1,-2),2,1)
X0
Q1X <- log(p1)-1/2*log(det(Cov_G1))-1/2*t(X1)%*%solve(Cov_G1)%*%X1-1/2*t(X0)%*%solve(Cov_G1)%*%X0+t(X1)%*%solve(Cov_G1)%*%X0
Q2X <- log(p2)-1/2*log(det(Cov_G2))-1/2*t(X2)%*%solve(Cov_G2)%*%X2-1/2*t(X0)%*%solve(Cov_G2)%*%X0+t(X2)%*%solve(Cov_G2)%*%X0
Q3X <- log(p3)-1/2*log(det(Cov_G3))-1/2*t(X3)%*%solve(Cov_G3)%*%X3-1/2*t(X0)%*%solve(Cov_G3)%*%X0+t(X3)%*%solve(Cov_G3)%*%X0

Q1X>Q2X
Q2X>Q3X
#G1 그룹에 속한다.

#(d)
X <- matrix(c('x1','x2'),2,1)
X
Spl <- (2*Cov_G1+2*Cov_G2+2*Cov_G3)/6 #합동공분산행렬 : [(n1-1)S1 + (n2-1)S2 + (n3-1)S3]/(n1+n2+n3-3)
Spl

#L1X <- log(p1)+t(X1)%*%solve(Spl)%*%X-1/2*(t(X1)%*%solve(Spl)%*%X1)
#L2X <- log(p2)+t(X2)%*%solve(Spl)%*%X-1/2*(t(X2)%*%solve(Spl)%*%X2)
#L3X <- log(p3)-t(X3)%*%solve(Spl)%*%X-1/2*(t(X3)%*%solve(Spl)%*%X3)

#(e)
X <- matrix(c(-1,-2),2,1)
L1X <- log(p1)+t(X1)%*%solve(Spl)%*%X-1/2*(t(X1)%*%solve(Spl)%*%X1)
L2X <- log(p2)+t(X2)%*%solve(Spl)%*%X-1/2*(t(X2)%*%solve(Spl)%*%X2)
L3X <- log(p3)-t(X3)%*%solve(Spl)%*%X-1/2*(t(X3)%*%solve(Spl)%*%X3)

L1X>L2X
L2X>L3X
L1X>L3X
#G3 그룹에 속한다.


#연습문제 10.3
bankruptcy <- read.csv('Table10.5.csv',header=T)
bankruptcy
attach(bankruptcy)
n=dim(bankruptcy)[[1]]
n

#(a)
library(MASS)
ld=lda(group~X1+X2+X3+X4,data=bankruptcy)
ld 
#p1 = 0.4565217, p2=0.5434783, Y=0.6587666X1 + 4.3944790X2 + 0.8883115X3 -1.1917220X4

#(b)
pc=predict(ld,bankruptcy)$class
pc=as.numeric(pc)
pc[(pc==1)]=1
pc[(pc==2)]=2
pc

class.table=table(group,pc)

res=cbind(group,pc)
res
correct=res[(group==pc),]
correct
correct.rate=dim(correct)[[1]]/n
correct.rate
error.rate=1-correct.rate
error.rate #재대입 분류에 의한 오류율

#(c)
x=bankruptcy[,1:4]
x
qd=qda(x,group)
qd

#(d)
qc=predict(qd)$class
qc=as.numeric(qc)
qc[(qc==1)]=1
qc[(qc==2)]=2
qc

class.table <- table(group,qc)

resq=cbind(group,qc)
correctq=resq[(group==qc),]
correctq.rate=dim(correctq)[[1]]/n
correctq.rate
errorq.rate=1-correctq.rate
errorq.rate #재대입 분류에 의한 오류율

#(e)


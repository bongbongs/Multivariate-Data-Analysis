#예제 1.2
st1 = c(90,80)
st2 = c(80,90)
st3 = c(75,80)
st4 = c(70,70)
st5 = c(65,80)

st=rbind(st1,st2,st3,st4,st5)
st
mu=colMeans(st)
mu
S=cov(st)
S
R=cor(st)
R
de=dist(st[1:2,],method="euclidean")
de
D=rbind(c(S[1,1],0),c(0,S[2,2]))
D
ds=t(st[1,]-st[2,])%*%solve(D)%*%(st[1,]-st[2,])
ds=sqrt(ds)
ds
dm=t(st[1,]-st[2,])%*%solve(S)%*%(st[1,]-st[2,])
dm=sqrt(dm)
dm

#연습문제 1.1
#(a)
en1 = c(3,5)
en2 = c(4,5)
en3 = c(2,4)
en4 = c(6,7)
en5 = c(8,10)
en6 = c(2,5)

en = rbind(en1,en2,en3,en4,en5,en6)
en

mu=colMeans(en)
mu

x1bar = 4.166667 
x2bar = 6
xbar = matrix(c(4.166667,6),nrow=2,ncol=1)
xbar #표본평균벡터

#(b)
x1 <- c(3,4,2,6,8,2)
var(x1)
s11 <- var(x1)
s11

x2 <- c(5,5,4,7,10,5)
var(x2)
s22 <- var(x2)
s22

#(c)
S=cov(en)
S

#(d)
R=cor(st)
R

#(e)
library(lattice)
splom(en)

#(f)
en[c(1,3)]
de=dist(en[c(1,3),],method="euclidean")
de #유클리드거리

#(g)
D=rbind(c(S[1,1],0),c(0,S[2,2]))
D
solve(D)
ds=t(en[1,]-en[3,])%*%solve(D)%*%(en[1,]-en[3,])
ds=sqrt(ds)
ds #표준화거리

#(h)
dm=t(en[1,]-en[3,])%*%solve(S)%*%(en[1,]-en[3,])
dm=sqrt(dm)
dm #Mahalanobis 거리

#(i)
#한글 파일에 그림으로 대체

#연습문제 1.4
run <- read.csv("run.csv", header=T)
run

#(a)
library(lattice)
splom(run)

#(b)
parallelplot(run, main="parallel graph")

#(c)
distance = run[,2:8]
m = colMeans(distance)
m #표본평균벡터

cv=cov(distance) #표본공분산행렬
cv

cr=cor(distance) #표본상관행렬
cr

#(d)
de = dist(distance[c(12,14),],method="euclidean")
de

#(e)
D = rbind(c(cv[1,1],0,0,0,0,0,0),c(0,cv[2,2],0,0,0,0,0),c(0,0,cv[3,3],0,0,0,0),c(0,0,0,cv[4,4],0,0,0),c(0,0,0,0,cv[5,5],0,0),c(0,0,0,0,0,cv[6,6],0),c(0,0,0,0,0,0,cv[7,7]))
D

d1 = distance[12,]
d2 = distance[14,]
d1-d2
solve(D)

a=matrix(c(-0.23,-0.49,-1.97,-0.06,-0.07,-0.42,-14.15),nrow=1,ncol=7)
a

ds = a%*%solve(D)%*%t(a)
ds = sqrt(ds)
ds

#(f)
dm = a%*%solve(cv)%*%t(a)
dm = sqrt(dm)
dm


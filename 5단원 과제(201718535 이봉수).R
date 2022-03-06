#5.1
xi1 <- c(2,8,6,8)
xi2 <- c(12,9,9,10)
xic=cbind(xi1,xi2)
xic

#(a)
xbar=colMeans(xic)
xbar

#(b)
S=cov(xic)
S

#(c)
solve(S)

#(d)
mu=c(7,11)
mu
muprime=cbind(mu)
muprime

xbar-mu

xbarprime=cbind(xbar)

n=length(xi1)
n
T2=n*(xbar-mu)%*%solve(S)%*%(xbarprime-muprime)

T2

{(n-1)*2}/(n-2)*qf(0.95,2,2) #유의수준 5% 기각값

#T2(13.636)가 유의수준 5% 기각값인 57보다 작으므로 H0를 기각할 수 없다.

#(e)
#사진첨부

#(f)
C=matrix(c(1,1,-1,1),nc=2)
C
xir=rbind(xi1,xi2)
xir
C%*%xir

#xi1-xi2 = (-10,-1,-3,-2)
#xi1+xi2 = (14,17,15,18)

#(g)
Cxi1=c(-10,-1,-3,-2)
Cxi2=c(14,17,15,18)

Cxic=cbind(Cxi1,Cxi2)
Cxic

Cxbar=colMeans(Cxic)
Cxbar

C%*%mu
Cmu=c(-4,18)

Cxbar-Cmu

Cxbarprime=cbind(Cxbar)
Cxbarprime

Cmuprime=cbind(Cmu)
Cmuprime


T2=n*(Cxbar-Cmu)%*%solve(S)%*%(Cxbarprime-Cmuprime)
T2

#위의 d번에서 T2에 비해 값이 커졌지만(26.181) 여전히 기각값인 57보다 작으므로 H0를 기각할 수 없다.

#(h)
{(n-1)*2}/(n-2)*qf(0.95,2,2)


#사진첨부

#(i)
xir
de=dist(xir[c(1,2),],method="euclidean")
de

#연습문제 5.5
Ex1 <- c(121,108,122,77,140,108,124,130,97,109)
Ex2 <- c(54,40,41,80,38,59,72,85,31,57)
Px1 <- c(132,123,129,131,110,89,125,129,141)
Px2 <- c(50,64,55,48,42,40,30,58,27)

#(a)
Exc <- cbind(Ex1,Ex2)
Exc
Exbar=colMeans(Exc) #엔지니어 표본평균벡터
Exbar

Pxc <- cbind(Px1,Px2)
Pxbar=colMeans(Pxc) #조종사 표본평균벡터
Pxbar

ES=cov(Exc) #엔지니어 표본공분산행렬
ES

PS <- cov(Pxc) #조종사 표본공분산행렬
PS

#(b)
n1=length(Ex1)
n2=length(Px1)
n1;n2

Spl=(((n1-1)*ES)+((n2-1)*PS))/(n1+n2-2)
Spl
solve(Spl)

Exbar_prime=cbind(Exbar)
Exbar_prime

Pxbar_prime=cbind(Pxbar)
Pxbar_prime

T2=n1*n2/(n1+n2)*(Exbar-Pxbar)%*%solve(Spl)%*%(Exbar_prime-Pxbar_prime)
T2

p=2
(n1+n2-p-1)
(n1+n2-2)*p/(n1+n2-p-1)*qf(0.95,2,16) # p=2인 F분포 유의수준 5% 기각치

#유의수준 5% 기각치에서 검정 할 때, T2(3.094)는 기각치(7.721)보다 값이 작으므로 H0(u1=u2)를 기각할 수 없다. 따라서 두 집단의 모평균벡터는 같다

#(c)
T2=(Exbar-Pxbar)%*%solve(ES/n1+PS/n2)%*%(Exbar_prime-Pxbar_prime)
T2

qchisq(0.95,df=2) #p=2인 카이제곱분포 유의수준 5% 기각치

#유의수준 5% 기각치에서 검정 할 때, T2(3.094)는 기각치(5.991)보다 값이 작으므로 H0(u1=u2)를 기각할 수 없다. 따라서 두 집단의 모평균벡터는 같다

#연습문제 5.9
Table2 <- read.csv("Table5.13.csv",header=T)
Table2


#(a)
d1=Table2[,1]-Table2[,6] #X1(Ho)-X1(Hc)
d1

d2=Table2[,2]-Table2[,7] #X2(Ho)-X2(Hc)
d2

d3=Table2[,3]-Table2[,8] #X3(Ho)-X3(Hc)
d3

d4=Table2[,4]-Table2[,9] #X4(Ho)-X4(Hc)
d4

d=cbind(d1,d2,d3,d4)
d

Md=colMeans(d)
Md

Md_prime=cbind(Md)
Md_prime

S=cov(d)
S

n=length(d1)
n

T2=n*Md%*%solve(S)%*%Md_prime
T2

p=4 #자유도
(n-1)*p/(n-p)*qf(0.95,2,n-p) #F분포 p=2인 유의수준 5% 기각치

#유의수준 5% 기각치일 때, T2(104.59)이 기각치(17.65)보다 크므로 h0(u1=u2)를 기각할 수 있다. 즉 두 딱정벌레 종류의  평균벡터간 차이가 존재한다.

#(b)
t.test(Table2[,1],Table2[,6]) #X1(Ho,Hc)
t.test(Table2[,2],Table2[,7])
t.test(Table2[,3],Table2[,8])
t.test(Table2[,4],Table2[,9])

#각 변수별로 t-검정 한 결과, p값이 모두 매우 작으므로 귀무가설은 기각된다.

#연습문제 5.10
Table <- read.csv("Table5.14.csv",header=T)
Table
d1=Table[,1]-Table[,3] #Y1-X1
d2=Table[,2]-Table[,4] #Y2-X2

d=cbind(d1,d2)
d

Md=colMeans(d)
Md

Md_prime=cbind(Md)
Md_prime

S=cov(d)
S

n=length(d1)

T2=n*Md%*%solve(S)%*%Md_prime
T2

p=2
(n-1)*p/(n-p)*qf(0.95,2,n-p) #F분포 p=2인 유의수준 5% 기각치

#유의수준 5% 기각치일 때, T2(22.3)이 기각치(8.01)보다 크므로 h0(u1=u2)를 기각할 수 있다. 즉 '첫번째 입원 날짜로부터 생존시간'과 '치료하지 않은 날짜로부터 생존시간' 두 집단의 평균벡터간에 통계적으로 유의한 차이가 있다.

summary(Table)

#연습문제 4.2
x <- c(-3,1,4)
u <- cbind(x)
u

sigma <-matrix(c(1,-2,0,2,5,0,0,0,2),nrow=3,ncol=3)
sigma

#(a)
sigma[1,2] 
#≠0이므로 독립이 아니다
corr_x1_x2 = -2/sqrt(1)%*%sqrt(5)
corr_x1_x2

#(b)
sigma[1,3] 
#0이므로 독립이다

#(c)
sigma[2,3] 
#0이므로 독립이다

#(d)
sigma
#x1,x2로 이뤄진 열벡터와 x3의 공분산이 0이므로 독립이다.

#(e)
#x1,x2/2와 x3의 공분산이 0이므로 독립이다.

#(f)
#x1-x2와 x3의 공분산이 0이므로 독립이다.

#(g)
y <- c(3,-2,1)

yy <- rbind(yy)
yyy <- cbind(y)

yy%*%u #평균

yy%*%sigma%*%yyy #분산

#3x1-2x2+x3은 평균이 -7, 분산이 31인 정규분포를 따른다

#(h)
#x1이 N(-3,1)을 따르므로 (x1+3)^2은 카이제곱 분포를 따른다

#(i)
#X가 다변량 정규분포를 따르므로 X-u는 평균이 0, 분산이 sigma인 N3(p=3) 정규분포를 따른다.

#(j)
#sigma^-1/2*(X-u)는 성질 4.2 표준화 벡터에 의해 평균이 0, 분산이 I인 N3(p=3) 정규분포를 따른다.

#(k)
#성질 4.7에 의해 (X-u)'*sigma^-1*(X-u)는 p=3인 카이제곱 분포를 따른다.

#(l)
#Xi~iid N3(p=3)(u,sigma)인 경우 다변량으로 확장할때, sigma i는 1부터 n까지 (Xi-u)(Xi-u)`은 Wp(n,sigma)인 위샤트분포를 따른다. 이에 의해 (x-u)(x-u)'은 W3(1,sigma)를 따른다.

#(m)
ea=eigen(sigma)
ea

#고유값1:3, 고유값2:3, 고유값3:2
#고유벡터1: [0.7071,0.7071,0]
#고유벡터2: [0.7071,0.7071,0]
#고유벡터3: [0,0,1]

#(n)
I_sigma=solve(sigma)
ea2=eigen(I_sigma)
ea2

#고유값1:0.5, 고유값2:0.3, 고유값3:0.3
#고유벡터1: [0,0,1]
#고유벡터2: [0.7071,0.7071,0]
#고유벡터3: [0.7071,0.7071,0]

#연습문제 4.5
K_indust <- read.csv("K_indust.csv",header=T)

#(a)
colMeans(K_indust[2:7]) #표본평균벡터
xbar=colMeans(K_indust[2:7])

cov(K_indust[2:7]) #공분산행렬
S=cov(K_indust[2:7])

#(b)
qqnorm(X1)
qqline(X1)

qqnorm(X2)
qqline(X2)

qqnorm(X3)
qqline(X3)

qqnorm(X4)
qqline(X4)

qqnorm(X5)
qqline(X5)

qqnorm(X6)
qqline(X6)

#대체로 정규성을 크게 벗어나지 않는다.

#(c)
chi2.plot <- function(x){
  n=dim(x)[[1]] #number of observations
  vp=dim(x)[[2]] #number of variables
  xbar=colMeans(x)
  S=cov(x)
d=mahalanobis(x,colMeans(x),S)
d2=sort(d)
tt=seq(1,n)
t=(tt-0.5)/n
q=qchisq(t,vp)
plot(q,d2,pch="*", main="Chisquare plot for multivariate normality", xlab="chi2 quantile",ylab="ordered Mahalanobis d^2")
abline(0,1)
return(list(xbar,S))
}
b=K_indust[2:7]
chi2.plot(b)

#정규성을 만족한다.

#연습문제 4.6

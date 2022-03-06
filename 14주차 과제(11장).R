#연습문제 11.3
X=matrix(c('a','b','c','d',5,-1,1,2,3,2,3,6),4,3)
X

#(a)
x=X[,2:3]
x
D=dist(x)
D

#(b)
hc1=hclust(dist(x)^2,method='single')
plot(hc1,hang=-1,labels=X[,1],main='single linkage')

#(c)
hc2=hclust(dist(x)^2,method='complete')
plot(hc2,hang=-1,labels=X[,1],main='complete linkage')

#(d)
hc3=hclust(dist(x)^2,method='average')
plot(hc3,hang=-1,labels=X[,1],main='average linkage')

#(e)
a=c(5,3)
b=c(-1,2)
c=c(1,3)
d=c(2,6)
X=rbind(a,b,c,d)
X #숫자화

X_scaled=scale(X) #표준화
X_scaled

A=dist(X_scaled) #표준화 거리행렬
A

#(f)
hc4=hclust(A^2,method='single')
plot(hc4,hang=-1,main='single linkage')

#(g)
hc5=hclust(A^2,method='complete')
plot(hc5,hang=-1,main='complete linkage')

#(h)
hc6=hclust(A^2,method='average')
plot(hc6,hang=-1,main='average linkage')

#(i)
cX=cov(X)
mahal_X=mahalanobis(X,center=FALSE,cov=cX) 

B=dist(mahal_X) #마할라노비스 거리행렬
B

#(j)
hc7=hclust(B^2,method='single')
plot(hc7,hang=-1,main='single linkage')

#(k)
hc8=hclust(B^2,method='complete')
plot(hc8,hang=-1,main='complete linkage')

#(l)
hc9=hclust(B^2,method='average')
plot(hc9,hang=-1,main='average linkage')

#연습문제 11.5
crime=read.csv('table7.7.csv')
crime

x=crime[,2:8]
x

#(a)
hc1=hclust(dist(x)^2,method='single')
plot(hc1,labels=crime[,1],hang=-100,main='single linkage')
#군집 간 높이의 차이가 큰 군집의 개수를 선택하면 군집 내 응집력은 높고, 군집간 이질성이 큰 적절한 군집을 구할 수 있다. 여기서는 3~5개가 적당해 보인다.
rect.hclust(hc1, k=4) #군집 개수 4개로 분류 

#(b)
hc2=hclust(dist(x)^2,method='complete')
plot(hc2,labels=crime[,1],hang=-5,main='complete linkage')
#군집 개수는 2~4개가 적당해 보인다.
rect.hclust(hc2, k=3) #군집 개수 3개로 분류 

#(c)
#최단연결법(a)은 최장연결법(b)에 비해 하나의 군집으로 개별 관측치가 모여드는 경향이 있다.

#연습문제 11.9
industry=read.csv('Table4.7.csv',encoding='UTF-8')
industry

x=industry[,2:7]
x

#(a)
hc1=hclust(dist(x)^2,method='single')
par(family="AppleGothic")
plot(hc1,labels=industry[,1],hang=-1,main='single linkage')
#군집 간 높이의 차이가 큰 군집의 개수를 선택하면 군집 내 응집력은 높고, 군집간 이질성이 큰 적절한 군집을 구할 수 있다. 여기서는 2~3개가 적당해 보인다.
rect.hclust(hc1, k=3) #군집 개수 3개로 분류 

#(b)
hc2=hclust(dist(x)^2,method='complete')
plot(hc2,labels=industry[,1],hang=-1,main='complete linkage')
#군집 개수는 2~4개가 적당해 보인다.
rect.hclust(hc2, k=4) #군집 개수 4개로 분류 

#(c)
hc3=hclust(dist(x)^2,method='ward.D')
plot(hc3,labels=industry[,1],hang=-1,main='ward method')
#군집 개수는 2~4개가 적당해 보인다.
rect.hclust(hc3, k=4) #군집 개수 4개로 분류 

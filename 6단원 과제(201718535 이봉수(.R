#6.6
x1bar=c(3,2,1)
x2bar=c(4,1,1)
x3bar=c(4,2,2)

#(a)
#Xlj=µ+rl+£lj, l=1,2,3, j=1,2,3,4...,12
#£lj ~ i.i.d N4(0,∑)
#µ = 전체 평균, n1=10, n2=20, n3=50
#rl는 l번째 처리효과, ∑(i=1,3)nlrl=0

#6.7
Fishcook=read.csv('Table6.13.csv')
Fishcook

#(a)
#Xlj=µ+rl+£lj, l=1,2,3, j=1,2,3,4...,12
#£lj ~ i.i.d N4(0,∑)
#µ = 전체 평균
#rl는 l번째 요리법 효과, ∑(i=1,3)nlrl=0

#(b)
attach(Fishcook)
method <- factor(method)
y <- cbind(x1,x2,x3,x4)
fit=manova(y~method)
fit

#(c)
summary(fit,test="Wilks")

#Wilks lambda 검정의 p=값이 = 1.233e-07로 유의수준 값 0.05보다 작다. 따라서 'H0 = 세 가지 요리방법 간 평균벡터 차이가 없다'를 기각하므로 차이가 있다.

#(d)
ax1=aov(x1~method)
ax1
summary(ax1)

#p값이 0.288>0.05이므로 'H0: r11=r21=r31=0'을 기각하지 못한다. 따라서 요리법 간 X1(향)요소는 차이가 없다.

ax2=aov(x2~method)
summary(ax2)
#p값이 0.0005<0.05이므로 'H0: r12=r22=r32=0'을 기각한다. 따라서 요리법 간 X2(맛)요소는 유의한 차이가 있다.

ax3=aov(x3~method)
summary(ax3)
#p값이 0.0046<0.05이므로 'H0: r13=r23=r33=0'을 기각한다. 따라서 요리법 간 X3(육질)요소는 유의한 차이가 있다.

ax4=aov(x4~method)
summary(ax4)
#p값이 0.295>0.05이므로 'H0: r14=r24=r34=0'을 기각하지 못한다. 따라서 요리법 간 X4(수분정도)요소는 차이가 없다.

#(e)
p = 4 #number of variables
g = 3 #number of methods

g1=Fishcook[(Fishcook$method==1),2:5]
s1=cov(g1); s1=round(s1,3)
g2=Fishcook[(Fishcook$method==2),2:5]
s2=cov(g2); s2=round(s2,3)
g3=Fishcook[(Fishcook$method==3),2:5]
s3=cov(g3); s3=round(s3,3)

n1=nrow(g1) ; n2=nrow(g2) ; n3=nrow(g3)
spool = ((n1-1)*s1+(n2-1)*s2+(n3-1)*s3)/(n1+n2+n3-3); spool=round(spool,3)

M1=(det(s1)/det(spool))^{(n1-1)/2}
M2=(det(s2)/det(spool))^{(n2-1)/2}
M3=(det(s3)/det(spool))^{(n3-1)/2}
M=M1*M2*M3

v1=n1-1;v2=n2-1;v3=n3-1;
v=c(v1,v2,v3)
temp1=sum(1/v) ; temp2=(2*p^2+3*p-1)/(6*(p+1)*(g-1))
c1=(temp1-1/sum(v))*temp2
u=-2*(1-c1)*log(M)
df=(g-1)*p*(p+1)/2
p.value=pchisq(u,df,lower.tail=FALSE)
u
p.value

#p-값이 0.8549로 0.05보다 크기 때문에 'H0: 공분산행렬이 동일하다'를 기각한다. 따라서 요리방법별 공분산행렬이 동일하지 않다.

#6.8
#(a)
#Xlj=µ+rl+ßk+rlk+elkr, l=1,2,3,4, k=1,2, r=1,2,3,4,5
#g=4, b=2, n=5
#elkr ~ i.i.d N2(0,∑)
#rl는 첫 번째 요인인 사과 종류, ßk는 두 번째 요인인 비료 종류, rlk는 사과 종류와 비료 종류에 따른 상호작용 효과, ∑(l=i,4)rl=∑(l=1,2)ßk=∑(l=1,4)rlk=∑(k=1,2)rlk

#(b)
applefert=read.csv('Table6.14.csv')
attach(applefert)
Apple=factor(Apple)
Fertilizer=factor(Ferilizer)
x=cbind(X1,X2)
fit=manova(x~Apple+Fertilizer+Apple:Fertilizer)
fit

#(c)
summary(fit)
#사과 종류와 비료 종류 모두 p-값이 0.05보다 작아 각각의 귀무가설 H0:r1=r2=r3=r4=0, H0:ß1=ß2=ß3=ß4=0을 기각하므로 효과가 유의하다.

#(d)
summary(fit)
#반면 상호작용효과는 p-값이 0.05보다 크기 때문에 귀무가설 H0:r11=r12=r13=r14=r21....=r44=0을 기각하지 못하므로 효과가 유의하지 않다.


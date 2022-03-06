#연습문제 8.5
#(1)
Kor <- read.csv('table4.6.csv')
Kor
Kordata <- Kor[,-1]
Kordata

library(psych)
#install.packages('GPArotation')
library(GPArotation)

Kor.varimax = principal(Kordata,nfactors=2,rotate='varimax') #principal = 주성분법 이용한 varimax 회전 함수
Kor.varimax

#(2)
#X1(총자본 투자효율), X2(노동소득 분배율)
#첫번째 인자는 X1과 X2의 비중이 각각 0.85,0.85로 'X1,X2 가중평균인자'로 해석할 수 있고, 두번째 인자는 X2의 비중이  X1에 비해 높기 때문에(0.53>-0.53) 'X2 인자'로 해석 가능하다.
#두 인자로 설명하는 분산 비율은 총 100%이다.


#연습문제 8.8
diabete <- read.csv('Table9.2.csv')
diabete.data <- diabete[,-1]
diabete.data

#(a)
fact <- factanal(diabete.data,factors=2,rotation="varimax",scores='regression')
fact #최대우도법 직교인자모형

diabete.prin1 <- principal(diabete.data,nfactors=2,rotate="varimax")
diabete.prin1 #주성분법 직교인자모형

#(b)
diabete.prin2 <- principal(diabete.data,nfactors=2,rotate="none") #직교회전X
diabete.prin2

#Y1 = 0.54PC1 + 0.59PC2
#Y2 = 0.10PC1 + 0.76PC2
#X1 = 0.48PC1 + 0.22PC2
#X2 = 0.71PC1 - 0.47PC2
#X3 = 0.82PC1 - 0.20PC2

#(c)
library(graphics)
prin=princomp(diabete.data)
screeplot(prin,type='lines')

#누적설명분산이 70%~90% 사이인 4개가 적절하다.

#(d) => 최대우도법 사용
fact <- factanal(diabete.data,factors=2,rotation="varimax",scores='regression')
namevar=names(fact$loadings)=c("Y1","Y2","X1","X2","X3")
plot(fact$loadings[,1],fact$loadings[,2],xlab='PC1',ylab='PC2')
text(x=fact$loadings[,1],y=fact$loadings[,2],labels=namevar)
abline(v=0,h=0)

#(e) => 주성분법 사용
diabete.prin1 <- principal(diabete.data,nfactors=2,rotate="varimax")
diabete.prin1
#첫 번째 인자는 X2,X3 비중이 높아 'X2,X3 인자', 두 번째 인자는 Y1,Y2 비중이 높아 'Y1,Y2 인자'로 해석 가능하다.

#(f)
fact
#첫 번째 인자는 X2의 비중이 높아(0.5이상) 'X2,X3 인자', 두 번째 인자는 Y1 비중이 높아 'Y1 인자'로 해석 가능하다. 이는 주성분값으로 구했을 때와 비교하면 두 번째 인자의 Y2 비중이 작아진 것 이외에 해석상 큰 차이는 없다.

#연습문제 8.10 => 모두 varimax 회전 사용
Weather <- read.csv('Table7.6.csv')
Weather

#주성분법
Weather.prin <- principal(Weather,nfactors=2,rotate='varimax')
Weather.prin
#인자 개수는 2개일때 누적설명분산이 0.74이므로(70~90% 사이), 2개가 적당하다.
#첫 번째 인자는 Y7,Y8,Y10을 제외한 모든 변수에서 비중이 높다. 두 번째 인자는 Y5,Y8,Y9,Y10에서 비중이 높다.

#최대우도법
fact <- factanal(Weather,factors=4,rotation="varimax",scores='regression')
fact
#인자 개수는 4개일때부터 p-value값이 0.655> 0.05이기 때문에, 4개가 적당하다.
#첫 번째 인자는 Y1,Y2,Y3,Y4,Y5,Y6에서 비중이 높다. 두 번째 인자는 Y9.Y11에서 비중이 높다. 세 번째 인자는 Y5,Y8,Y10에서 비중이 높다. 네 번째 인자는 Y7에서 비중이 높다.

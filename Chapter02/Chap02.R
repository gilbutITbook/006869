# vers. 1.0
### ----- 제2장 -----###

# RStudio 기초
1 + 2
2 * 3 * 4
8 / 4 / 2


#　１부터 10까지의 정수
1:10
# 1부터 10까지의 합
sum (1:10)
# 1부터 10까지의 평균
mean (1:10)


# 파일 읽기 창을 통해 데이터 읽기
breads <- read.csv (file.choose()) 
# Chapter02/breads.csv 선택 
## 파일명을 지정할 수도 있다. 단, 파일 위치 지정에 주의해야 한다.
# breads <- read.csv ("Chapter02/breads.csv")
## breads <- read.csv (file.choose())
# 데이터 앞부분만 표시
head (breads)

# 평균값 
mean (breads$weight)
# 표준편차 
sd (breads$weight)


# 표준편차의 의미 
# 히스토그램을 그려본다
library (ggplot2)
ggplot (breads, aes(x = weight)) + geom_histogram(binwidth = 10, fill = "steelblue", colour="black",  alpha = 0.5) + xlab("식빵의 무게") + ylab ("개수") + ggtitle("식빵 히스토그램")

## 정규분포(주사위의 기댓값 예) 
mean (sample (1:6, 10,replace = TRUE))


## 주사위를 10회 던지는 시뮬레이션을 100000회 반복
# 10만회 실시
res <- replicate(100000,  mean (sample(1:6, 100, replace = TRUE)))


# 간단한 히스토그램 생성 
hist(res)

# 좀 더 고급스러운 그래프 생성
library(ggplot2)
# 데이터 가공　
dice <- data.frame(주사위= res)
head (dice)


ggplot(dice, aes (x = 주사위)) + geom_histogram(aes(y = ..density..),binwidth = .1, fill = "steelblue", colour="black",  alpha = 0.5) + xlab("기댓값") + ylab ("") + ggtitle("주사위 평균값의 평균값") +   stat_function(geom="line", fun = dnorm, args=list(mean = mean (dice$주사위), sd = sd (dice$주사위)))
##  +stat_function 이후 부분은 정규분포의 산 모양 곡선을 추가하는 코드

# 평균값 차이 검증
t.test (breads$weight, mu = 400)



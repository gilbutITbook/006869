# vers. 1.0
### ----- 1장----- ###

# 왼쪽 기호를 샵 기호(#)라고 하며,
# RStudio(R)는 이 기호 오른쪽에 있는 내용을 무시합니다.
# 이 기호를 이용해서 주석을 기록해둡니다.



# R을 이용한 추첨 시뮬레이션
# 꽝과 1등을 뽑는 lottery 작성
lottery <- c("꽝","1등")
# lottery 내용 확인
lottery

# lottery의 요소는 두 개지만 R 명령으로
# '꽝'과 '1등'을 99:1의 비율로 등장하도록
# 조정할 수 있다.
sample(lottery, 1, prob = c(99,1))
# sample은 무작위(랜덤)로 추출하는 함수로
# 위에서 하나만 추출하고 있다

# 100개 추출하는 경우 replace = TRUE를 추가로 지정하면 된다.
# 몇 번 추출하든지 99:1 비율으 변하지 않는다.
sample(lottery, 100, prob = c(99,1), replace = TRUE)


#　추첨을 매일 100회, 일주일동안 뽑아서 1등이 나온 횟수를 시뮬레이션
replicate (7, sample (lottery, 100, prob = c(99,1), replace = TRUE) ) # 100행 7열의 결과가 표시된다
# replicate 라는 함수는 명령을 지정한 횟수만큼 반복 실행한다
### 구문: replicate (횟수, 명령)

# １등이 나온 횟수를 센다
sum ( replicate (7, sample (lottery, 100, prob = c(99,1), replace = TRUE) ) == "1등")
### 구문  sum (A == "1등"), A 안에 포함된 1등을 세는 명령


# 100회 추첨하는 것을 7일 반복해서 1등이 나온 총 횟수를 구한다.
# 그리고 이 계산을 1000주 반복하는 명령을 작성한다
# 결과는 res로 저장한다
res <- replicate(1000, sum(replicate (7, sample (lottery, 100, prob = c(99,1), replace = TRUE) ) == "1등"))

### 구문 　replicate (1000, 합계（replicate (7, 100회 추첨했을 때의 1등이 나온 횟수）)

# 간단한 히스토그램 작성 
hist(res)

#  좀 더 정교한 그래프 생성
# ggplot2를 아직 설치하지 않은 경우 
# 책 부록에 있는 '번외편'을 참고할 것.
# 또는 아래의 명령을(앞의 #는 삭제한 후) 실행해도 된다
# install.packages("ggplot2")
# 참고로 패키지 설치는 한 번만 하면 된다
# (다른 날에 R스튜디오를 실행하는 경우, 이미 ggplot2를 설치했다면,
# 다시 설치 과정을 실행할 필요가 없다. 

# ggplt2를 사용하기 위해 패키지를 읽는다
library(ggplot2)

### 그래프를 그리기 위해 데이터 가공하기 
resD <- as.data.frame(table(res))
# 위에서 10줄만 표시
head (resD, n = 10)# 

### 수치를 랜덤 생성하고 있으므로 실행 결과는 매번 달라진다
ggplot(resD, aes(y=Freq, x = res)) +  geom_bar(stat= "identity", fill = "steelblue") + xlab("주당 당첨 횟수") + ylab ("누적 기간") + ggtitle("히스토그램") 



# RStudio 가지고 놀기

# 덧셈
1 + 2 + 3 + 4 + 5

sum (1:1000)

1:6

sample (1:6, 1)
sample (1:6, 1)

sample (1:6, 100, replace = TRUE)

# 재귀
lottery <- c("꽝","1등")
lottery # 확인
# lottery에서 한 개를 꺼낸다. 단, '꽝'과 '1등'이 99:1의 비율로 나오도록 설정
sample(lottery, 1, prob = c(99,1))

sample(lottery, 100, prob = c(99,1), replace = TRUE)


# 주사위를 100회(100개) 던진다
table (sample (1:6, 100, replace = TRUE))

# 히스토그램
hist (sample (1:6, 100, replace = TRUE), breaks = 0:6)


# 주사위의 평균값(랜덤으로 던진 결과이므로 매번 숫자가 다르다）
mean(sample (1:6, 100, replace = TRUE))

# 주사위를 1만번 던지는 시뮬레이션과 히스토그램
res <- sample(1:6, 10000, replace = TRUE)

# 긴단한 히스토그램
hist(res, breaks = 0:6)

# 좀 더 정교한 그래프를 작성하기 위한 데이터 가공 
dice <- data.frame(주사위= res)
# 시작 부분 확인
head (dice)

# 그래프 그리기
ggplot(dice, aes (x = 주사위)) + xlim(1,6) + geom_histogram(binwidth = 1 , fill = "steelblue", colour="black",  alpha = 0.5) + xlab("나온 수") + ylab ("횟수") + ggtitle("주사위를 1만번 던진 결과") 


# 주사위의 평균값과 기댓값
# 여기서도 명령을 반복 실행하는 replicate 사용
res <- replicate(100000,  mean (sample(1:6, 100,replace = TRUE)))
# 간단한 히스토그램
hist(res)

# 좀 더 정교한 그래프를 생성하기 위한 데이터 가공 
dice <- data.frame(주사위 = res)
# 그래프 그리기
ggplot(dice, aes (x = 주사위)) + geom_histogram(binwidth = .1, fill = "steelblue", colour="black",  alpha = 0.5) + xlab("기댓값") + ylab ("횟수") + ggtitle("주사위를 100회 던진 기댓값") 


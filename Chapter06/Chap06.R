# vers. 1.0
### ----- 제6장 ----- ###


# Code06-01
# 철수 씨의 도둑 피해 데이터
chulsoo <- read.csv(file.choose(), colClasses = c("numeric","factor","Date","factor","factor"))
# "Chapter06/chulsoo.csv"를 선택한다
# chulsoo <- read.csv("Chapter06/chulsoo.csv", colClasses = c("numeric","factor","Date","factor","factor"))

# 시계열 데이터를 편리하게 처리할 있는 패키지 설치
install.packages("xts")
library("xts") 

# 데이터에 날짜를 지정
lostD <- seq(as.Date("2014-10-1"), as.Date("2015-5-30"), by = "days")
lost <- xts(chulsoo$피해금액, order.by = lostD, dateFormat = "Date")



## chulsoo %>% group_by(요일) %>% summarize (mean = mean (피해금액), sd  = sd (피해금액))# filter (요일 == "월요일")
## chulsoo %>% summary #summary (chulsoo)

# 그래프1
library(dplyr)
lost %>% plot (type = "l", main = "도둑 피해금액")
# plot(lost, type = "l", main = "도둑 피해금액")



# Code06-02
# 그래프2
lost[1:31] %>% plot ( type = "l",main = "도둑 피해금액(10月）")#  plot(lost[1:30], type = "l",main = "도둑 피해금액(10月）")

# Code-06-03
# 해석 선배의 그래프(2주 주기성 확인)
lost %>% acf #acf(lost)#


# 달력 그래프를 추가하기 위해 필요한 패키지
 install.packages("openair")
## 설치 시에 '사용할 수 있는 바이너리 버전이 있습니다.'하고 
## y 또는 n을 입력하도록 묻는 경우가 있다. 이때는 n을 누르면 된다.

library(openair)


# Code06-04
# 해석 선배 작성 그래프(달력 그래프)
calendarPlot(chulsoo, pollutant = "피해금액", year = c("2014"))#한글 부분이 깨져나올 수 있음


calendarPlot(chulsoo, pollutant = "피해금액", year = c("2015"))
# dev.off()

# 로지스틱 회귀 분석
# 일요일부터 시작하도록 데이터 수정
chulsoo$요일 <- factor ( chulsoo$요일, levels = c( "일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일" ))
levels (chulsoo$요일 )

chulsoo.glm <- chulsoo %>% glm (손해 ~ 요일 + 행사일, data = ., family = binomial)
# chulsoo.glm <- glm (손해 ~ 요일 + 행사일, data = chulsoo, family = binomial)

chulsoo.glm %>% summary
# summary (chulsoo.glm)

# 승산비 출력
chulsoo.glm$coefficients %>% exp %>% round(2)
# round (exp(chulsoo.glm$coefficients), 2)

# RStudio를 이용한 로지스틱 회귀 분석
load("chapter06/birthwt.rda") 
# 또는 
load(file.choose())
head(birthwt)

bw.glm <- birthwt %>% glm(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv, data = .,family = binomial)
bw.glm %>% summary

bw.glm$coefficients %>% exp %>% round(2)

###################################
## R 부속 데이터로 로지스틱 회귀 분석
## （이하의 결과는 책의 결과와 다릅니다）
data(birthwt, package = "MASS")

bw.glm <- birthwt %>% glm(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv, data = .,family = binomial)
# bw.glm <- glm(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv, data = birthwt,family = binomial)

bw.glm %>% summary 
# summary (bw.glm)

bw.glm$coefficients %>% exp %>%  round(2)



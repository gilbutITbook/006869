# ver1.0
### ----- 제4장 ---- ###


# 회장님에게서 받은 데이터를 분석 사장이 정리한 이후 부분부시 시작
menus <- read.csv(file.choose(), stringsAsFactors =  FALSE, colClasses = c("factor","Date","numeric"))
# /Chapter04/menus.csv 선택
# menus <- read.csv("Chapter04/menus.csv", stringsAsFactors =  FALSE, colClasses = c("factor","Date","numeric"))
# 세 개의 열이 각각 다른 성질을 가지고 있다.
# 품목은 카테고리고 날짜는 날짜형 데이터, 매상은 숫자다.
# 데이터를 읽을 때 각각데 데이터 형을 지정하고 있다(읽은 후에 설정할 수도 있다)

# 데이터 처리 준비
library(dplyr)

# 열 명 확인
menus %>% names # names (menus)와 같음

# 데이터 처음 부분
menus %>% head # head (menus)


# Code 04-01 # 시계열 그래프 작성 준비

library(ggplot2)

# 삼각김밥의 매상을 추출
onigiri <- menus %>%   filter (품목== "삼각김밥")  
# 시계열 그래프（scale_x_date로 지정）
ggplot(onigiri, aes(날짜, 매상)) + geom_line() +  scale_x_date()  + ggtitle("삼각김밥 매상")
# 파이프 처리를 사용하면
# onigiri %>% ggplot(aes(날짜, 매상)) + geom_line() +  scale_x_date()  + ggtitle("おにぎりの매상")

# Code04-02
# 볶음밥 매상
rice <- menus %>% filter (품목 == "볶음밥")  
#시계열 그래프
rice %>% ggplot( aes(날짜, 매상)) + geom_line() +  scale_x_date()  + ggtitle("볶음밥 매상")


# Code04-03
# 면 종류 추출
noodles <- menus %>%  filter (품목 %in% c( "스파게티", "비빔국수", "우동","짬뽕", "라면"))
# 시계열 그래프
noodles %>% ggplot(aes(날짜, 매상))+geom_line()+ facet_wrap (~품목) + ggtitle("면종류 매상")

# Code-4-04
# 상관 계수 
# 데이터 정리를 위한 패키지 도입
install.packages("tidyr") 
library(tidyr)

# 상관 행렬 작성을 위한 데이터 정리
noodles2 <- menus %>%  filter (품목 %in% c("삼각김밥", "된장국", "카레", "국밥","스파게티", "비빔국수", "우동", "짬뽕", "라면")) %>%  spread (품목, 매상) 

# 처음 부분 확인
head (noodles2)
# 상관 계수 구하기(-1은 날짜 부분을 제외한다）
noodles2 [, -1] %>% cor #cor (noodles2 [, -1])
## 다음은 Windows 이외의 OS에서만 동작한다
# noodles %>% select (-날짜) %>% cor

# 분포도 행렬 #첫 번째 열은 날짜이므로 제외하고 있다
noodles2[, -1] %>% pairs #pairs ( noodles2[, -1] )

# Code 04-5
# 우동과 삼각김밥의 분포도
udon <- menus %>% filter (품목 %in% c("삼각김밥", "우동")) %>% spread (품목, 매상)
udon %>% ggplot(aes(우동, 삼각김밥)) + geom_point() + ggtitle("우동과 삼각김밥의 분포도")


# Code 04-06
# 삼각김밥과 우유의 분포도
milk <- menus %>%  filter (품목 %in% c("삼각김밥", "우유")) %>%  spread (품목, 매상)

milk %>% ggplot(aes(삼각김밥 , 우유)) + geom_point(size = 2, color = "grey50")  + geom_smooth(method = "lm", se = FALSE) + ggtitle("삼각김밥과 우유의 분포도")

# Code 04-07
# 우유 매상
milk2 <-menus %>% filter (품목 == "우유")
# 시계열 그래프
milk2 %>% ggplot(aes(날짜, 매상)) + geom_line() +  scale_x_date()  + ggtitle("우유 매상")


# Code 04-08 상관이 있다? 없다?
# 신장과 연수입 데이터
heights <- read.csv(file.choose())
#  Chapter04/heights.csv를 선택
# heights <- read.csv("Chapter04/heights.csv")

# 상관 관계는 작다
heights %>% cor # cor (heights)

ggplot(heights, aes( 키,연수입)) + geom_point() +  ggtitle("신장과 연수입의 상관？")
heights %>% ggplot(aes( 키,연수입)) + geom_point() +  ggtitle("신장과 연수입의 상관")


# Code-04-09
# 무상관 데이터
xy <- data.frame (X = -10:10, Y =  (-10:10)^2)
xy %>% ggplot(aes (x = X, y = Y)) + geom_point(size  =2 )+ ggtitle("상관 관계가 있는 것 같지만？")
# ggplot(xy, aes (x = X, y = Y)) + geom_point(size  =2 )+ ggtitle("상관 관계가 있는 것 같지만？")
#

# 회귀 분석 
# 아이스크림 판매 수 데이터를 읽어 icecream에 저장한다
# Code04-09
icecream <- read.csv(file.choose())
#  Chapter04/icecream.csv을 선택한다
# icecream <- read.csv("Chapter04/icecream.csv")
icecream %>% head #head (icecream)
# 기온과 판매 수의 분포도 작성
icecream %>% ggplot(aes(기온, 판매수)) + geom_point(size = 2)
# ggplot(icecream, aes(기온,  판매수)) + geom_point(size = 2)

# 상관 계수 구하기
icecream %>% select (판매수, 기온) %>% cor

# lm 함수로 기울기와 절편 구하기
lm(판매수 ~ 기온, data = icecream)
# 파이프 처리를 사용하면
# icecream %>% lm(판매수 ~ 기온, data = .)

# 회귀 직선을 추가한 분포도 작성
ggplot(icecream, aes(기온, 판매수)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE)
# 파이프 처리를 사용하면
# icecream %>% ggplot(aes(기온, 판매수)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE)


# 귀무 가설 '기울기는 0이다'를 검증한 결과와 결정 계수
summary (lm(판매수 ~ 기온, data = icecream))
# 파이프 처리를 사용하면
# icecream %>% lm(판매수 ~ 기온, data = .) %>% summary
# 


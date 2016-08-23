# vers. 1.0
### ----- 제3장  ----- ###

# 데이터를 분할표로 표현하기
dat <- read.csv (file.choose())# AmosWIN/Chapter03/sample.csv를 선택
# dat <- read.csv ("Chapter03/sample.csv")
dat

table (dat)

# 파이프 연산자를 사용할 수 있는 패키지 설치
# (패키지 설치 방법에 대해선 '번외편'을 참고
# install.packages("dplyr") 

# 패키지 이용 선언
library (dplyr) 

# 파이프 연사자 사용해보기(왼쪽 데이터를 오른쪽 처리에 적용한다)
dat %>% table

# 분할표 저장
dat2 <-  dat %>% table
dat2

# 카이제곱 검증 실행
dat2 %>% chisq.test 
# chisq.test (dat2)# 이렇게 해도 동일한 결과를 얻을 수 있다

# 독립성 검증의 의미
survey <- read.csv(file.choose())# AmosWIN/Chapter03/survey.csv 선택
# survey <- read.csv("Chapter03/survey.csv")
survey %>% head # head (survey) 와 같다


# 열을 선택해서 분할표 작성
table1 <- survey  %>% select (입장, 응답6) %>% table
table1

# 그래프(누적 막대 그래프 작성)
library(ggplot2)
table1 %>% as.data.frame %>% ggplot(aes (x = 입장, y = Freq, fill = 응답6 )) + geom_bar(stat="identity") + ylab ("인수") 

# 참고: 간단한 막대 그래프
table1 %>% plot #   plot (table1)과 같음

# 카이제곱 검증
# 귀무 가설 '점주와 손님의 응답은 독립적이다'를 검증
survey  %>% select (입장, 응답6) %>% table %>% chisq.test


# 이건 뭐지?
# 응답7에 대한 분할표 작성
survey  %>% select (입장, 응답7) %>% table


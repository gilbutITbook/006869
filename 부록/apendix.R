### 부록: 한국어 형태소 분석 ###

install.packages("KoNLP")
library(KoNLP)
# 사전 설정
useSejongDic()
#useSystemDic()

# 소설 'B사감과 러브레터'에서 명사 추출하기기
# 소설 파일 읽기 
sagam <- file("sagam.txt",  encoding = "UTF-8")

# 읽은 파일을 한 줄씩 저장. 
novel <- readLines(sagam)

# 한 줄 단위로 분석해서 명사 추출 
myungsa <- sapply(novel, extractNoun, USE.NAMES = T)
extractNoun("아버지가 가방에 들어가신다")

# 처음 3줄에서 명사를 추출한 결과 
head(myungsa, 3)

# 형태소 분석 
hyungtaeso<- sapply(novel, MorphAnalyzer, USE.NAMES = F)
MorphAnalyzer("아버지가 가방에 들어가신다")
head(hyungtaeso, 1)

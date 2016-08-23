# ver1.0
###----- 5장  -----###
## RMeCab을 설치한다. 사전에 Mecab을 설치해야 한다
## 
install.packages("RMeCab", repos = "http://rmecab.jp/R")
library(RMeCab)


# '달려라 멜로스'를 형태소 분석한다. 명사, 형용사, 동사를 추출한다
m <- NgramDF(file.choose(), type = 1, pos =  c("名詞","形容詞","動詞")) # AmosWIN/Chapter05/merosu.txt를 선택
#m <- NgramDF("merosu.txt", type = 1, N=3)

library(dplyr)
nrow (m) #파이프 처리의 경우 m %>% nrow #
# 데이터가 크기 때문에 처음 부분만 표시
head (m) # m %>% head #head (m)

# Code05-01
# 자주 사용되는 단어를 추린다(여기선 2회 이상 사용된 단어 선택）
m.df <- m %>% filter(Freq > 2)


# 네트워크 그래프용 패키지를 설치한다
install.packages("igraph")
library(igraph) 

# 단어의 연결을 네트워크 그래프로 표현
m.g <- graph.data.frame(m.df)
E(m.g)$weight <- m.df[,3] #

# 맥(Mac) 의 경우、Xquartz 설치 필요. http://xquartz.macosforge.org/landing/

tkplot(m.g, vertex.label =V(m.g)$name, edge.label =E(m.g)$weight , vertex.size = 23, vertex.color = "SkyBlue")
# 참고로 생성된 네트워크 그래프상에선 점이나 선을 클릭해서 이동할 수 있다. 
# 이외에도 그래프용 옵션을 메뉴에서 선택할 수 있으니 시험해보자.。


# 오가이와 쇼세키 
# Code05-02
m <- docNgram ("bungo", type = 0) 

# 열 이름을 알기 쉽게 변경
colnames (m) <- c("오가이1","오가이2","오가이3","오가이4","소세키1","소세키2","소세키3","소세키4")


# 현재는 m에 대량의 정보가 있다. 여기서 조사와 쉼표로 구성된 8개 조합으로 추린다
m <- m [ rownames(m) %in% c("[と-、]", "[て-、]", "[は-、]", "[が-、]", "[で-、]",  "[に-、]",  "[ら-、]",  "[も-、]" ),  ]
#m <- m [ rownames(m) %in% c("[하면]", "[로]", "[는]", "[지만]", "[에서]",  "[에]",  "[들]",  "[도]" ) ,  ]
# 8행 8열의 데이터 확인
m 

# Code05-03
# 「で」(에서)와「が」(지만)을 분포도로 작성.  
dega <- data.frame(지만 = m[1,] ,에서 = m[3,],작가=c("오가이","오가이","오가이","오가이","소세키","소세키","소세키","소세키"))
library(ggplot2)

dega %>% ggplot(aes(x = 지만, y = 에서 , group=작가 ) ) + geom_point(aes(shape = 작가), size = 6) + scale_shape(solid = FALSE)# +scale_shape_manual(values=c(22,23))
# ggplot(dega, aes(x = が, y = で , group=작가 ) ) + geom_point(aes(shape = 작가), size = 6) + scale_shape(solid = FALSE)

# Code05-04
# 오사무의 4개 작품을 추가한 폴더 분석
m2 <- docNgram ("dazai", type = 0) 
colnames(m2) <- c("오사무1","오사무2","오사무3","오사무4",
                  "오가이1","오가이2","오가이3","오가이4",
                  "소세키1","소세키2","소세키3","소세키 4")

m2 <- m2 [ rownames(m2) %in% c("[と-、]", "[て-、]", "[は-、]", "[が-、]", "[で-、]",  "[に-、]",  "[ら-、]",  "[も-、]" ) ,  ]


dega2 <- data.frame(지만 = m2[1,] , 에서 = m2[3,],작가=c("오사무","오사무","오사무","오사무",
                                                "오가이","오가이","오가이","오가이",
                                                "소세키","소세키","소세키","소세키"))
dega2

dega2 %>% ggplot(aes(x = 지만, y = 에서 , group=작가 ) ) + geom_point(aes(shape = 작가),size = 6) +scale_shape_manual(values=c(21,15,24))
# ggplot(dega2, aes(x = が, y = で , group=작가 ) ) + geom_point(aes(shape = 작가), size = 6) +scale_shape_manual(values=c(21,15,24))

# Code05-05
# 주성분 분석
# 파이프 도중에 함수를 넣어서 데이터의 가로, 세로를 바꾼다(즉, 행과 열을 바꾼다)
m2.pca <- m2 %>% t %>% prcomp # m2.pca <- prcomp(t(m2))
m2.pca　%>% biplot (cex = 1.0) # cex = 1.0 문자 크기를 조절한다

# Code05-06
# 주성분 확인
round (m2.pca[[2]], 2) 


############################

## 악성 고객의 메일과 블로그 데이터
sentence <- read.csv(file.choose(), row.name = 1)#mb.csv 선택
# AmosWIN/Chapter05/mb.csv 선택한다
# sentence <- read.csv("Chapter05/mb.csv", row.name = 1)

sentence
# 참고로 본문의 표와 출력 결과가 다릅니다. 책이 잘못된 것입니다 m(_ _)m

## 주성분 분석
blog <- sentence %>% prcomp # blog <- prcomp(sentence)

blog %>% biplot # biplot(blog, cex = 1.0)


## 댓글 분석


# Code05-07
# 인터넷상의 사이트에서 페이지를 추출하는 rvest 패키지를 설치한다
install.packages("rvest") 
library(rvest)
library(dplyr)

# Wiki의 화투 페이지를 가져온다
wiki <- read_html("http://ko.wikipedia.org/wiki/%ED%99%94%ED%88%AC")
hwatoo <- wiki %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td") %>% html_text() 
dim(hwatoo) <- c(16,12)
hwatoo %>% t %>% as.table %>% iconv (from = "UTF-8")


#Mac을 사용한다면 다음과 같이 실행하면 된다(일본어 문자 코드드 문제로 윈도우에선 제대로 동작하지 않는다.）
# wiki <- read_html("http://ko.wikipedia.org/wiki/%ED%99%94%ED%88%AC)
# hana <- html_table(html_nodes(wiki, "table")[[3]])
# hana


# Code05-08
# 댓글 분석
reply <- read.csv(file.choose(), row.name = 1)
# AmosWIN/Chapter05/reply.csv 선택
# reply <- read.csv("reply.csv", row.name = 1)
reply %>% head 


# Code05-09
## 계통도
reply.clus <- reply %>% t %>% dist %>% hclust 
reply.clus <- hclust(dist(t(reply)))
reply.clus %>% plot 

# 계통도 전용 패키지
install.packages("ggdendro") 
library("ggdendro") 
#reply.clus %>% ggdendrogram(rotate = FALSE,size = 20) + labs(title= "댓글 클러스터") + xlab ("클러스터") + ylab( "유사성") + theme_bw(base_size = 18)


# Code05-10
## 대응 분석
reply.cor <- reply %>% MASS::corresp(nf = 2)
# reply.cor <- MASS::corresp(reply, nf = 2)

reply.cor %>% biplot(cex = 1.0)



# 대응 분석 예
HE <- HairEyeColor[,,2]


dimnames(HE) <- list (hair=c("검정","갈색","빨강","금색"), eyes=c("갈색","파랑","밤색","초록"))
HE

HEca <- HE %>% MASS::corresp(nf = 2)
#HEca  <- MASS::corresp(HE,nf=2)


HEca %>% biplot 


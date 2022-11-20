install.packages("widyr")
devtools::install_github("lchiffon/wordcloud2")
install.packages("wordcloud2")
install.packages("dplyr")
install.packages("wordcloud")

# 라이브러리
library(rvest)
library(httr)
library(wordcloud)
library(dplyr)

start=(0:9)*10+1
crawl=list()
n=1

https://search.naver.com/search.naver?where=news&sm=tab_jum&query=%EC%88%98%EB%8F%84%EA%B6%8C%EB%A7%A4%EB%A6%BD%EC%A7%80

for (i in start) {
  url= paste0("https://search.naver.com/search.naver?where=news&sm=tab_jum&query=%EC%88%98%EB%8F%84%EA%B6%8C%EB%A7%A4%EB%A6%BD%EC%A7%80&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=33&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start=",i)
  data=GET(url)
  data_title <- data %>%
    read_html(encoding="UTF-8") %>%
    html_nodes('.news_area') %>%
    html_nodes('a') %>%
    html_attr('title')
  crawl[[n]] <- data_title
  n=n+1  
}

head(crawl)

crawl <- unlist(crawl)
crawl <- crawl[complete.cases(crawl)]
crawl


# 제대로 분석 "수도권매립지" 검색 키워드:3000페이지
start=(0:299)*10+1
start
n=1
crawl=list()
crawl
for (i in start) {
  url= paste0("https://search.naver.com/search.naver?where=news&sm=tab_jum&query=%EC%88%98%EB%8F%84%EA%B6%8C%EB%A7%A4%EB%A6%BD%EC%A7%80&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=33&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start=",i)
  data=GET(url)
  data_title <- data %>%
    read_html(encoding="UTF-8") %>%
    html_nodes('.news_area') %>%
    html_nodes('a') %>%
    html_attr('title')
  crawl[[n]] <- data_title
  n=n+1
}

str(crawl)
is.na(crawl)
# 결측값 제거 

crawl <- unlist(crawl)
crawl <- crawl[complete.cases(crawl)]
head(crawl)
str(crawl)


write.csv(crawl,file="C:/Users/son beom su/Desktop/2022_2학기 학교/데이터시각화/데이터시각화 project/crawl.csv")


facebook <- file("C:/Users/son beom su/Desktop/2022_2학기 학교/데이터시각화/데이터시각화 project/crawl1.txt", encoding="UTF-8")
facebook_data <- readLines(facebook) # 줄 단위 데이터 생성

head(facebook_data)

gsub("[[:punct:]]", "", facebook_data)


# 사용자 정의 함수 실행 순서 : 문자변환 -> 명사 단어추출 -> 공백으로 합침
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse=" ")}

# exNouns 함수 이용 단어 추출 
# 형식) sapply(적용 데이터, 적용함수)
facebook_nouns <- sapply(facebook_data, exNouns) 
facebook_nouns


mycorpus <- Corpus(VectorSource(facebook_nouns))
mycorpus


myCorpusPrepro<- tm_map(myCorpus, removePunctuation) # 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removeNumbers) # 수치 제거


# 결과 확인 
inspect(myCorpusPrepro[1])

myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, 
                                          control=list(wordLengths=c(4,16))) 
myCorpusPrepro_term

# 데이터프레임 변환환
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term)) 
dim(myTerm_df)

# 단어 빈도수 내림차순 정렬
wordResult <- sort(rowSums(myTerm_df), decreasing=TRUE) 
wordResult[1:50]

# 불용어 제거
myCorpusPrepro <- tm_map(myCorpus, removeNumbers)

myName <- names(wordResult) # 단어 이름 생성 -> 빈도수의 이름 
wordcloud(myName, wordResult) # 단어구름 적성


word.df <- data.frame(word=myName, freq=wordResult) 
str(word.df) # word, freq 변수
# (2) 단어 색상과 글꼴 지정
pal <- brewer.pal(12,"Paired") # 12가지 색상 pal <- brewer.pal(9,"Set1") # Set1~ Set3
# 폰트 설정세팅 : "맑은 고딕", "서울남산체 B"
windowsFonts(malgun=windowsFont("맑은 고딕"))  #windows
# (3) 단어 구름 시각화 - 별도의 창에 색상, 빈도수, 글꼴, 회전 등의 속성을 적용하여 
x11( ) # 별도의 창을 띄우는 함수
wordcloud(word.df$word, word.df$freq, 
          scale=c(5,1), min.freq=2, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")

#(1) 상위 20개 토픽추출
topWord <- head(sort(wordResult, decreasing=T), 20) # 상위 20개 토픽추출 
# (2) 파일 차트 생성 
pie(topWord, col=rainbow(20), radius=100) # 파이 차트-무지개색, 원크기
# (3) 빈도수 백분율 적용 
pct <- round(topWord/sum(topWord)*100, 1) # 백분율
names(topWord)
# (4) 단어와 백분율 하나로 합친다.
lab <- paste(names(topWord), "\n", pct, "%")
# (5) 파이차트에 단어와 백분율을 레이블로 적용 
pie(topWord, main="네이버 수도권매립지 검색 토픽 TOP 20", col=rainbow(10), cex=0.8, labels=lab)
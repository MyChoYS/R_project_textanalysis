library(RSelenium);library(stringr);library(tm);library(sys);library(wordcloud);library(wordcloud2);library(tm)
library(dplyr);library(KoNLP);library(RColorBrewer)
library(stopwords);library(proxy) ; library(qgraph)

#마케팅 직무에서 가장 많이 지원한 회사명
comname <- read.csv("output/all_comname_데이터.csv", header= T)
comname <- (comname$x)
comname
View(comname)
comname <- table(comname)

sort(comname,decreasing = T)[1:10] #상위 10개 
windowsFonts(dog=windowsFont("THE개이득"))
?wordcloud2

wordcloud2(comname,size=0.7,col="random-light",backgroundColor = "white", shape = "circle",
           fontFamily = windowsFont("THE개이득"))

#마케팅 직무에서 가장 많이 가지고 있는 스펙확인
actspec <- read.csv("output/act_spec_데이터.csv")
actspec <- actspec$x
actspec_count <- table(actspec)

##############################
#스펙활동 데이터전처리 
actspec[which(str_detect(actspec,"학점"))] <- NA #학점 제거 
actspec[which(str_detect(actspec,"4.5"))] <- NA #학점 제거 
actspec[which(str_detect(actspec,"봉사활동"))] <- "봉사활동"
actspec[which(str_detect(actspec,"사회생활 경험"))] <- "인턴"
actspec[which(str_detect(actspec,"인턴"))] <- "인턴"
actspec[which(str_detect(actspec,"공모전"))] <- "공모전"
actspec[which(str_detect(actspec,"경영"))] <- "경영학과"
actspec[which(str_detect(actspec,"대외활동"))] <- "대외활동"
actspec[which(str_detect(actspec,"ih"))] <- "오픽 IH"
actspec[which(str_detect(actspec,"al"))] <- "오픽 AL"
actspec[which(str_detect(actspec,"AL"))] <- "오픽 AL"
actspec[which(str_detect(actspec,"레벨6"))] <- "토익스피킹 레벨6"
actspec[which(str_detect(actspec,"수상"))] <- "수상"
actspec[which(str_detect(actspec,"급"))] <- "자격증"
actspec[which(str_detect(actspec,"기사"))] <- "자격증"
actspec[which(str_detect(actspec,"자격증"))] <- "자격증"
actspec[which(str_detect(actspec,"토익 980"))] <- "TOEIC 900 이상"
actspec[which(str_detect(actspec,"경제"))] <- "경제학과"
actspec[which(str_detect(actspec,"신문"))] <- "신문방송학과"
actspec[which(str_detect(actspec,"교환"))] <- "해외경험"
actspec[which(str_detect(actspec,"해외"))] <- "해외경험"
actspec[which(str_detect(actspec,"서성한"))] <- NA #학교명은 따로 다루기 때문에 삭제 
actspec[which(str_detect(actspec,"중경외시"))] <- NA
actspec[which(str_detect(actspec,"지거국"))] <- NA
actspec[which(str_detect(actspec,"SKY"))] <- NA
actspec[which(str_detect(actspec,"sky"))] <- NA

actspec[which(as.numeric(gsub("\u6709","",gsub("[[A-z][:space:]]","",gsub("[가-힣:() ]","",actspec[which(str_detect(gsub("[:space:]","",actspec),"토익"))])))) > 900 )] <- "TOEIC 900 이상"
actspec[which(as.numeric(gsub("\u6709","",gsub("[[A-z][:space:]]","",gsub("[가-힣:() ]","",actspec[which(str_detect(gsub("[:space:]","",actspec),"토익"))])))) > 800 )] <- "TOEIC 800 이상"


mostspec <- sort(table(actspec),decreasing = T)
View(mostspec)
#가장많이보유한 스펙 - 마케팅 워드클라우드 

wordcloud2(mostspec,size=0.7,col="random-light",backgroundColor = "white", shape = "circle",
           fontFamily = windowsFont("THE개이득"))

##############
#평균학점, 가장많이 보이는 대학교, 자소서 데이터전처리
int <- read.csv("output/all_spec_데이터.csv")

int %>%
  group_by(학교명) %>% summarise(mean_univ = mean(학점)) -> a


avg <- int$학점
avg <- avg[!is.na(avg)]
avg <- mean(avg)
avg #마케팅자소서 평균 학점 3.75 
table(int$스펙수)
avg_spec <- int$스펙수
avg_spec[which(avg_spec <= 0)] <- NA #0 이하의 스펙들은 결측치로 처리 
avg_spec <- avg_spec[which(!is.na(avg_spec))]
avg_spec
avg_spec <- mean(avg_spec)
avg_spec #마케팅 지원자들은 평균 4.28개의 스펙을 지니고 있다.

###학교명 전처리
univ <- int$학교명
univ <-str_remove(univ,' ')
univ[which(str_detect(univ,"sky"))] <- "SKY"
univ[which(str_detect(univ,"스카이"))] <- "SKY"
univ[which(str_detect(univ,"서연고"))] <- "SKY"
univ[which(str_detect(univ,"SKY"))] <- "SKY"
univ[which(str_detect(univ,"서울대"))] <- "SKY"
univ[which(str_detect(univ,"연세"))] <- "SKY"
univ[which(str_detect(univ,"고려"))] <- "SKY"
univ[which(str_detect(univ,"학점"))] <- NA
univ[which(str_detect(univ,"수도권"))] <- "수도권"
univ[which(str_detect(univ,"토익"))] <- NA

univ[which(str_detect(univ,"중앙"))] <- "중경외시"
univ[which(str_detect(univ,"경희"))] <- "중경외시"
univ[which(str_detect(univ,"한국외"))] <- "중경외시"
univ[which(str_detect(univ,"서울시립"))] <- "중경외시"
univ[which(str_detect(univ,"중경외시"))] <- "중경외시"

univ[which(str_detect(univ,"서강"))] <- "서성한"
univ[which(str_detect(univ,"성균"))] <- "서성한"
univ[which(str_detect(univ,"한양"))] <- "서성한"
univ[which(str_detect(univ,"서성"))] <- "서성한"
univ[which(str_detect(univ,"서성한"))] <- "서성한"

univ[which(str_detect(univ,"건국"))] <- "건동홍"
univ[which(str_detect(univ,"동국"))] <- "건동홍"
univ[which(str_detect(univ,"홍익"))] <- "건동홍"
univ[which(str_detect(univ,"건동홍"))] <- "건동홍"

univ[which(str_detect(univ,"해외"))] <- "해외대"
univ <- table(univ)

univ[which(str_detect(univ," "))]

univ <- sort(univ,decreasing = T)
View(univ)
univ[7] <- NA #공백칸 제거 후 다시 정렬 
univ <- sort(univ,decreasing = T)

wordcloud2(univ,size=0.7,col="random-light",backgroundColor = "white", shape = "circle",
           fontFamily = windowsFont("THE개이득"))


####################여기까지 정량평가 대상 ########################################### 




################# 자기소개서 추출, 전처리####################
stops <- stopwords::stopwords("ko", source = "marimo")
mystopwords <- readLines("data/stopwords_ko.txt", encoding="UTF-8")
write(stops,"data/stopwords_ko2.txt") #마리모 스탑워드 추가 stopwords_ko로 합침 

stopwords::stopwords_getlanguages("marimo")
lett<- int$자기소개서
lett <- gsub("[:punct:]","",lett)
lett <- gsub("■","",lett)
lett <- gsub("[:cntrl:]","",lett)

words <- extractNoun(lett) #명사추출 
words[[1]][1]






warnings()

unlist(words) -> all_words
all_words[which(nchar(all_words)>=2)] -> all_words #모든 자소서의 단어 

all_words[which(all_words=="생각")] <- NA #전처리
all_words[which(all_words=="하기")] <- NA
all_words[which(all_words=="때문")] <- NA
all_words[which(all_words=="회사")] <- NA
all_words[which(all_words=="기업")] <- NA
all_words[which(all_words=="삼성")] <- NA
all_words[which(all_words=="목표")] <- NA
all_words[which(all_words=="직무")] <- NA
all_words[which(all_words=="결과")] <- NA

sort_words<- sort(table(all_words),decreasing = T)
 mostwords <- rownames(sort_words[1:20])
 View(sort_words)
########마케팅 직무 합격 자소서에서 자주 쓰이는 단어들  +  워드클라우드
wordcloud2(sort_words,size=0.7,col="random-light",backgroundColor = "white", shape = "circle",
           fontFamily = windowsFont("THE개이득"))


View(all_words)

######################동시출현 
wordss <- NULL #두글자 이상 단어 임시저장소
twowords <- NULL #두글자 이상의 단어만 추출해서 저장, 동시출현 할것 

for (i in words){
  for (t in i){
    if (nchar(t) >= 2){
      wordss <- append(wordss,t)
    }
  }
   wordss <- list(wordss)  
   twowords <- append(twowords,wordss) 
   wordss <- NULL
}


twowordss <- NULL

#동시출현 전처리 
for (i in twowords){
    i[which(str_detect(i,"생각"))] <- NA #전처리
    i[which(str_detect(i,"하기"))] <- NA
    i[which(str_detect(i,"때문"))] <- NA
    i[which(str_detect(i,"회사"))] <- NA
    i[which(str_detect(i,"기업"))] <- NA
    i[which(str_detect(i,"삼성"))] <- NA
    i[which(str_detect(i,"목표"))] <- NA
    i[which(str_detect(i,"직무"))] <- NA
    i[which(str_detect(i,"결과"))] <- NA
    i[which(str_detect(i,"하시오"))] <- NA
    i[which(str_detect(i,"1000"))] <- NA
    i <- i[which(!is.na(i))]
    twowordss <- append(twowordss,list(i))
  
}

twowords

#단어들간의 동시출현########
a <- NULL
for (i in 1:59){
     a <- append(a,paste(twowordss[i]))
     
}
a <- gsub("[[:punct:][:cntrl:]]","",a)
a
View(a)
View(twowords)

###동시출현 시각화에 사용
cps <- VCorpus(VectorSource(a))
tdm <- TermDocumentMatrix(cps)
tdm
as.matrix(tdm)


#cps <- VCorpus(VectorSource(a))
#tdm <- TermDocumentMatrix(cps, 
#                         control=list(wordLengths = c(1, Inf))) ## c(1, Inf) -> 1글자부터 무한대로 받아옴'''

tdm
m <- as.matrix(tdm)

word.count <- rowSums(m)
word.order <- order(word.count,decreasing = T)
freq.words <- m[word.order[1:20],]
co.matrix <- freq.words %*% t(freq.words)
View(co.matrix)

colSums(m)

#com <- m %*% t(m)  #동시출현 개수
#com


qgraph(co.matrix, labels=rownames(co.matrix), diag=F,  #동시출현
       layout='spring',  edge.color='blue',
       vsize=log(diag(co.matrix)*2))





#유사도 (코사인,유클리드) 분석 
dtm <- DocumentTermMatrix(cps)
as.matrix(dtm)
inspect(dtm)
m2 <- as.matrix(dtm)
com <- m2 %*% t(m2)  
com
dist(com, method = "cosine")
which.min(dist(com, method = "cosine"))
com[2]

num <- 0
count <- 0
for (i in 1:406){
  num <- num + i
  if (num >= 19390){
    break
  }
  count <- count + 1
}
19503-19390
19503-197
19390-19306
309-196
int$자기소개서[196]
int$자기소개서[84]
com[which.min(dist(com, method = "Euclidean"))]
which.min(dist(com, method = "cosine")) #유사도 분석으로 얻는 것은 쓰잘데기 없다.
#유사도분석으로 정형화된 합격자소서말고, 가장 많이 쓰인 단어들을 포함한 자소서를 찾아보자.




#########가장 자주 쓰이는 단어 20개를 가장 많이 포함하고 있는 대표자소서 추출 
wordcount <- NULL
list(sum(str_count(qwe,mostwords)))

for (i in 1:59){
  wordcount <- append(wordcount,list(sum(str_count(twowordss[i],mostwords))))
}

wordcount[which.max(wordcount)]
int$자기소개서[which.max(wordcount)]
int$자기소개서[219]




View(int)




library(RSelenium);library(stringr);library(tm);library(sys);library(wordcloud);library(wordcloud2);library(tm)
library(dplyr);library(KoNLP);library(RColorBrewer)

#마케팅 직무에서 가장 많이 지원한 회사명
comname <- read.csv("output/all_comname_마케팅.csv", header= T)
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
actspec <- read.csv("output/act_spec_마케팅.csv")
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

#가장많이보유한 스펙 - 마케팅 워드클라우드 

wordcloud2(mostspec,size=0.7,col="random-light",backgroundColor = "white", shape = "circle",
           fontFamily = windowsFont("THE개이득"))

##############
#평균학점, 가장많이 보이는 대학교, 자소서 데이터전처리
int <- read.csv("output/all_spec_마케팅.csv")
View(int)
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




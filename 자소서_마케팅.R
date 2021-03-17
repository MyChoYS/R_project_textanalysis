library(RSelenium)
library(stringr)
library(tm)
#java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
#링커리어 합격 자소서 긁어모으기 #마케팅 직무별 긁어오기
remDr$navigate("https://linkareer.com/cover-letter/24709?page=1&role=%EB%A7%88%EC%BC%80%ED%8C%85&sort=RELEVANCE&tab=all")
all_comname <- NULL
all_spec <- NULL
act_spec <- NULL
#기업명 긁어오기 
comElem <- remDr$findElements(using = "css selector", "#__next > div.jss13091.jss13086 > div.MuiContainer-root.jss13099.jss13088.MuiContainer-disableGutters > div > div > div.MuiContainer-root.jss13402.MuiContainer-maxWidthLg > div.jss13720 > div.MuiBox-root.jss13724.jss13716 > div > div > div.MuiBox-root.jss13732 > p")

comname <- sapply(comElem, function(x) x$getElementText() )
comname <- str_split(unlist(comname)[1],"/")
comname <- comname[[1]][1]
all_comname <- append(all_comname,comname)
##################################

#스펙
webElem <- remDr$findElements(using = "css selector", "#__next > div.jss13091.jss13086 > div.MuiContainer-root.jss13099.jss13088.MuiContainer-disableGutters > div > div > div.MuiContainer-root.jss13402.MuiContainer-maxWidthLg > div.jss13720 > div.MuiBox-root.jss13724.jss13716 > div > div > div.MuiBox-root.jss13733 > p")
speclist <- list(학교명="",학점="",스펙수="",자기소개서="") #초기화
spec <- sapply(webElem, function(x) x$getElementText())
spec <- gsub("/",",",spec)
spec <- str_split(spec,',') # "/" 기준으로 나누어서 학교, 학점, 스펙으로 나눈다
spec <- unlist(spec)

speclist[1] <- spec[1] #스펙리스트에 학교명 추가
for (i in spec){ #스펙리스트에 학점 추가
  if(str_detect(i,"학점")){
    speclist[2] <- as.numeric(gsub("[^0-9.]","",spec[3]))
  }
}
speclist[3] <- length(spec)- which(str_detect(spec,"학점")) #스펙수 추가

act_spec <- append(act_spec,spec[which(str_detect(spec,"학점"))+1:length(spec)
                                 -which(str_detect(spec,"학점"))]) #스펙 내용 저장 

##################################


#자소서 긁어오기 #speclist[4]에 따와서 저장하기 
webElem1 <- remDr$findElements(using = "css selector","#coverLetterContent > main")
text <- sapply(webElem1, function(x) x$getElementText())
a<- append(a,gsub("[[:punct:][:cntrl:]]","",text))
a[1] #학교
a[2] #학점
a[3] #스펙 개수 
a[4]
View(data.frame(a))

## spec에서 /를 기준으로 나누었는데, /를 ,로 바꾸고 ,로 나누어야 스펙을 더 세분화 할 수 있을 듯 
## list화 해서 ,로 나누어진 벡터를 a[1]는 대학교, a[2]는 학점으로 본이 뒤는 모두 스펙으로 간주
## 잡코리아 합격자소서 샘플이 더 낭르거 같다. 
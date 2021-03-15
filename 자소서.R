library(RSelenium)
library(stringr)
library(tm)
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
#링커리어 합격 자소서 긁어모으기 
remDr$navigate("https://linkareer.com/cover-letter/24337?page=1&sort=SCRAP_COUNT&tab=all")

webElem <- remDr$findElements(using = "css selector", "#__next > div.jss6.jss1 > div.MuiContainer-root.jss14.jss3.MuiContainer-disableGutters > div > div > div.MuiContainer-root.jss935.MuiContainer-maxWidthLg > div.jss1215 > div.MuiBox-root.jss1219.jss1211 > div > div > div.MuiBox-root.jss1228 > p")
spec <- sapply(webElem, function(x) x$getElementText())
spec <- gsub("/",",",spec)
spec <- str_split(spec,',') # "/" 기준으로 나누어서 학교, 학점, 스펙으로 나눈다
spec[[1]][2] <- as.numeric(gsub("[^0-9.]","",spec[[1]][2])) #학점
spec[[1]][3] <- str_split(spec[[1]][3],',') #스펙 
spec[[1]][3] <-length(spec[[1]])-2 #스펙 갯수 but, 양은 측정가능하나 질은 측정 불가능하다.

#완성된 첫 스펙
a <- unlist(spec) #1:대학교 2:학점 3:스펙개수 4:대외활동 명  -> 어떤 대학교, 평균 학점, 평균 스펙개수, 가장 많이 보이는 대외활동 

#기업명 정리
webElem <- remDr$findElements(using = "#coverLetterContent > main")



#자소서 긁어오기 
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
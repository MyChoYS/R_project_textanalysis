library(RSelenium)
library(stringr)
library(tm)
library(sys)
#java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
#링커리어 합격 자소서 긁어모으기 #삼성 합격자소서 긁어오기
remDr$navigate("https://linkareer.com/cover-letter/24244?organizationName=%EC%82%BC%EC%84%B1&page=1&sort=RELEVANCE&tab=all")
all_comname <- NULL
all_spec <- NULL
act_spec <- NULL

#####

repeat{
for (i in 1:5){
#endlink <- remDr$findElement(using='xpath',value = "//*[@id='__next']/div[1]/div[2]/div/div/div[2]/div[1]/div/div[2]/div[8]/div/div[1]/button[2]/span")
Sys.sleep(1)
for(e in 1:20){
  getlink <- remDr$findElements(using='xpath',value = paste0("//*[@id='__next']/div[1]/div[2]/div/div/div[2]/div[1]/div/div[2]/div[",e,"]/a/div"))
  remDr$executeScript("arguments[0].click();",getlink)
  Sys.sleep(2)
  
  #기업명 긁어오기 
  comElem <- remDr$findElements(using = "xpath", value =  "//*[@id='__next']/div[1]/div[2]/div/div/div[2]/div[2]/div[1]/div/div/div[1]/p")
  
  comname <- sapply(comElem, function(x) x$getElementText() )
  comname <- str_split(unlist(comname)[1],"/")
  Sys.sleep(1)
  comname <- comname[[1]][1]
  all_comname <- append(all_comname,comname)
  ##################################
  Sys.sleep(1)
  #스펙
  webElem <- remDr$findElements(using = "xpath", value = "//*[@id='__next']/div[1]/div[2]/div/div/div[2]/div[2]/div[1]/div/div/div[2]/p")
  speclist <- list(학교명="",학점="",스펙수="",자기소개서="") #초기화
  spec <- sapply(webElem, function(x) x$getElementText())
  spec <- gsub("/",",",spec)
  spec <- str_split(spec,',') # "/" 기준으로 나누어서 학교, 학점, 스펙으로 나눈다
  spec <- unlist(spec)
  Sys.sleep(1)
  speclist[1] <- spec[1] #스펙리스트에 학교명 추가
  
  for (t in spec){ #스펙리스트에 학점 추가
    if(str_detect(t,"학점")){
      speclist[2] <- as.numeric(gsub("[^0-9.]","",t))
      speclist[3] <- length(spec)- which(str_detect(spec,"학점")) #스펙수 추가
    }else{
      speclist[3] <- length(spec) - 2
    }
  }
  Sys.sleep(1)
  
  act_spec <- append(act_spec,spec[which(str_detect(spec,"학점"))+1:length(spec)
                                   -which(str_detect(spec,"학점"))]) #스펙 내용 저장 
  
  ##################################
  
  #자소서 긁어오기 #speclist[4]에 저장하기 
  webElem1 <- remDr$findElements(using = "css selector","#coverLetterContent > main")
  text <- sapply(webElem1, function(x) x$getElementText())
  speclist[4]<- gsub("[[:punct:][:cntrl:]]","",text)
  all_spec <- rbind(all_spec,speclist)
  
  
  Sys.sleep(1)
  
}
alink <- remDr$findElements(using='xpath',value = paste0("//*[@id='__next']/div[1]/div[2]/div/div/div[2]/div[1]/div/div[2]/div[21]/div/div[1]/button[",i+2,"]/span"))
Sys.sleep(1)
remDr$executeScript("arguments[0].click();",alink)
Sys.sleep(1)
}

#nextlink <- remDr$findElements(using='xpath',value = "//*[@id='__next']/div[1]/div[2]/div/div/div[2]/div[1]/div/div[2]/div[21]/div/div[1]/button[7]")   
#Sys.sleep(1)
#remDr$executeScript("arguments[0].click();",nextlink)
Sys.sleep(1)
 
#if (as.numeric(endlink$getElementText) == 21){
 # break

}


#각각 _마케팅으로 저장 ! 
all_comname
View(data.frame(all_spec))
(act_spec)

write.csv(all_comname,"output/all_comname_삼성.csv")
write.csv(all_spec,"output/all_spec_삼성.csv")
write.csv(act_spec,"output/act_spec_삼성.csv")



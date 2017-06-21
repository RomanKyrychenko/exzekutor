library(RSelenium)
library(dplyr)
library(RJSONIO)
library(digest)
library(httr)
library(RCurl)
library(openssl)

source("https://raw.githubusercontent.com/RomanKyrychenko/exzekutor/master/persons")

fb_login <- function(){
  user <- remDr$findElement(using = "id", "email")
  user$sendKeysToElement(list("melnykeo94@gmail.com"))
  pass <- remDr$findElement(using = "id", value = "pass")
  pass$sendKeysToElement(list("usotrskvn1"))
  login <- remDr$findElement(using = "css selector", value = ".uiButton.uiButtonConfirm")
  login$clickElement()
}

#fb_login <- function(){
#  user <- remDr$findElement("xpath", '//*[@id="u_0_1"]/div[1]/div/input')
#  user$sendKeysToElement(list("melnykeo94@gmail.com"))
#  pass <- remDr$findElement("xpath",'//*[@id="u_0_2"]')
#  pass$sendKeysToElement(list("usotrskvn1",key="enter"))
#}


getAP <- function() {
  post <- remDr$findElements(using = "css selector", value = "._5pcq")
  unlist(lapply(post, function(x){x$getElementAttribute("href")}))[1]
}

get_content <- function(links) {
  
  wall <- data.frame()
  
  for (i in links) {
    
    remDr$navigate(i)
    
    post <- remDr$findElements(using = "css selector", value = "._5pbx")
    post <- unlist(lapply(post, function(x){x$getElementText()}))
    if(length(post)==0) {post = "NA"}
    
    link <- i
    
    foo <- data.frame(link, post,stringsAsFactors = F)
    wall <- rbind.data.frame(wall, foo)
    
  }
  
  #wall <- 
  unique.data.frame(wall)
  
  #return(wall)
  
  #remDr$close()
  
}

getFbName <- function() {
  name <- remDr$findElements(using = "css selector", value = "._2nlw")
  unlist(lapply(name, function(x){x$getElementText()}))
}

getFblink <- function() {
  name <- remDr$findElements(using = "css selector", value = "._8_2")
  unlist(lapply(name, function(x){x$getElement()}))
}

alerts_facebook <- function(fb_page){
  remDr$navigate(fb_page)
  name <- getFbName()
  #new_post <- getAllPosts(1)
  new_post <- getAP()
  c(name,stringr::str_sub(fb_page, start=26),new_post[1])
}

exezekutor <- function(lgd){
  do.call("rbind",lapply(lgd, alerts_facebook))
}

csv_to_json <- function(dat, pretty = F,na = "null",raw = "mongo",digits = 3,force = "unclass")
{
  dat_to_json <- jsonlite::toJSON(dat,pretty = pretty,na = "null",raw = raw,digits = digits ,force = force )
  dat_to_json=substr(dat_to_json,start = 2,nchar(dat_to_json)-1) 
  return(dat_to_json)
}

for (i in 1:100000){
  rD <- rsDriver(port = as.integer(round(runif(1, 1000,9999))),browser = "chrome")
  #rD <- rsDriver(port = as.integer(round(runif(1, 1000,9999))),browser = "phantomjs")
  remDr <- rD[["client"]]
  remDr$navigate("http://www.facebook.com")
  
  fb_login()
  
  jj <- exezekutor(persons)
  
  for (i in 1:500){
    tryCatch({jj <- inner_join(data.frame(jj,stringsAsFactors = FALSE),data.frame(exezekutor(persons),stringsAsFactors = FALSE),by=c("X1","X2"))
    b <- data.frame(link=jj[which((jj[length(jj)]==jj[length(jj)-1])==FALSE),length(jj)], username=jj[which((jj[length(jj)]==jj[length(jj)-1])==FALSE),1],stringsAsFactors = FALSE)
    b <- b %>% distinct(link,.keep_all = T)
    #f <- get_all_photos(b$link)
    f <- get_content(b$link)
    f <- f %>% distinct(link,.keep_all = T)
    if (nrow(f)>0){
      request <- data.frame(url=f$link,
                            username=gsub("\n", " ",b$username),
                            title=paste0(gsub("\n", " ",b$username),": ",substr(f$post,1,75)),
                            post=gsub("\n", " ",f$post),
                            dtpost=paste0(substr(as.character(Sys.time()),1,10),"T",substr(as.character(Sys.time()-round(runif(1, 0, 180),0)),12,20),"+0200"),
                            domain="facebook.com",
                            dt=paste0(substr(as.character(Sys.time()),1,10),"T",substr(as.character(Sys.time()),12,20),"+0200"),
                            stringsAsFactors = FALSE)
      #es = c("http://13.93.147.221:9200","http://13.64.64.38:9200","http://13.64.70.94:9200")
      es = "http://13.59.27.188:9200"
      url_id=unname(sapply(tolower(f$link), md5))
      index_name = paste0("urls_",gsub("-","",as.character(Sys.Date())))
      type_name = "news"
      #for (id in 1: nrow(request)) httpPUT(paste(sample(es,1),index_name,type_name,url_id[id],sep="/"), csv_to_json(request[id,]))
      for (id in 1: nrow(request)) httpPUT(paste(es,index_name,type_name,url_id[id],sep="/"), csv_to_json(request[id,]))
      print(paste("Add request",Sys.time()))
    }
    print(paste("worked",Sys.time()))
    lapply(c("p","act","datr","lu","fr","presence","csm","pl","sb"),remDr$deleteCookieNamed)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

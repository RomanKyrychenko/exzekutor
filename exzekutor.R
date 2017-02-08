library(RSelenium)
library(dplyr)
library(RJSONIO)

pusk <- function(){
  rD <- rsDriver(port = as.integer(round(runif(1, 1000,9999))))
  remDr <- rD[["client"]]
  remDr$navigate("http://www.facebook.com")
}


#melnykeo94@gmail.com
#usotrskvn1


alerts_facebook <- function(fb_page){
  remDr$navigate(fb_page)
  name <- getFbName()
  new_post <- getAllPosts(1)
  c(name,stringr::str_sub(fb_page, start=26),new_post[1])
}

exezekutor <- function(lgd){
  do.call("rbind",lapply(lgd, alerts_facebook))
}


jj <- exezekutor(persons)

pusk()

for (i in 1:5){
  jj <- inner_join(data.frame(jj,stringsAsFactors = FALSE),data.frame(exezekutor(persons),stringsAsFactors = FALSE),by=c("X1","X2"))
  
  b <- data.frame(link=jj[which((jj[length(jj)]==jj[length(jj)-1])==FALSE),length(jj)], username=jj[which((jj[length(jj)]==jj[length(jj)-1])==FALSE),1],stringsAsFactors = FALSE)
  
  f <- get_all_photos(b$link)
  exportJson <- apply(data.frame(b$username,f$link,f$post,stringsAsFactors = FALSE),1,toJSON)
  write(exportJson, paste0(as.character(Sys.Date()),"_",substr(as.character(Sys.time()),12,13),"-",substr(as.character(Sys.time()),15,16),"-",substr(as.character(Sys.time()),18,20),".json"))
}

,rep(Sys.time(),nrow(f))


library(httr)
library("rjson")

json_data <- fromJSON(file="2017-02-06_20-08-36.json")

r <- POST("http://192.168.100.232:9200/fb/post/", 
          body = exportJson,encode = "json")
stop_for_status(r)
content(r, "parsed", "application/json")

print(paste(b$username, "написав новий пост за лінком -",f$link, "Ось його текст:",  f$post))
exportJson <- toJSON(data.frame(b$username,f$link,f$post,stringsAsFactors = FALSE))
write(exportJson, paste0(as.character(Sys.Date()),"_",substr(as.character(Sys.time()),12,13),"-",substr(as.character(Sys.time()),15,16),"-",substr(as.character(Sys.time()),18,20),".json"))


library('elastic')
connect(es_port = 9200)

connect(es_host = <aws_es_endpoint>, es_path = "192.168.100.232", es_port = 9200, es_transport_schema  = "http")

connect(es_host = "192.168.100.232", es_path = "", es_port = 9200, es_transport_schema  = "http")
Search("fb")
Search(index="fb", size=1)$hits$hits
Search(index="fb", type="post", q="что", size=1)$hits$hits

docs_bulk(data.frame(b$username,f$link,f$post,stringsAsFactors = FALSE), index="fb", type="post")

f <- get_all_photos(b$link)
paste(b$username, "написав новий пост за лінком -",f$link, "Ось його текст:",  f$post)


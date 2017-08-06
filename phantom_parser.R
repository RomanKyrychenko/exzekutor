library(rvest)
library(qdapRegex)
library(RSelenium)
library(dplyr)
library(RJSONIO)
library(digest)
library(httr)
library(RCurl)
library(openssl)
library(purrr)
library(readr)
library(stringr)

fb_login <- function(){
  user <- remDr$findElement(using = "name", "email")
  user$sendKeysToElement(list("melnykeo94@gmail.com"))
  pass <- remDr$findElement(using = "name", value = "pass")
  pass$sendKeysToElement(list("usotrskvn1"))
  login <- remDr$findElement(using = "name", value = "login")
  login$clickElement()
}

csv_to_json <- function(dat, pretty = F,na = "null",raw = "mongo",digits = 3,force = "unclass")
{
  dat_to_json <- jsonlite::toJSON(dat,pretty = pretty,na = "null",raw = raw,digits = digits ,force = force )
  dat_to_json=substr(dat_to_json,start = 2,nchar(dat_to_json)-1) 
  return(dat_to_json)
}

alerts_facebook <- function(fb_page){
  remDr$navigate(fb_page)
  post <- read_html(remDr$getPageSource()[[1]]) %>% html_node(css = ".loadedSectionContent")
  url <- rm_between(post, 'top_level_post_id.', ':tl_objid.', extract=TRUE)[[1]][1]
  #pname <- substr(fb_page,24,nchar(fb_page))
  if(grepl("profile",fb_page)) {fb_page <- paste0("http://www.facebook.com/",substr(fb_page,39,nchar(fb_page)))}
  paste0(gsub("m.facebook.com","www.facebook.com",fb_page),"/posts/",url)
}

exezekutor <- function(lgd){
  unlist(lapply(lgd, alerts_facebook))
}

source("https://raw.githubusercontent.com/RomanKyrychenko/exzekutor/master/persons")
persons <- persons[-24]

get_post_info <- function(url){
  remDr$navigate(url)
  ps <-  read_html(remDr$getPageSource()[[1]])
  fullhtml <- ps %>% html_node("._5pbx.userContent") %>% html_text()
  if(is_empty(fullhtml)) fullhtml<-NA
  if(fullhtml=="" | is.na(fullhtml)) fullhtml <- ps %>% html_node(".mvl._52jv") %>% html_text()
  if(is.na(fullhtml)) fullhtml<-NA
  time <- ps %>% html_node("abbr._5ptz") %>% html_attr("data-utime") %>% as.numeric() %>% as.POSIXct(origin="1970-01-01")
  dtpost=paste0(gsub(" ","T",as.character(time)),"+0300")
  authors <- list(word(string = (ps %>% html_node("span.fwn.fcg") %>% html_text()), start = 1, end = 2, sep = fixed(" ")))
  title=paste0(unlist(authors),": ",substr(fullhtml,1,75))
  domain="facebook.com"
  dt=paste0(substr(as.character(Sys.time()),1,10),"T",substr(as.character(Sys.time()),12,20),"+0300")
  distinct(data_frame(url,authors,title,fullhtml,dtpost,domain,dt))
}

es = "http://13.59.27.188:9200"
type_name = "news"

for (i in 1:100000){
  rD <- rsDriver(port = as.integer(round(runif(1, 1000,9999))),browser = "phantomjs")
  remDr <- rD[["client"]]
  remDr$navigate("http://www.facebook.com")
  
  fb_login()
  Sys.sleep(3)
  remDr$navigate("http://www.facebook.com")
  jj <- exezekutor(gsub("www.facebook.com","m.facebook.com",persons))
  for (i in 1:500){
    tryCatch({
      jj <- rbind(jj,exezekutor(gsub("www.facebook.com","m.facebook.com",persons)))
      new=na.omit(jj[nrow(jj),(grepl("NA",jj[nrow(jj),]) | grepl("NA",jj[nrow(jj)-1,]))!=(jj[nrow(jj)-1,] != jj[nrow(jj),])])
      if (length(new)>0){
        for(i in 1:length(new)){
          request <- get_post_info(new[i])
          if(!is.na(request$fullhtml)){
            url_id=md5(tolower(request$url))
            index_name = paste0("urls_",gsub("-","",as.character(Sys.Date())))
            httpPUT(paste(es,index_name,type_name,url_id,sep="/"), csv_to_json(request))
            print(paste("Add request",Sys.time()))  
          }
        }
      }
      print(paste("worked",Sys.time()))
      lapply(c("p","act","datr","lu","fr","presence","csm","pl","sb"),remDr$deleteCookieNamed)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

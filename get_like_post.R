library(RSelenium)
library(dplyr)
library(RJSONIO)
library(httr)
library(RCurl)
library(rvest)

Sys.setlocale(,"UK_ua")

start_fb <- function(login,password){
  rD <<- rsDriver(port = as.integer(round(runif(1, 1000,9999))),browser = "chrome") #створюється вікно браузера
  remDr <<- rD[["client"]] #сворюється клієнт
  remDr$setWindowSize(1280L, 1024L)
  remDr$navigate("http://www.facebook.com")
  user <- remDr$findElement(using = "id", "email")
  user$sendKeysToElement(list(login))
  pass <- remDr$findElement(using = "id", value = "pass")
  pass$sendKeysToElement(list(password))
  login <- remDr$findElement(using = "css selector", value = ".uiButton.uiButtonConfirm")
  login$clickElement()
}

get_fb_id <- function(link){
  remDr$navigate(link)
  name <- remDr$findElements(using = "css selector", value ="._5h60")
  format(parse_number(unlist(lapply(name, function(x){x$getElementAttribute("data-gt")}))),scientific = F)
}

get_fb_post_links <- function(){
  name <- remDr$findElements(using = "css selector", value ="._5pcq")
  link <- unique(unlist(lapply(name, function(x){x$getElementAttribute("href")})))[-2]
  link[grepl("l.facebook.com",link)==F]
}

get_post_info <- function(post_link){
  remDr$navigate(post_link)
  ps <-  read_html(remDr$getPageSource()[[1]])
  is_page <- ps %>% html_node("._2yav") %>% html_text()
  is_page <- ifelse(is.na(is_page),0,1)
  #text
  #post <- ps %>% html_node("p") %>% html_text()
  post_id <- format(readr::parse_number(gsub("\\.","",gsub(":","",gsub("/","",post_link)))),scientific = F)
  if(nchar(post_id)<10){post_id <- format(readr::parse_number(gsub(".*/","",post_link)),scientific = F)}
  post <- ps %>% html_node("._5pbx.userContent") %>% html_text()
  if(is_empty(post)) post<-NA
  if(post=="" | is.na(post)) post <- ps %>% html_node(".mvl._52jv") %>% html_text()
  if(is.na(post)) post<-NA
  repost_text <- ps %>% html_node(".mtm._5pco") %>% html_text()
  repost_author <- ps %>% html_node("._1nb_.fwn.fcg") %>% html_text()
  repost_time <- (ps %>% html_nodes("abbr._5ptz") %>% html_attr("data-utime") %>% as.numeric() %>% as.POSIXct(origin="1970-01-01"))[2]
  #post-type
  type <- if(grepl("post",post_link)){"post"} else if(grepl("photo",post_link)) {"photo"} else{"video"}
  #time
  time <- ps %>% html_node("abbr._5ptz") %>% html_attr("data-utime") %>% as.numeric() %>% as.POSIXct(origin="1970-01-01")
  #location
  location <- ps %>% html_nodes("._5pcq") %>% html_text()
  location <- ifelse(grepl("pages",(ps %>% html_nodes("._5pcq") %>% html_attr("href"))[2]),location[2],NA)[1]
  #privacy
  privacy <- gsub("Опубликовано для: ","",(ps %>% html_node("._6a._29ee._4f-9._43_1") %>% html_attr("data-tooltip-content")))
  #username
  #user <- ps %>% html_node(".profileLink") %>% html_text()
  #if(is.na(user)) 
  user <- ps %>% html_node("span.fwn.fcg") %>% html_text()
  if(is_page==1){user <- ps %>% html_node("span.fwb.fcg") %>% html_text()}
  user <- stringr::word(string = user, start = 1, end = 2, sep = stringr::fixed(" "))
  #userlink
  #user_link <- ps %>% html_node(".profileLink") %>% html_attr("href")
  #if(is.na(user_link)) 
  user_link <- ps %>% html_node("._5pb8._8o._8s.lfloat._ohe") %>% html_attr("href")
  user_link <- gsub("?fref=nf","",user_link)
  user_link <- substr(user_link,1,nchar(user_link)-1)
  if(is_page==1){user_link <- substr(gsub(gsub(".*/","",user_link),"",user_link),1,nchar(gsub(gsub(".*/","",user_link),"",user_link)))}
  #photos
  photos <- ps %>% html_nodes("._5dec._xcx") %>% html_attr("href") %>% paste(collapse=", ")
  if(is_empty(photos)) photos<-NA
  if(photos=="") photos=NA
  #likes
  likes <- (ps %>% html_nodes("a._3emk") %>% html_attr("aria-label"))[1:3]
  like <- (if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Нрав",x))] %>% parse_number()})[1]
  if(is_empty(like) | is_empty(likes)) like=0
  super <- (if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Супе",x))] %>% parse_number()})[1]
  if(is_empty(super) | is_empty(likes)) super=0
  wow <- (if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Ух",x))] %>% parse_number()})[1]
  if(is_empty(wow) | is_empty(likes)) wow=0
  lol <- (if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Ха-ха",x))] %>% substr(5,10) %>%  parse_number()})[1]
  if(is_empty(lol) | is_empty(likes)) lol=0
  #comments
  comments <- ps %>% html_nodes("._4bl7") %>% html_text() 
  comments <- comments[2] %>% parse_number()
  if(is.na(comments)){comments <- 0}
  #shares
  shares <- ps %>% html_node(".UFIShareLink") %>% html_text() %>% parse_number()
  if(is.na(shares)){shares <- 0}
  #attr-text
  attr_text <- gsub(paste0(user," "),"",(ps %>% html_node("span.fcg") %>% html_text()))
  #link-in-post
  link_in_post<-ps %>% html_node("a._52c6") %>% html_attr("href")
  #image-link
  image_link <- ps %>% html_node(".scaledImageFitWidth.img") %>% html_attr("src")
  #app
  app_name <- ps %>% html_node("a._5pcq._20y0") %>% html_text()
  #app-link
  app_link <- ps %>% html_node("a._5pcq._20y0") %>% html_attr("href")
  is_repost <- ifelse(!is.na(repost_text),1,0)
  is_memories <- ifelse(repost_author==user,1,0)
  if(is.na(is_memories)) {is_memories <- 0}
  distinct(data_frame(post_id,user,user_link,post,post_link,type,time,location,privacy,photos,like,super,wow,lol,comments,shares,attr_text,link_in_post,
                      image_link,app_name,app_link,repost_text,repost_author,repost_time,is_repost,is_memories,is_page))
}

get_like_post <- function(userlink){
  fid <- get_fb_id(userlink)
  remDr$navigate(paste0("https://www.facebook.com/search/",fid,"/stories-liked/intersect"))
  i<-0
  while(i < 4000){
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
    i<-i+1
    print(i)
  }
  tsl <- get_fb_post_links()
  do.call("rbind",lapply(tsl,get_post_info))
}
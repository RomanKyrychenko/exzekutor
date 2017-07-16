get_post_info <- function(post_link){
  remDr$navigate(post_link)
  ps <-  html(remDr$getPageSource()[[1]])
  #text
  #post <- ps %>% html_node("p") %>% html_text()
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
  location <- location[2]
  #privacy
  privacy <- ps %>% html_node("._6a._29ee._4f-9._43_1") %>% html_attr("data-tooltip-content")
  #username
  #user <- ps %>% html_node(".profileLink") %>% html_text()
  #if(is.na(user)) 
  user <- ps %>% html_node("span.fwn.fcg") %>% html_text()
  #userlink
  #user_link <- ps %>% html_node(".profileLink") %>% html_attr("href")
  #if(is.na(user_link)) 
  user_link <- ps %>% html_node("._5pb8._8o._8s.lfloat._ohe") %>% html_attr("href")
  #photos
  photos <- ps %>% html_nodes("._5dec._xcx") %>% html_attr("href") %>% paste(collapse=", ")
  if(is_empty(photos)) photos<-NA
  if(photos=="") photos=NA
  #likes
  likes <- ps %>% html_nodes("a._3emk") %>% html_attr("aria-label")
  like <- if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Нрав",x))] %>% parse_number()}
  if(is_empty(like) | is_empty(likes)) like=0
  super <- if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Супе",x))] %>% parse_number()}
  if(is_empty(super) | is_empty(likes)) super=0
  wow <- if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Ух",x))] %>% parse_number()}
  if(is_empty(wow) | is_empty(likes)) wow=0
  lol <- if(!is_empty(likes)) {likes[sapply(likes,function(x)grepl("Ха-ха",x))] %>% substr(5,10) %>%  parse_number()}
  if(is_empty(lol) | is_empty(likes)) lol=0
  #comments
  comments <- ps %>% html_nodes("._4bl7") %>% html_text() 
  comments <- comments[2] %>% parse_number()
  #shares
  shares <- ps %>% html_node(".UFIShareLink") %>% html_text() %>% parse_number()
  #attr-text
  attr_text <- ps %>% html_node("span.fcg") %>% html_text()
  #link-in-post
  link_in_post<-ps %>% html_node("a._52c6") %>% html_attr("href")
  #image-link
  image_link <- ps %>% html_node(".scaledImageFitWidth.img") %>% html_attr("src")
  #app
  app_name <- ps %>% html_node("a._5pcq._20y0") %>% html_text()
  #app-link
  app_link <- ps %>% html_node("a._5pcq._20y0") %>% html_attr("href")
  distinct(data_frame(user,user_link,post,post_link,type,time,location,privacy,photos,like,super,wow,lol,comments,shares,attr_text,link_in_post,
             image_link,app_name,app_link,repost_text,repost_author,repost_time))
}

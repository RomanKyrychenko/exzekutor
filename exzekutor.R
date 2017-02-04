library(RSelenium)
library(dplyr)
rD <- rsDriver(port = as.integer(4715))
remDr <- rD[["client"]]
remDr$navigate("http://www.facebook.com")

lgd <- c("https://www.facebook.com/taras.berezovets",
"https://www.facebook.com/sonya.koshkina",
"https://www.facebook.com/Mustafanayyem",
"https://www.facebook.com/leshchenko.ukraine",
"https://www.facebook.com/mixailotkach",
"https://www.facebook.com/zoryan.zoryan",
"https://www.facebook.com/VikUkolov",
"https://www.facebook.com/oleksandr.palii",
"https://www.facebook.com/tchornovol",
"https://www.facebook.com/evgen.magda",
"https://www.facebook.com/dubinskyi",
"https://www.facebook.com/brtcomua",
"https://www.facebook.com/applecrysis",
"https://www.facebook.com/nusspavlo",
"https://www.facebook.com/arsen.avakov.1",
"https://www.facebook.com/anton.gerashchenko.7",
"https://www.facebook.com/den.kazansky",
"https://www.facebook.com/profile.php?id=100003767320921&fref=ts",
"https://www.facebook.com/alexey.arestovich",
"https://www.facebook.com/yuri.biriukov",
"https://www.facebook.com/dubilet",
"https://www.facebook.com/valerii.pekar",
"https://www.facebook.com/yuriy.romanenko",
"https://www.facebook.com/anatolijsharij",
"https://www.facebook.com/SaakashviliMikheil",
"https://www.facebook.com/alex.mochanov",
"https://www.facebook.com/profile.php?id=100000437913326&fref=ts",
"https://www.facebook.com/sevgil.musaieva",
"https://www.facebook.com/e.menendes",
"https://www.facebook.com/greg.dawis",
"https://www.facebook.com/ekaterina.roshuk")


alerts_facebook <- function(fb_page){
  remDr$navigate(fb_page)
  name <- getFbName()
  new_post <- getAllPosts(1)
  c(name,new_post[1])
}

txt <- get_all_posts(new_post[1],stringr::str_sub(fb_page, start=26))
txt
ifelse(is.null(grep("Порошенко", txt))==FALSE,"nema","Alarm")

alerts_facebook("https://www.facebook.com/taras.berezovets")

exezekutor <- function(lgd){
  z <- lapply(lgd, alerts_facebook)
  data.frame(name=gsub("\\/.*","",stringr::str_sub(z, start=26)),link=do.call("rbind", z))
}


feed <- exezekutor(lgd)

posts <- full_join(data.frame(name=gsub("\\/.*","",stringr::str_sub(z, start=26))),posts,by="name")
txt <- sapply(ifelse(posts[2]==posts[2],posts[2]," "), as.character)
f <- do.call("rbind",lapply(txt[,1],function(x){get_all_posts(x,gsub("\\/.*","",stringr::str_sub(x, start=26)))}))
write.csv(f, paste0(gsub(" ","",Sys.Date()),"posts.csv"))

bs <- getAllPosts(1)
bs_post <- get_all_posts(bs, "taras.berezovets")
https://www.facebook.com/monti.czardas?fref=ts

remDr$navigate("https://www.facebook.com/profile.php?id=100004775745586")
mil <- getAllPosts(1)


remDr$navigate("https://www.facebook.com/nusspavlo")
nuss <- rbind(nuss[1],getAllPosts(1))
nuss_post <- get_all_posts(nuss,"nusspavlo")


ifelse(nuss[3]==nuss[1],get_all_posts(nuss[1],"nusspavlo"),print("niichavo"))


startpage  = "https://www.facebook.com/taras.berezovets" #enter link to user page here

remDr$navigate(startpage) 


library(stringr)
str_sub("https://www.facebook.com/taras.berezovets", start=26)
# stop the selenium server
rD[["server"]]$stop() 

# if user forgets to stop server it will be garbage collected.
rD <- rsDriver()
rm(rD)
gc(rD)

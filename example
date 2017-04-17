library(RSelenium)
library(dplyr)
library(RJSONIO)
library(httr)
library(RCurl)

#старт клієнта
#на різних системах може бути по-різному, потрібна встановлена Java

rD <- rsDriver(port = as.integer(round(runif(1, 1000,9999))),browser = "chrome") #створюється вікно браузера
remDr <- rD[["client"]] #сворюється клієнт
remDr$navigate("http://www.facebook.com") #навігація на потрібну сторінку

getAP <- function() {
  post <- remDr$findElements(using = "css selector", value = "._5pcq") #знаходиться в коді потрібний клас
  unlist(lapply(post, function(x){x$getElementAttribute("href")}))[1] #береться вміст атрибуту "href" з виділеного вище класу
}

# якщо в коді вище в останній функції прописати post <- unlist(lapply(post, function(x){x$getElementText()})), 
# то витягненться текст, що міститься в класі

#деталі: https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html

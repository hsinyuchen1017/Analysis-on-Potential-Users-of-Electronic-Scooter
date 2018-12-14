pkgs <- c("jsonlite", "httr","gmt")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])] 
if(length(pkgs)) install.packages(pkgs)

library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
options(stringsAsFactors = F)

url <- "https://wapi.gogoro.com/tw/api/vm/list"
df <- fromJSON(content(GET(url), "text"))
dff<-as.data.frame(df$data)

dff$City1<-str_sub(dff$City,-22,-20)
tapply(dff$Id,dff$City1,length)

library(gmt)
dffKao <- filter(dff,
                 City1=="高雄市"
)
dffTpe <- filter(dff,
                 City1=="台北市"
)
dffTan <- filter(dff,
                 City1=="台南市"
)
dffTch <- filter(dff,
                 City1=="台中市"
)
dffTao <- filter(dff,
                 City1=="桃園市"
)
dffNtp <- filter(dff,
                 City1=="新北市"
)

resdis <- c()
for(i in 1:nrow(dffKao)){
  a <- dffKao$Latitude[i]
  b <- dffKao$Longitude[i]
  k <- c()
  for(j in 1:nrow(dffKao)){
    c <- dffKao$Latitude[j]
    d <- dffKao$Longitude[j]
    t <- geodist(a,b,c,d)
    k <- c(k,t)
  }
  k <- k[k!=0]
  k <- na.omit(k)
  resdis <- c(resdis,min(k))
}
resdis
KAO<-mean(resdis)

resdis1 <- c()
for(i in 1:nrow(dffTpe)){
  a <- dffTpe$Latitude[i]
  b <- dffTpe$Longitude[i]
  k <- c()
  for(j in 1:nrow(dffTpe)){
    c <- dffTpe$Latitude[j]
    d <- dffTpe$Longitude[j]
    t <- geodist(a,b,c,d)
    k <- c(k,t)
  }
  k <- k[k!=0]
  k <- na.omit(k)
  resdis1 <- c(resdis1,min(k))
}
resdis1
TPE<-mean(resdis1)

resdis2 <- c()
for(i in 1:nrow(dffNtp)){
  a <- dffNtp$Latitude[i]
  b <- dffNtp$Longitude[i]
  k <- c()
  for(j in 1:nrow(dffNtp)){
    c <- dffNtp$Latitude[j]
    d <- dffNtp$Longitude[j]
    t <- geodist(a,b,c,d)
    k <- c(k,t)
  }
  k <- k[k!=0]
  k <- na.omit(k)
  resdis2 <- c(resdis2,min(k))
}
resdis2
NTP<-mean(resdis2)

resdis3 <- c()
for(i in 1:nrow(dffTan)){
  a <- dffTan$Latitude[i]
  b <- dffTan$Longitude[i]
  k <- c()
  for(j in 1:nrow(dffTan)){
    c <- dffTan$Latitude[j]
    d <- dffTan$Longitude[j]
    t <- geodist(a,b,c,d)
    k <- c(k,t)
  }
  k <- k[k!=0]
  k <- na.omit(k)
  resdis3 <- c(resdis3,min(k))
}
resdis3
TAN<-mean(resdis3)

resdis4 <- c()
for(i in 1:nrow(dffTao)){
  a <- dffTao$Latitude[i]
  b <- dffTao$Longitude[i]
  k <- c()
  for(j in 1:nrow(dffTao)){
    c <- dffTao$Latitude[j]
    d <- dffTao$Longitude[j]
    t <- geodist(a,b,c,d)
    k <- c(k,t)
  }
  k <- k[k!=0]
  k <- na.omit(k)
  resdis4 <- c(resdis4,min(k))
}
resdis4
TAO<-mean(resdis4)

resdis5 <- c()
for(i in 1:nrow(dffTch)){
  a <- dffTch$Latitude[i]
  b <- dffTch$Longitude[i]
  k <- c()
  for(j in 1:nrow(dffTch)){
    c <- dffTch$Latitude[j]
    d <- dffTch$Longitude[j]
    t <- geodist(a,b,c,d)
    k <- c(k,t)
  }
  k <- k[k!=0]
  k <- na.omit(k)
  resdis5 <- c(resdis5,min(k))
}
resdis5
TCH<-mean(resdis5)

city<-c("台北","新北","桃園","台中","台南","高雄")
mindistance<-c(TPE,NTP,TAO,TCH,TAN,KAO)
distance<-as.data.frame(mindistance,city)

#畫圖
library(ggmap)
library(ggplot2)
map=get_googlemap(center= c(lon=121.5344265,lat=25.0703636)
                  ,zoom =12 , maptype = "roadmap")
ggmap(map) +geom_point(aes(x = dffTpe$Longitude, y = dffTpe$Latitude), size = 3, col="black",data = dffTpe, alpha = 1)


map=get_googlemap(center= c(lon=121.5517901,lat=25.1138355)
                  ,zoom =10 , maptype = "roadmap")
ggmap(map) +geom_point(aes(x = dffNtp$Longitude, y = dffNtp$Latitude), size = 1.5, col="black",data = dffNtp, alpha = 1)


map=get_googlemap(center= c(lon=121.2156053,lat=24.9196215)
                  ,zoom =11 , maptype = "roadmap")
ggmap(map) +geom_point(aes(x = dffTao$Longitude, y = dffTao$Latitude), size = 1.5, col="black",data = dffTao, alpha = 1)


map=get_googlemap(center= c(lon=120.6845212,lat=24.2085723)
                  ,zoom =11 , maptype = "roadmap")
ggmap(map) +geom_point(aes(x = dffTch$Longitude, y = dffTch$Latitude), size = 1.5, col="black",data = dffTch, alpha = 1)


map=get_googlemap(center= c(lon=120.2361417,lat=23.0782705)
                  ,zoom =10 , maptype = "roadmap")
ggmap(map) +geom_point(aes(x = dffTan$Longitude, y = dffTan$Latitude), size = 1.5, col="black",data = dffTan, alpha = 1)


map=get_googlemap(center= c(lon=120.3638934,lat=22.6567327)
                  ,zoom =10 , maptype = "roadmap")
ggmap(map) +geom_point(aes(x = dffKao$Longitude, y = dffKao$Latitude), size = 1.5, col="black",data = dffKao, alpha = 1)

#台北
resdis1
assignColor <- function(resdis1){
  if(resdis1 > 1){
    return("#FF4500") # red
  }
  else if(resdis1 < 0.5){
    return("#228B22") # green
  }
  else{
    return("#FFD700") #yellow 
  }
}


resdis1color <- sapply(resdis1, assignColor)

ggmap(
  get_googlemap(center=c(121.5344265,25.0703636),
                zoom=12,
                maptype='terrain')) +
  geom_point(data=dffTpe, 
             aes(x=dffTpe$Longitude, y=dffTpe$Latitude), 
             colour=resdis1color, 
             size=4, 
             alpha=0.8)


#雙北
df_list <- list(dffTpe,dffNtp)
dfft <- Reduce(function(x,y) merge(x,y,all=T),df_list)
dfft
resdis12 <- c()
for(i in 1:nrow(dfft)){
  a <- dfft$Latitude[i]
  b <- dfft$Longitude[i]
  k <- c()
  for(j in 1:nrow(dfft)){
    if(j!=i){
      c <- dfft$Latitude[j]
      d <- dfft$Longitude[j]
      t <- geodist(a,b,c,d)
      if(is.na(t)==TRUE){
        t <- 0
      }
      k <- c(k,t)
    }
  }
  resdis12 <- c(resdis12,min(k))
}
resdis12
bothtp<-mean(resdis12)

assignColor <- function(resdis12){
  if(resdis12 > 1){
    return("#FF4500") # red
  }
  else if(resdis12 < 0.5){
    return("#228B22") # green
  }
  else{
    return("#FFD700") #yellow 
  }
}

resdis12color <- sapply(resdis12, assignColor)

ggmap(
  get_googlemap(center=c(121.5344265,25.0703636),
                zoom=12,
                maptype='terrain')) +
  geom_point(data=dfft, 
             aes(x=dfft$Longitude, y=dfft$Latitude), 
             colour=resdis12color, 
             size=4, 
             alpha=0.8)

mean(resdis12)



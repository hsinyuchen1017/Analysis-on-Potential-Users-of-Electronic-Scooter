pkgs <- c("jsonlite", "httr")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])] 
if(length(pkgs)) install.packages(pkgs)

library(httr)
library(jsonlite)
options(stringsAsFactors = F)

url <- "https://wapi.gogoro.com/tw/api/vm/list"
df <- fromJSON(content(GET(url), "text"))
View(df$data$Latitude)

library(ggmap)
library(ggplot2)
map=get_googlemap(center= c(lon=120.7697297,lat=23.5857384)
                  ,zoom =7 , maptype = "hybrid")
ggmap(map) +geom_point(aes(x = df$data$Longitude, y = df$data$Latitude), size = 0.8, col="blue",data = df$data, alpha = 1)



map=get_googlemap(center= c(lon=120.742318,lat=23.7250039)
                  ,zoom =8 , maptype = "roadmap")
ggmap(map) +geom_point(aes(x = df$data$Longitude, y = df$data$Latitude), size = 0.8, col="black",data = df$data, alpha = 1)

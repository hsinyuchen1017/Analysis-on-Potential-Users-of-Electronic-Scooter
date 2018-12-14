pkgs <- c("readr", "httr", "RColorBrewer", "dplyr", "tidyr", "data.table")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])] 
if(length(pkgs)) install.packages(pkgs)
options(stringsAsFactors = F)
Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
url <- "https://quality.data.gov.tw/dq_download_csv.php?nid=6216&md5_url=6e5b67fc5701cb46d958be531f070980"
df <- read.csv(url, fileEncoding = "UTF-8")
df$E3<-as.numeric(df$E3)
df$F5<-as.numeric(df$F5)
df$F7<-as.numeric(df$F7)
df$D1<-as.numeric(df$D1)
df$E3[is.na(df$E3)] <- 0
df$F5[is.na(df$F5)] <- 0
df$F7[is.na(df$F7)] <- 0
df$D1[is.na(df$D1)] <- 0

E3F5<-table(df$E3,df1$df.F5)
E3F5<-E3F5[-1,]
View(E3F5)
barplot(E3F5,names.arg=c("no answer","employed","unemployed"),beside = TRUE,
      main="employment status",col=c("lightblue","darkblue"),legend=rownames(E3F5))


E3F7<-table(df$E3,df1$df.F7)
E3F7<-E3F7[-1,]
View(E3F7)
barplot(E3F7,beside = TRUE,main="Average monthly income per capita",col=c("red","pink"),legend=rownames(E3F7))

E3D1<-table(df$E3,df1$df.D1)
E3D1<-E3D1[-1,]
E3D1
barplot(E3D1,beside = TRUE,main="Average weekly fuel fee",col=c("lightgreen","darkgreen"),legend=rownames(E3D1))





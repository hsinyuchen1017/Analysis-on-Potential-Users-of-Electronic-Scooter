pkgs <- c("readr", "httr", "RColorBrewer", "dplyr", "tidyr", "data.table")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])] 
if(length(pkgs)) install.packages(pkgs)
options(stringsAsFactors = F)
Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
url <- "https://quality.data.gov.tw/dq_download_csv.php?nid=6216&md5_url=6e5b67fc5701cb46d958be531f070980"
df <- read.csv(url, fileEncoding = "UTF-8")
View(df)
str(df)
df$E3<-as.numeric(df$E3)
df$F5<-as.numeric(df$F5)
df$F7<-as.numeric(df$F7)
df$D1<-as.numeric(df$D1)
df$E3[is.na(df$E3)] <- 0
df$F5[is.na(df$F5)] <- 0
df$F7[is.na(df$F7)] <- 0
df$D1[is.na(df$D1)] <- 0

nonchangeF5<-as.data.frame(table(df$E3==1,df1$df.F5))
nonchangeF5<-subset(nonchangeF5,Var1==T, select=c(Var1:Freq))
names(nonchangeF5)[names(nonchangeF5)=="Var1"]="nonchange"
names(nonchangeF5)[names(nonchangeF5)=="Var2"]="employed or unemployed"
nonchangeF5$`employed or unemployed`<-as.numeric(as.character(nonchangeF5$`employed or unemployed`))
plot(nonchangeF5$Freq, main = "employed or unemployed of nonchange",
     ,xlab = "employed or unemployed",ylab = "frequency",ylim=c(40,4800))

changeF5<-as.data.frame(table(df$E3==2,df1$df.F5))
changeF5<-subset(changeF5,Var1==T, select=c(Var1:Freq))
names(changeF5)[names(changeF5)=="Var1"]="change"
names(changeF5)[names(changeF5)=="Var2"]="employed or unemployed"
changeF5$`employed or unemployed`<-as.numeric(as.character(changeF5$`employed or unemployed`))
plot(changeF5$Freq, main = "employed or unemployed of change",
     ,xlab = "employed or unemployed",ylab = "frequency",ylim=c(40,4800))

nonchangeF7<-as.data.frame(table(df$E3==1,df1$df.F7))
nonchangeF7<-subset(nonchangeF7,Var1==T, select=c(Var1:Freq))
names(nonchangeF7)[names(nonchangeF7)=="Var1"]="nonchange"
names(nonchangeF7)[names(nonchangeF7)=="Var2"]="incomelevel"
nonchangeF7$incomelevel<-as.numeric(as.character(nonchangeF7$incomelevel))
plot(nonchangeF7$Freq, main = "monthly incomelevel of nonchange",
     ,xlab = "income level",ylab = "frequency",xlim=c(0,10),ylim=c(100,1200))

changeF7<-as.data.frame(table(df$E3==2,df1$df.F7))
changeF7<-subset(changeF7,Var1==T, select=c(Var1:Freq))
names(changeF7)[names(changeF7)=="Var1"]="change"
names(changeF7)[names(changeF7)=="Var2"]="incomelevel"
changeF7$incomelevel<-as.numeric(as.character(changeF7$incomelevel))
plot(changeF7$Freq, main = "monthly incomelevel of change",
     ,xlab = "income level",ylab = "frequency",xlim=c(0,10),ylim=c(100,1200))

nonchangeD1<-as.data.frame(table(df$E3==1,df1$df.D1))
nonchangeD1<-subset(nonchangeD1,Var1==T, select=c(Var1:Freq))
names(nonchangeD1)[names(nonchangeD1)=="Var1"]="nonchange"
names(nonchangeD1)[names(nonchangeD1)=="Var2"]="fuelfee"
nonchangeD1$fuelfee<-as.numeric(as.character(nonchangeD1$fuelfee))
plot(nonchangeD1$Freq, main = "weekly fruelfee of nonchange",
     ,xlab = "fruelfee level",ylab = "frequency",xlim=c(0,12),ylim=c(100,1300))

changeD1<-as.data.frame(table(df$E3==2,df1$df.D1))
changeD1<-subset(changeD1,Var1==T, select=c(Var1:Freq))
names(changeD1)[names(changeD1)=="Var1"]="change"
names(changeD1)[names(changeD1)=="Var2"]="fuelfee"
changeD1$fuelfee<-as.numeric(as.character(changeD1$fuelfee))
plot(changeD1$Freq, main = "weekly fruelfee of change",
     ,xlab = "fruelfee level",ylab = "frequency",xlim=c(0,12),ylim=c(100,1300))


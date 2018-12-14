pkgs <- c("readr", "httr", "RColorBrewer", "dplyr", "tidyr")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])] 
if(length(pkgs)) install.packages(pkgs)
dfsix <- read.csv("~/Desktop/sixx.csv", stringsAsFactors = F)
library(ggplot2)
library(reshape2)

a <- dfsix$X
colnames(dfsix) <- c("X","Minimum Distance(km)","Amounts_of_Charging_Points")
dfsix$X <- c(1:6)
dfsix$Amounts_of_Charging_Points <- dfsix$Amounts_of_Charging_Points/100
dfsixm <- melt(dfsix,id=c("X"))
ggplot(dfsixm,aes(x=factor(X),y=value,fill=variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("steelblue1","palevioletred1"))+
  scale_y_continuous(sec.axis = sec_axis(~.*100))+
  scale_x_discrete(labels=a)+
  xlab("Country")+
  ylab("Value")+
  guides(fill=guide_legend(title = ""))
ggsave("mini.png",width = 7,height = 4)
?guide_legend

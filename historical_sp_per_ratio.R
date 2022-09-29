library(rvest)
library(dplyr)
col_link="https://www.multpl.com/s-p-500-pe-ratio/table/by-month"
col_page=read_html(col_link)
col_table=col_page %>% html_nodes("div#table")%>%
  html_table() %>% .[[1]]

names(col_table)<-c("month","PER")
col_table$PER<-gsub('estimate','',col_table$PER)
col_table$PER<-as.numeric(col_table$PER)

col_table$month<-as.Date(col_table$month,format="%b%d,%Y")
data<-col_table[order(as.Date(col_table$month,format="%b%d,%Y")),]
data<-slice(data,1:(n()-1))
data$month<-as.character(data$month)
data$month<-substr(data$month,1,nchar(data$month)-3)
library(xts)
data$month<-as.character(as.yearmon(data$month))

### longitudinal
per.ts<-ts(data$PER, start=c(1871,1),end=c(2022,9),freq=12)
target.ts4<-window(per.ts,start=c(1982,1),end=c(2022,9))
target.ts3<-window(per.ts,start=c(1992,1),end=c(2022,9))
target.ts2<-window(per.ts,start=c(2002,1),end=c(2022,9))
target.ts1<-window(per.ts,start=c(2012,1),end=c(2022,9))

### scatter plots
par(mfrow=c(2,2))
plot(target.ts4,xlab="Time",ylab="PER",bty="l")
plot(target.ts3,xlab="Time",ylab="PER",bty="l")
plot(target.ts2,xlab="Time",ylab="PER",bty="l")
plot(target.ts1,xlab="Time",ylab="PER",bty="l")

### minimum, 1st quartile, median, 3rd quartile, and maximum
data$PER[data$month=="Sep 2022"]
fivenum(data$PER[data$month>"Sep 1982"])
fivenum(data$PER[data$month>"Sep 1992"])
fivenum(data$PER[data$month>"Sep 2002"])
fivenum(data$PER[data$month>"Sep 2012"])




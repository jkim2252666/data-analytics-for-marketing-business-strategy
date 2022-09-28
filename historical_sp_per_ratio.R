library(rvest)
library(dplyr)
col_link="https://www.multpl.com/s-p-500-pe-ratio/table/by-month"
col_page=read_html(col_link)
col_table=col_page %>% html_nodes("div#table")%>%
  html_table() %>% .[[1]]
View(col_table)
names(col_table)<-c("month","PER")
col_table$PER<-gsub('estimate','',col_table$PER)
col_table$PER<-as.numeric(col_table$PER)
### longitudinal
## tomorrow
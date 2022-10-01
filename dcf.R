library(readxl)
sbux_2021 <- read_excel("C:\\Users\\Admin.WIN-FBCTH3KTTKS\\OneDrive - oakland.edu\\MGT_MKT_4900_6900\\data/sbux_2021.xlsx")
inflation<-0.02 # inflation rate
wacc<-0.07 # wieghted average of cost of capital
growth<-0.05 # growth rate after 5th year
colnames(sbux_2021)<-c("year","operating","investing")
oper<-lm(operating~year,sbux_2021) ### the relationship between year and cash flow from operating activities
ives<-lm(investing~year,sbux_2021) ### the relationship between year and cash flow from investing activities
future<-c(16,17,18,19,20) # 16 for 2021, 17 for 2022, 18 for 2023, 19 for 2024, 20 for 2025 for extrapolation 
f_year<-c(1,2,3,4,5,6) 
# 2021 for the first year in the future, 2022 for second, 2023 for third, 2024 for fourth, 2025 for fifth 
# for NPV calculation

a_oper<-data.frame(predict(oper,sbux_2021[sbux_2021$year==16,],interval="predict",level=0.95))
b_oper<-data.frame(predict(oper,sbux_2021[sbux_2021$year==17,],interval="predict",level=0.95))
c_oper<-data.frame(predict(oper,sbux_2021[sbux_2021$year==18,],interval="predict",level=0.95))
d_oper<-data.frame(predict(oper,sbux_2021[sbux_2021$year==19,],interval="predict",level=0.95))
e_oper<-data.frame(predict(oper,sbux_2021[sbux_2021$year==20,],interval="predict",level=0.95))
df_oper<-rbind(a_oper,b_oper,c_oper,d_oper,e_oper) # predicted value for operating cf

a_ives<-data.frame(predict(ives,sbux_2021[sbux_2021$year==16,],interval="predict",level=0.95))
b_ives<-data.frame(predict(ives,sbux_2021[sbux_2021$year==17,],interval="predict",level=0.95))
c_ives<-data.frame(predict(ives,sbux_2021[sbux_2021$year==18,],interval="predict",level=0.95))
d_ives<-data.frame(predict(ives,sbux_2021[sbux_2021$year==19,],interval="predict",level=0.95))
e_ives<-data.frame(predict(ives,sbux_2021[sbux_2021$year==20,],interval="predict",level=0.95))
df_ives<-rbind(a_ives,b_ives,c_ives,d_ives,e_ives) # predicted value for investment cf

fcf<-df_oper+df_ives ## predicted free cash flow from future 1 to future 5
annuity<-c(fcf[5,1]/(wacc-growth),fcf[5,2]/(wacc-growth),fcf[5,3]/(wacc-growth)) #present value for growing perpetuity. 
df<-rbind(fcf,annuity)
df<-cbind(df,f_year)
df$fit_real<-with(df,fit/(1+inflation)^f_year)
df$lwr_real<-with(df,lwr/(1+inflation)^f_year)
df$upr_real<-with(df,upr/(1+inflation)^f_year)

## discount future FCF to the present value: WACC = 0.06
#install.packages("FinancialMath")
library(FinancialMath)
NPV(0,df$fit_real,f_year,wacc)
NPV(0,df$lwr_real,f_year,wacc)
NPV(0,df$upr_real,f_year,wacc)


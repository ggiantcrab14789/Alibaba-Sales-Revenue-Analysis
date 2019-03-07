#-------------------- Import data file --------------------#
library(readxl)
DATA_threesources <- read_excel("C:/Users/Alicia/Downloads/DATA_threesources.xls")
attach(DATA_threesources)
DATA_threesources$Date <- as.Date(DATA_threesources$Date)
View(DATA_threesources)

#-------------------- Feature Engineering --------------------#
#sales revenue = (DV+SV+RV)*average price - (SV*fee to google+RV*fee to third party)
#SVFee = 1
#RVFee = 1
Sales_Revenue <- (DATA_threesources$'Direct Volumn'*35*0.07) + 
  (DATA_threesources$'Search Volumn'+DATA_threesources$'Referral Volumn')*28*0.07 #-
  #(DATA_threesources$'Search Volumn'*SVFee) -
  #(DATA_threesources$'Referral Volumn'*RVFee)
DATA_threesources <- cbind(DATA_threesources, Sales_Revenue)

#ad revenue = Search AD + Display AD (US dollars)
AD_Revenue <- DATA_threesources$'Search AD' + DATA_threesources$'Display AD'
DATA_threesources <- cbind(DATA_threesources, AD_Revenue)

#Membership_Revenue = Membership*3000
Membership_Revenue <- DATA_threesources$'Membership'*3000
DATA_threesources <- cbind(DATA_threesources, Membership_Revenue)


#Total Revenue = Sales Reveune + AD Revenue + Membership Revenue
Total_Revenue <- DATA_threesources$'Sales_Revenue' + DATA_threesources$'AD_Revenue' + DATA_threesources$'Membership_Revenue'
DATA_threesources <- cbind(DATA_threesources, Total_Revenue)



#-------------------- Correlation --------------------#
#Accumulated 
Cum._Seller_Community <- cumsum(DATA_threesources$'Seller Community')
DATA_threesources <- cbind(DATA_threesources, Cum._Seller_Community)

Cum._Membership <- cumsum(DATA_threesources$Membership)
DATA_threesources <- cbind(DATA_threesources, Cum._Membership)

#seperate the dataset by state
state0 <-subset(DATA_threesources,State=="0")
state1 <-subset(DATA_threesources,State=="1")

message("From ", state0$Date[1], " to ", state0$Date[nrow(state0)], " total ", nrow(state0), " days, the states are 0")
message("From ", state1$Date[1], " to ", state1$Date[nrow(state1)], " total ", nrow(state1), " days, the states are 1")

#plot all the correlation coefficient of variables
library(corrplot)

df0 <- cbind(state0[2:9],state0[11:14])
res_state0<-cor(df0)
res_state0<-res_state0[order(-res_state0[,12]),]
corrplot(res_state0, type = "upper", tl.col = "black", tl.srt = 45)

df1 <- cbind(state1[2:9],state1[11:14])
res_state1<-cor(df1)
res_state1<-res_state1[order(-res_state1[,12]),]
corrplot(res_state1, type = "upper", tl.col = "black", tl.srt = 45)

#-------------------- EDA of Alibaba Transaction --------------------#

library(ggplot2)
library(scales)
options(scipen = 999)
df <- data.frame(DATA_threesources$'Sales_Revenue', DATA_threesources$'AD_Revenue', DATA_threesources$'Membership_Revenue', DATA_threesources$'Total_Revenue')
ggplot(df, aes(Date, y = Value, fill = color)) +
  scale_fill_brewer()+
  geom_area(aes(x = as.Date(Date), y =`Total_Revenue`, fill = "Total_Revenue"))+
  geom_area(aes(x = as.Date(Date), y =`Membership_Revenue`, fill = "Membership_Revenue"))+
  geom_area(aes(x = as.Date(Date), y =`Sales_Revenue`, fill = "Sales_Revenue"))+
  geom_area(aes(x = as.Date(Date), y =`AD_Revenue`, fill = "AD_Revenue"))+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Daily Revenue")

#Find Low Revenue Period
low_Revenue<-subset(DATA_threesources,DATA_threesources$'Total_Revenue'<10000000)
message("From ", low_Revenue$'Date'[1], " to ", low_Revenue$'Date'[nrow(low_Revenue)], " the daily total revenue is lower than 10m")

#from this chart, we can see that from 2017-1-28 to 2017-02-08, 
#the totoal revenue is lower than 10000k/day
#This peorid matches the chinese new year(1/29~2/7)

message("Sales Revenue is ",round(sum(DATA_threesources$Sales_Revenue)/sum(DATA_threesources$Total_Revenue)*100,2),"% of Total Revenue")
message("Memership Revenue is ",round(sum(DATA_threesources$Membership_Revenue)/sum(DATA_threesources$Total_Revenue)*100,2),"% of Total Revenue")
message("AD Revenue is ",round(sum(DATA_threesources$AD_Revenue)/sum(DATA_threesources$Total_Revenue)*100,2),"% of Total Revenue")

dfpie <- data.frame(Revenue = c("Membership", "Sales", "AD")
                 ,value = c(sum(DATA_threesources$Membership_Revenue)/sum(DATA_threesources$Total_Revenue)*100,
                            sum(DATA_threesources$Sales_Revenue)/sum(DATA_threesources$Total_Revenue)*100,
                            sum(DATA_threesources$AD_Revenue)/sum(DATA_threesources$Total_Revenue)*100))

ggplot(dfpie, aes(x="", y=value, fill=Revenue)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+ 
  geom_text(aes(label = paste0(round(value), "%")), position = position_stack(vjust = 0.5), size=5)+
  scale_fill_brewer(palette="Blues")+
  labs(x = NULL, y = NULL, fill = NULL, title = "Revenue Pie Chart")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"))+theme(axis.text.x=element_blank())

df <- data.frame(DATA_threesources$'Direct Volumn', DATA_threesources$'Search Volumn', DATA_threesources$'Referral Volumn')

ggplot(df, aes(Date, y = Value, color = Variable)) + 
  ylim(0,2500000)+
  geom_hline(aes(yintercept= mean(DATA_threesources$'Direct Volumn'), col = "Direct Volumn", size=1)) +
  geom_hline(aes(yintercept= mean(DATA_threesources$'Search Volumn'), col = "Search Volumn", size=1)) +
  geom_hline(aes(yintercept= mean(DATA_threesources$'Referral Volumn'), col = "Referral Volumn", size=1)) +
  geom_point(aes(x = as.Date(Date), y =`Direct Volumn`, col = "Direct Volumn")) + 
  geom_point(aes(x = as.Date(Date), y =`Search Volumn`, col = "Search Volumn")) +
  geom_point(aes(x = as.Date(Date), y =`Referral Volumn`, col = "Referral Volumn")) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + Overall Average Line of Direct, Search and Referral Volumn")

ggplot(df, aes(Date, y = Value, color = Variable)) + 
  ylim(0,2500000)+
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Direct Volumn'), yend=mean(state0$'Direct Volumn'), col = "Direct Volumn", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Direct Volumn'), yend=mean(state1$'Direct Volumn'), col = "Direct Volumn", size=1)) +
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Search Volumn'), yend=mean(state0$'Search Volumn'), col = "Search Volumn", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Search Volumn'), yend=mean(state1$'Search Volumn'), col = "Search Volumn", size=1)) +
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Referral Volumn'), yend=mean(state0$'Referral Volumn'), col = "Referral Volumn", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Referral Volumn'), yend=mean(state1$'Referral Volumn'), col = "Referral Volumn", size=1)) +
  geom_point(aes(x = as.Date(Date), y =`Direct Volumn`, col = "Direct Volumn")) + 
  geom_point(aes(x = as.Date(Date), y =`Search Volumn`, col = "Search Volumn")) +
  geom_point(aes(x = as.Date(Date), y =`Referral Volumn`, col = "Referral Volumn")) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + State Average Line of Direct, Search and Referral Volumn")

#message("The average of Direct Volumn is ", round(mean(DATA_threesources$'Direct Volumn'),0))
#message("The average of Direct Volumn when State=0 is ", round(mean(state0$'Direct Volumn'),0))
#message("The average of Direct Volumn when State=1 is ", round(mean(state1$'Direct Volumn'),0))

ggplot(df, aes(Date, y = Value, color = Variable)) + 
  ylim(0,2500000)+
  geom_smooth(aes(x = as.Date(Date), y =`Direct Volumn`, col = "Direct Volumn"), se=FALSE, fullrange=TRUE)+
  geom_smooth(aes(x = as.Date(Date), y =`Search Volumn`, col = "Search Volumn"), se=FALSE, fullrange=TRUE)+
  geom_smooth(aes(x = as.Date(Date), y =`Referral Volumn`, col = "Referral Volumn"), se=FALSE, fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Trend Line of Direct, Search and Referral Volumn")

ggplot(df, aes(x=Date, y = Value, color = Variable)) + 
  ylim(0,2000000)+
  geom_smooth(aes(x = as.Date(Date), y =`Direct Volumn`, col = "Direct Volumn"), method=lm, fullrange=TRUE)+
  geom_smooth(aes(x = as.Date(Date), y =`Search Volumn`, col = "Search Volumn"), method=lm, fullrange=TRUE)+
  geom_smooth(aes(x = as.Date(Date), y =`Referral Volumn`, col = "Referral Volumn"), method=lm, fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Regression Line of Direct, Search and Referral Volumn")

#Scatter Plot of Search AD & Display AD
df2 <- data.frame(DATA_threesources$'Search AD', DATA_threesources$'Display AD')

ggplot(df, aes(Date, y = Value, color = Variable)) + 
  geom_hline(aes(yintercept= mean(DATA_threesources$'Search AD'), col = "Search AD", size=1)) +
  geom_hline(aes(yintercept= mean(DATA_threesources$'Display AD'), col = "Display AD", size=1)) +
  geom_point(aes(x = as.Date(Date), y =`Search AD`, col = "Search AD")) + 
  geom_point(aes(x = as.Date(Date), y =`Display AD`, col = "Display AD")) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + Average Line of Search AD & Display AD")

message("The average of Search AD is ", round(mean(DATA_threesources$'Search AD'),0))
message("The average of Search AD when State=0 is ", round(mean(state0$'Search AD'),0))
message("The average of Search AD when State=1 is ", round(mean(state1$'Search AD'),0))
message("The average of Display AD is ", round(mean(DATA_threesources$'Display AD'),0))
message("The average of Display AD when State=0 is ", round(mean(state0$'Display AD'),0))
message("The average of Display AD when State=1 is ", round(mean(state1$'Display AD'),0))

ggplot(df, aes(Date, y = Value, color = Variable)) + 
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Search AD'), yend=mean(state0$'Search AD'), col = "Search AD", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Search AD'), yend=mean(state1$'Search AD'), col = "Search AD", size=1)) +
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Display AD'), yend=mean(state0$'Display AD'), col = "Display AD", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Display AD'), yend=mean(state1$'Display AD'), col = "Display AD", size=1)) +
  geom_point(aes(x = as.Date(Date), y =`Search AD`, col = "Search AD")) + 
  geom_point(aes(x = as.Date(Date), y =`Display AD`, col = "Display AD")) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + State Average Line of Search AD & Display AD")

ggplot(df, aes(Date, y = Value, color = Variable)) + 
  geom_smooth(aes(x = as.Date(Date), y =`Search AD`, col = "Search AD"), se=FALSE, fullrange=TRUE)+
  geom_smooth(aes(x = as.Date(Date), y =`Display AD`, col = "Display AD"), se=FALSE, fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Trend Line of Search AD & Display AD")

ggplot(df, aes(x=Date, y = Value, color = Variable)) + 
  ylim(0,2000000)+
  geom_smooth(aes(x = as.Date(Date), y =`Search AD`, col = "Search AD"), method=lm, fullrange=TRUE)+
  geom_smooth(aes(x = as.Date(Date), y =`Display AD`, col = "Display AD"), method=lm, fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Regression Line of Search AD & Display AD")

ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_point(aes(x = as.Date(Date), y =`Seller Community`, col = "Seller Community")) + 
  geom_hline(aes(yintercept= mean(DATA_threesources$'Seller Community'), col = "Seller Community", size=1)) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + Average Line of Seller Community")

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_point(aes(x = as.Date(Date), y =`Seller Community`, col = "Seller Community")) + 
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Seller Community'), yend=mean(state0$'Seller Community'), col = "Seller Community", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Seller Community'), yend=mean(state1$'Seller Community'), col = "Seller Community", size=1)) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + State Average Line of Seller Community")

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_smooth(aes(x = as.Date(Date), y =`Seller Community`, col = "Seller Community"), fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Trend Line of Seller Community")

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_smooth(aes(x = as.Date(Date), y =`Seller Community`, col = "Seller Community"), method=lm, fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Regression Line of Seller Community")

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_point(aes(x = as.Date(Date), y =Cum._Seller_Community, col = "Cum._Seller_Community")) + 
  geom_line(aes(x=Date, y =Cum._Seller_Community, col = "Cum._Seller_Community"))+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Accumulated Seller Community")

message("The average of daily new seller is ", round(mean(DATA_threesources$'Seller Community'),0))
message("The average of daily new seller when State=0 is ", round(mean(state0$'Seller Community'),0))
message("The average of daily new seller when State=1 is ", round(mean(state1$'Seller Community'),0))

ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_point(aes(x = as.Date(Date), y =`Seller Community`, col = "Membership")) + 
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Membership'), yend=mean(state0$'Membership'), col = "Membership", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Membership'), yend=mean(state1$'Membership'), col = "Membership", size=1)) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + State Average Line of Membership")

ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_smooth(aes(x = as.Date(Date), y =Membership, col = "Membership"), fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Trend Line of Membership")

ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_smooth(aes(x = as.Date(Date), y =Membership, col = "Membership"), method=lm, fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Regression Line of Membership")

ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_point(aes(x = as.Date(Date), y =Cum._Membership, col = "Cum._Membership")) + 
  geom_line(aes(x=Date, y =Cum._Membership, col = "Cum._Membership"))+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Accumulated Membership")

message("The average of Membership is ", round(mean(DATA_threesources$'Membership'),0))
message("The average of Membership when State=0 is ", round(mean(state0$'Membership'),0))
message("The average of Membership when State=1 is ", round(mean(state1$'Membership'),0))

#Export dataframe
#install.packages("xlsx")
#library("xlsx")
#write.xlsx(DATA_threesources, file = "Alibaba.xlsx", sheetName = "sheet1", append = FALSE)

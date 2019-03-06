#-------------------- Import data file --------------------#
library(readxl)
DATA_threesources <- read_excel("C:/Users/Alicia/Downloads/DATA_threesources.xls")
attach(DATA_threesources)
DATA_threesources$Date <- as.Date(DATA_threesources$Date)
View(DATA_threesources)

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
res_state0<-cor(state0[2:9])
corrplot(res_state0, type = "upper", tl.col = "black", tl.srt = 45)

res_state1<-cor(state1[2:9])
corrplot(res_state1, type = "upper", tl.col = "black", tl.srt = 45)

#-------------------- EDA of Alibaba Transaction --------------------#

library(ggplot2)
library(scales)

#Scatter Plot of Direct, Search and Referral Volumn
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

options(scipen = 999)
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

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_point(aes(x = as.Date(Date), y =`Seller Community`, col = "Membership")) + 
  geom_segment(aes(x=as.Date("2010-10-01"), xend=as.Date("2011-02-28"), y=mean(state0$'Membership'), yend=mean(state0$'Membership'), col = "Membership", size=1)) +
  geom_segment(aes(x=as.Date("2011-03-01"), xend=as.Date("2011-09-23"), y=mean(state1$'Membership'), yend=mean(state1$'Membership'), col = "Membership", size=1)) +
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Scatter Plot + State Average Line of Membership")

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_smooth(aes(x = as.Date(Date), y =Membership, col = "Membership"), fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Trend Line of Membership")

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_smooth(aes(x = as.Date(Date), y =Membership, col = "Membership"), method=lm, fullrange=TRUE)+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Regression Line of Membership")

options(scipen = 999)
ggplot(data=DATA_threesources, aes(x=Date, y = Value)) + 
  geom_point(aes(x = as.Date(Date), y =Cum._Membership, col = "Cum._Membership")) + 
  geom_line(aes(x=Date, y =Cum._Membership, col = "Cum._Membership"))+
  scale_x_date(date_labels =  "%Y-%m-%d")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2011-03-01"))), colour="#BB0000", linetype="dashed")+
  labs(title="Accumulated Membership")

message("The average of Membership is ", round(mean(DATA_threesources$'Membership'),0))
message("The average of Membership when State=0 is ", round(mean(state0$'Membership'),0))
message("The average of Membership when State=1 is ", round(mean(state1$'Membership'),0))
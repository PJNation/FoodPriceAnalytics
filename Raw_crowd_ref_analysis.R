##Using Raw Data as curated originally in the crowdsourcing system
###Data prepped by checking for obviously spurious price submissions e.g. 0.1 - N10/kg or >=N10,000/kg
###Ref Data were mostly submitted from Retail markets (open market, City market etc) but initial analysis was conducted with all data.


library(tidyr)
library(dplyr)
library(MASS)
library(pastecs)
library(stats)
library(tseries)
library(forecast)
library(MASS)
library(quantmod)
library(lubridate)
library(ggplot2)


setwd("C:/Users/PJNat/Dropbox/Projects/WB_FTE_Consulting/Data/working_csv")

Raw_daily <- read.csv("Raw_groundref_FPCA_0km_clean.csv", header = TRUE)
Raw_daily$Date <- as.Date(Raw_daily$Date, format = "%m/%d/%Y")

Raw_daily <- Raw_daily %>% dplyr::select(-Market, -Market_type, -State, -Latitude, -Longitude,-Buying, -Buying_purpose) #Drop non-needed columns


Raw_daily <- pivot_longer(Raw_daily, cols = c(-Date, -Data_Source, -month), names_to = "commodity", values_to = "price")



##Datapoints trend

YMze_Raw_day <- Raw_daily %>%
  filter(commodity %in% "Ymaize")

YMze_Raw_day<- subset(YMze_Raw_day, !is.na(YMze_Raw_day$price))#Removing NAs.....

#Plotting the Datapoints for Daily Data

YMze_Raw_day2 <- YMze_Raw_day %>%
  group_by(Date,Data_Source) %>%
  summarize(Day_price = round (mean(price, na.rm = TRUE)))

ggplot(data = YMze_Raw_day2, aes(x = Date, y = Day_price, color = Data_Source)) +
  geom_point(size=2) +
   geom_smooth()+
  labs(
    x = "Date",
    y = "Price (N/Kg)",
    title = "Yellow Maize",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))



#Plotting the Datapoints for weekly Data

YMze_Raw_week <- YMze_Raw_day %>%
  group_by(week = lubridate::week(Date), Date = lubridate::floor_date(Date, "week"), Data_Source) %>%
  summarize(week_price = round(mean(price, na.rm = TRUE))) #For actual date of the week

ggplot(data = YMze_Raw_week, aes(x = week, y = week_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Week of the year",
    y = "Price (N/Kg)",
    title = "Yellow Maize",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))

#Plotting the Datapoints for monthly Data

YMze_Raw_month <- YMze_Raw_day %>%
  group_by(month=lubridate::month(Date),Data_Source) %>%
  summarize(month_price = round(mean(price, na.rm = TRUE)))

YMze_Raw_month <- YMze_Raw_day %>%
  group_by(month = lubridate::month(Date), Date = lubridate::floor_date(Date, "month"), Data_Source) %>%
  summarize(month_price = round(mean(price, na.rm = TRUE))) #For actual date of the month

ggplot(data = YMze_Raw_month, aes(x = month, y = month_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Month",
    y = "Price (N/Kg)",
    title = "Yellow Maize",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))


##White Maize - Data Points

WMze_Raw_day <- Raw_daily %>%
  filter(commodity %in% "Wmaize")

#Plotting the Datapoints for Daily Data

WMze_Raw_day2 <- WMze_Raw_day %>%
  group_by(Date,Data_Source) %>%
  summarize(Day_price = mean(price, na.rm = TRUE))

ggplot(data = WMze_Raw_day2, aes(x = Date, y = Day_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Date",
    y = "Price (N/Kg)",
    title = "White Maize",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))



#Plotting the Datapoints for weekly Data

WMze_Raw_week <- WMze_Raw_day %>%
  group_by(week=lubridate::week(Date),Data_Source) %>%
  summarize(week_price = mean(price, na.rm = TRUE))

ggplot(data = WMze_Raw_week, aes(x = week, y = week_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Week of the year",
    y = "Price (N/Kg)",
    title = "White Maize",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))

#Plotting the Datapoints for monthly Data

WMze_Raw_month <- WMze_Raw_day %>%
  group_by(month=lubridate::month(Date),Data_Source) %>%
  summarize(month_price = mean(price, na.rm = TRUE))

ggplot(data = WMze_Raw_month, aes(x = month, y = month_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Month",
    y = "Price (N/Kg)",
    title = "White Maize",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))


##Plotting the Datapoints for Thailand Rice

TRice_Raw_day <- Raw_daily %>%
  filter(commodity %in% "Thailand_Rice")

#Plotting the Datapoints for Daily Data

TRice_Raw_day2 <- TRice_Raw_day %>%
  group_by(Date,Data_Source) %>%
  summarize(Day_price = mean(price, na.rm = TRUE))

ggplot(data = TRice_Raw_day2, aes(x = Date, y = Day_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Date",
    y = "Price (N/Kg)",
    title = "Thailand Rice",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))



#Plotting the Datapoints for weekly Data

TRice_Raw_week <- TRice_Raw_day %>%
  group_by(week=lubridate::week(Date),Data_Source) %>%
  summarize(week_price = mean(price, na.rm = TRUE))

ggplot(data = TRice_Raw_week, aes(x = week, y = week_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Week of the year",
    y = "Price (N/Kg)",
    title = "Thailand Rice",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))

#Plotting the Datapoints for monthly Data

TRice_Raw_month <- TRice_Raw_day %>%
  group_by(month=lubridate::month(Date),Data_Source) %>%
  summarize(month_price = mean(price, na.rm = TRUE))

ggplot(data = TRice_Raw_month, aes(x = month, y = month_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Month",
    y = "Price (N/Kg)",
    title = "Thailand Rice",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))






##Plotting the Datapoints for Indian Rice

IRice_Raw_day <- Raw_daily %>%
  filter(commodity %in% "I_Rice")

#Plotting the Datapoints for Daily Data

IRice_Raw_day2 <- IRice_Raw_day %>%
  group_by(Date,Data_Source) %>%
  summarize(Day_price = mean(price, na.rm = TRUE))

ggplot(data = IRice_Raw_day2, aes(x = Date, y = Day_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Date",
    y = "Price (N/Kg)",
    title = "Indian Rice",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))



#Plotting the Datapoints for weekly Data

IRice_Raw_week <- IRice_Raw_day %>%
  group_by(week=lubridate::week(Date),Data_Source) %>%
  summarize(week_price = mean(price, na.rm = TRUE))

ggplot(data = IRice_Raw_week, aes(x = week, y = week_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Week of the year",
    y = "Price (N/Kg)",
    title = "Indian Rice",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))

#Plotting the Datapoints for monthly Data

IRice_Raw_month <- IRice_Raw_day %>%
  group_by(month=lubridate::month(Date),Data_Source) %>%
  summarize(month_price = mean(price, na.rm = TRUE))

ggplot(data = IRice_Raw_month, aes(x = month, y = month_price, color = Data_Source)) +
  geom_point(size=2) +
  geom_smooth()+
  labs(
    x = "Month",
    y = "Price (N/Kg)",
    title = "Indian Rice",
    color = ""
  ) +
  theme_minimal()+
  theme(text = element_text(size = 22))



##Relationships


Raw_daily_avg <- Raw_daily %>%
  group_by(Date, commodity, Data_Source) %>%
  summarize(Daily_price = mean(price, na.rm = TRUE)) #Daily averages per commodity

##White Maize - Daily
WMze_raw_daily <- as.data.frame(subset(Raw_daily_avg, subset=Raw_daily_avg$commodity=="Wmaize"))

WMze_raw_daily_ref<-as.data.frame(subset(WMze_raw_daily, subset= WMze_raw_daily$Data_Source=="Enumerator"))
WMze_raw_daily_crd<- as.data.frame(subset(WMze_raw_daily, subset= WMze_raw_daily$Data_Source=="Crowd")) 

WMze_raw_daily_ref_crd <- full_join(WMze_raw_daily_ref, WMze_raw_daily_crd, by = "Date")

ggplot(WMze_raw_daily_ref_crd, aes(x = Daily_price.y, y = Daily_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Daily Crowdsourced Price",
    y = "Daily Enumerator Price",
    title = "White Maize"
  )+
  theme(text = element_text(size = 22))

summary(lm(Daily_price.x ~ Daily_price.y, data = WMze_raw_daily_ref_crd))
cor.test(WMze_raw_daily_ref_crd$Daily_price.x, WMze_raw_daily_ref_crd$Daily_price.y, method = "pearson")
stat.desc(WMze_raw_daily_ref_crd$Daily_price.y)
t.test(WMze_raw_daily_ref_crd$Daily_price.x, WMze_raw_daily_ref_crd$Daily_price.y, paired = TRUE, alternative = "two.sided")
var.test(WMze_raw_daily_ref_crd$Daily_price.x, WMze_raw_daily_ref_crd$Daily_price.y)


#Weekly
Raw_weekly_avg <- Raw_daily %>%
  group_by(week = lubridate::week(Date), commodity, Data_Source) %>%
  summarize(Weekly_price = mean(price, na.rm = TRUE)) #Daily averages per commodity

##White Maize - Weekly
WMze_raw_weekly <- as.data.frame(subset(Raw_weekly_avg, subset=Raw_weekly_avg$commodity=="Wmaize"))

WMze_raw_weekly_ref<-as.data.frame(subset(WMze_raw_weekly, subset= WMze_raw_weekly$Data_Source=="Enumerator"))
WMze_raw_weekly_crd<- as.data.frame(subset(WMze_raw_weekly, subset= WMze_raw_weekly$Data_Source=="Crowd")) 

WMze_raw_weekly_ref_crd <- full_join(WMze_raw_weekly_ref, WMze_raw_weekly_crd, by = "week")

ggplot(WMze_raw_weekly_ref_crd, aes(x = Weekly_price.y, y = Weekly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Weekly Crowdsourced Price",
    y = "Weekly Enumerator Price",
    title = "White Maize"
  )+
  theme(text = element_text(size = 22))

summary(lm(Weekly_price.x ~ Weekly_price.y, data = WMze_raw_weekly_ref_crd))
cor.test(WMze_raw_weekly_ref_crd$Weekly_price.x, WMze_raw_weekly_ref_crd$Weekly_price.y, method = "pearson")
stat.desc(WMze_raw_weekly_ref_crd$Weekly_price.x)
stat.desc(WMze_raw_weekly_ref_crd$Weekly_price.y)
t.test(WMze_raw_weekly_ref_crd$Weekly_price.x, WMze_raw_weekly_ref_crd$Weekly_price.y, paired = TRUE, alternative = "two.sided")
var.test(WMze_raw_weekly_ref_crd$Weekly_price.x, WMze_raw_weekly_ref_crd$Weekly_price.y)



#Monthly

Raw_monthly_avg <- Raw_daily %>%
  group_by(month, commodity, Data_Source) %>%
  summarize(monthly_price = mean(price, na.rm = TRUE)) #monthly averages per commodity

##White Maize - Monthly
WMze_raw_monthly <- as.data.frame(subset(Raw_monthly_avg, subset=Raw_monthly_avg$commodity=="Wmaize"))

WMze_raw_monthly_ref<-as.data.frame(subset(WMze_raw_monthly, subset= WMze_raw_monthly$Data_Source=="Enumerator"))
WMze_raw_monthly_crd<- as.data.frame(subset(WMze_raw_monthly, subset= WMze_raw_monthly$Data_Source=="Crowd")) 

WMze_raw_monthly_ref_crd <- full_join(WMze_raw_monthly_ref, WMze_raw_monthly_crd, by = "month")

ggplot(WMze_raw_monthly_ref_crd, aes(x = monthly_price.y, y = monthly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Monthly Crowdsourced Price",
    y = "Monthly Enumerator Price",
    title = "White Maize"
  )+
  theme(text = element_text(size = 22))

summary(lm(monthly_price.x ~ monthly_price.y, data = WMze_raw_monthly_ref_crd))

cor.test(WMze_raw_monthly_ref_crd$monthly_price.x, WMze_raw_monthly_ref_crd$monthly_price.y, method = "pearson")
stat.desc(WMze_raw_monthly_ref_crd$monthly_price.x)
stat.desc(WMze_raw_monthly_ref_crd$monthly_price.y)
t.test(WMze_raw_monthly_ref_crd$monthly_price.x, WMze_raw_monthly_ref_crd$monthly_price.y, paired = TRUE, alternative = "two.sided")
var.test(WMze_raw_monthly_ref_crd$monthly_price.x, WMze_raw_monthly_ref_crd$monthly_price.y)




##Yellow Maize - Daily

YMze_raw_daily <- as.data.frame(subset(Raw_daily_avg, subset=Raw_daily_avg$commodity=="Ymaize"))

YMze_raw_daily_ref<-as.data.frame(subset(YMze_raw_daily, subset= YMze_raw_daily$Data_Source=="Enumerator"))
YMze_raw_daily_crd<- as.data.frame(subset(YMze_raw_daily, subset= YMze_raw_daily$Data_Source=="Crowd")) 

YMze_raw_daily_ref_crd <- full_join(YMze_raw_daily_ref, YMze_raw_daily_crd, by = "Date")

ggplot(YMze_raw_daily_ref_crd, aes(x = Daily_price.y, y = Daily_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Daily Crowdsourced Price",
    y = "Daily Enumerator Price",
    title = "Yellow Maize"
  )+
  theme(text = element_text(size = 22))

summary(lm(Daily_price.x ~ Daily_price.y, data = YMze_raw_daily_ref_crd))
cor.test(YMze_raw_daily_ref_crd$Daily_price.x, YMze_raw_daily_ref_crd$Daily_price.y, method = "pearson")
stat.desc(YMze_raw_daily_ref_crd$Daily_price.x)
stat.desc(YMze_raw_daily_ref_crd$Daily_price.y)
t.test(YMze_raw_daily_ref_crd$Daily_price.x, YMze_raw_daily_ref_crd$Daily_price.y, paired = TRUE, alternative = "two.sided")
var.test(YMze_raw_daily_ref_crd$Daily_price.x, YMze_raw_daily_ref_crd$Daily_price.y)


##Yellow Maize - Weekly

YMze_raw_weekly <- as.data.frame(subset(Raw_weekly_avg, subset=Raw_weekly_avg$commodity=="Ymaize"))

YMze_raw_weekly_ref<-as.data.frame(subset(YMze_raw_weekly, subset= YMze_raw_weekly$Data_Source=="Enumerator"))
YMze_raw_weekly_crd<- as.data.frame(subset(YMze_raw_weekly, subset= YMze_raw_weekly$Data_Source=="Crowd")) 

YMze_raw_weekly_ref_crd <- full_join(YMze_raw_weekly_ref, YMze_raw_weekly_crd, by = "week")

ggplot(YMze_raw_weekly_ref_crd, aes(x = Weekly_price.y, y = Weekly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Weekly Crowdsourced Price",
    y = "Weekly Enumerator Price",
    title = "Yellow Maize"
  )+
  theme(text = element_text(size = 22))

summary(lm(Weekly_price.x ~ Weekly_price.y, data = YMze_raw_weekly_ref_crd))
cor.test(YMze_raw_weekly_ref_crd$Weekly_price.x, YMze_raw_weekly_ref_crd$Weekly_price.y, method = "pearson")
stat.desc(YMze_raw_weekly_ref_crd$Weekly_price.x)
stat.desc(YMze_raw_weekly_ref_crd$Weekly_price.y)
t.test(YMze_raw_weekly_ref_crd$Weekly_price.x, YMze_raw_weekly_ref_crd$Weekly_price.y, paired = TRUE, alternative = "two.sided")
var.test(YMze_raw_weekly_ref_crd$Weekly_price.x, YMze_raw_weekly_ref_crd$Weekly_price.y)

##Yellow Maize Monthly

YMze_raw_monthly <- as.data.frame(subset(Raw_monthly_avg, subset=Raw_monthly_avg$commodity=="Ymaize"))

YMze_raw_monthly_ref<-as.data.frame(subset(YMze_raw_monthly, subset= YMze_raw_monthly$Data_Source=="Enumerator"))
YMze_raw_monthly_crd<- as.data.frame(subset(YMze_raw_monthly, subset= YMze_raw_monthly$Data_Source=="Crowd")) 

YMze_raw_monthly_ref_crd <- full_join(YMze_raw_monthly_ref, YMze_raw_monthly_crd, by = "month")

ggplot(YMze_raw_monthly_ref_crd, aes(x = monthly_price.y, y = monthly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Monthly Crowdsourced Price",
    y = "Monthly Enumerator Price",
    title = "Yellow Maize"
  )+
  theme(text = element_text(size = 22))

summary(lm(monthly_price.x ~ monthly_price.y, data = YMze_raw_monthly_ref_crd))

cor.test(YMze_raw_monthly_ref_crd$monthly_price.x, YMze_raw_monthly_ref_crd$monthly_price.y, method = "pearson")
stat.desc(YMze_raw_monthly_ref_crd$monthly_price.x)
stat.desc(YMze_raw_monthly_ref_crd$monthly_price.y)
t.test(YMze_raw_monthly_ref_crd$monthly_price.x, YMze_raw_monthly_ref_crd$monthly_price.y, paired = TRUE, alternative = "two.sided")
var.test(YMze_raw_monthly_ref_crd$monthly_price.x, YMze_raw_monthly_ref_crd$monthly_price.y)







#Thailand Rice Daily

TRice_raw_daily <- as.data.frame(subset(Raw_daily_avg, subset=Raw_daily_avg$commodity=="Thailand_Rice"))

TRice_raw_daily_ref<-as.data.frame(subset(TRice_raw_daily, subset= TRice_raw_daily$Data_Source=="Enumerator"))
TRice_raw_daily_crd<- as.data.frame(subset(TRice_raw_daily, subset= TRice_raw_daily$Data_Source=="Crowd")) 

TRice_raw_daily_ref_crd <- full_join(TRice_raw_daily_ref, TRice_raw_daily_crd, by = "Date")

ggplot(TRice_raw_daily_ref_crd, aes(x = Daily_price.y, y = Daily_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Daily Crowdsourced Price",
    y = "Daily Enumerator Price",
    title = "Thailand Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(Daily_price.x ~ Daily_price.y, data = TRice_raw_daily_ref_crd))
cor.test(TRice_raw_daily_ref_crd$Daily_price.x, TRice_raw_daily_ref_crd$Daily_price.y, method = "pearson")
stat.desc(TRice_raw_daily_ref_crd$Daily_price.x)
stat.desc(TRice_raw_daily_ref_crd$Daily_price.y)
t.test(TRice_raw_daily_ref_crd$Daily_price.x, TRice_raw_daily_ref_crd$Daily_price.y, paired = TRUE, alternative = "two.sided")
var.test(TRice_raw_daily_ref_crd$Daily_price.x, TRice_raw_daily_ref_crd$Daily_price.y)


##Thailand Rice - Weekly

TRice_raw_weekly <- as.data.frame(subset(Raw_weekly_avg, subset=Raw_weekly_avg$commodity=="Thailand_Rice"))

TRice_raw_weekly_ref<-as.data.frame(subset(TRice_raw_weekly, subset= TRice_raw_weekly$Data_Source=="Enumerator"))
TRice_raw_weekly_crd<- as.data.frame(subset(TRice_raw_weekly, subset= TRice_raw_weekly$Data_Source=="Crowd")) 

TRice_raw_weekly_ref_crd <- full_join(TRice_raw_weekly_ref, TRice_raw_weekly_crd, by = "week")

ggplot(TRice_raw_weekly_ref_crd, aes(x = Weekly_price.y, y = Weekly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Weekly Crowdsourced Price",
    y = "Weekly Enumerator Price",
    title = "Thailand Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(Weekly_price.x ~ Weekly_price.y, data = TRice_raw_weekly_ref_crd))
cor.test(TRice_raw_weekly_ref_crd$Weekly_price.x, TRice_raw_weekly_ref_crd$Weekly_price.y, method = "pearson")
stat.desc(TRice_raw_weekly_ref_crd$Weekly_price.x)
stat.desc(TRice_raw_weekly_ref_crd$Weekly_price.y)
t.test(TRice_raw_weekly_ref_crd$Weekly_price.x, TRice_raw_weekly_ref_crd$Weekly_price.y, paired = TRUE, alternative = "two.sided")
var.test(TRice_raw_weekly_ref_crd$Weekly_price.x, TRice_raw_weekly_ref_crd$Weekly_price.y)



##Thailand - Monthly

TRice_raw_monthly <- as.data.frame(subset(Raw_monthly_avg, subset=Raw_monthly_avg$commodity=="Thailand_Rice"))

TRice_raw_monthly_ref<-as.data.frame(subset(TRice_raw_monthly, subset= TRice_raw_monthly$Data_Source=="Enumerator"))
TRice_raw_monthly_crd<- as.data.frame(subset(TRice_raw_monthly, subset= TRice_raw_monthly$Data_Source=="Crowd")) 

TRice_raw_monthly_ref_crd <- full_join(TRice_raw_monthly_ref, TRice_raw_monthly_crd, by = "month")

ggplot(TRice_raw_monthly_ref_crd, aes(x = monthly_price.y, y = monthly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Monthly Crowdsourced Price",
    y = "Monthly Enumerator Price",
    title = "Thailand Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(monthly_price.x ~ monthly_price.y, data = TRice_raw_monthly_ref_crd))

cor.test(TRice_raw_monthly_ref_crd$monthly_price.x, TRice_raw_monthly_ref_crd$monthly_price.y, method = "pearson")
stat.desc(TRice_raw_monthly_ref_crd$monthly_price.x)
stat.desc(TRice_raw_monthly_ref_crd$monthly_price.y)
t.test(TRice_raw_monthly_ref_crd$monthly_price.x, TRice_raw_monthly_ref_crd$monthly_price.y, paired = TRUE, alternative = "two.sided")
var.test(TRice_raw_monthly_ref_crd$monthly_price.x, TRice_raw_monthly_ref_crd$monthly_price.y)





#Indian Rice Daily

IRice_raw_daily <- as.data.frame(subset(Raw_daily_avg, subset=Raw_daily_avg$commodity=="I_Rice"))

IRice_raw_daily_ref<-as.data.frame(subset(IRice_raw_daily, subset= IRice_raw_daily$Data_Source=="Enumerator"))
IRice_raw_daily_crd<- as.data.frame(subset(IRice_raw_daily, subset= IRice_raw_daily$Data_Source=="Crowd")) 

IRice_raw_daily_ref_crd <- full_join(IRice_raw_daily_ref, IRice_raw_daily_crd, by = "Date")

ggplot(IRice_raw_daily_ref_crd, aes(x = Daily_price.y, y = Daily_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Daily Crowdsourced Price",
    y = "Daily Enumerator Price",
    title = "Indian Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(Daily_price.x ~ Daily_price.y, data = IRice_raw_daily_ref_crd))
cor.test(IRice_raw_daily_ref_crd$Daily_price.x, IRice_raw_daily_ref_crd$Daily_price.y, method = "pearson")
stat.desc(IRice_raw_daily_ref_crd$Daily_price.x)
stat.desc(IRice_raw_daily_ref_crd$Daily_price.y)
t.test(IRice_raw_daily_ref_crd$Daily_price.x, IRice_raw_daily_ref_crd$Daily_price.y, paired = TRUE, alternative = "two.sided")
var.test(IRice_raw_daily_ref_crd$Daily_price.x, IRice_raw_daily_ref_crd$Daily_price.y)


##Indian Rice - Weekly

IRice_raw_weekly <- as.data.frame(subset(Raw_weekly_avg, subset=Raw_weekly_avg$commodity=="I_Rice"))

IRice_raw_weekly_ref<-as.data.frame(subset(IRice_raw_weekly, subset= IRice_raw_weekly$Data_Source=="Enumerator"))
IRice_raw_weekly_crd<- as.data.frame(subset(IRice_raw_weekly, subset= IRice_raw_weekly$Data_Source=="Crowd")) 

IRice_raw_weekly_ref_crd <- full_join(IRice_raw_weekly_ref, IRice_raw_weekly_crd, by = "week")

ggplot(IRice_raw_weekly_ref_crd, aes(x = Weekly_price.y, y = Weekly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Weekly Crowdsourced Price",
    y = "Weekly Enumerator Price",
    title = "Indian Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(Weekly_price.x ~ Weekly_price.y, data = IRice_raw_weekly_ref_crd))
cor.test(IRice_raw_weekly_ref_crd$Weekly_price.x, IRice_raw_weekly_ref_crd$Weekly_price.y, method = "pearson")
stat.desc(IRice_raw_weekly_ref_crd$Weekly_price.x)
stat.desc(IRice_raw_weekly_ref_crd$Weekly_price.y)
t.test(IRice_raw_weekly_ref_crd$Weekly_price.x, IRice_raw_weekly_ref_crd$Weekly_price.y, paired = TRUE, alternative = "two.sided")
var.test(IRice_raw_weekly_ref_crd$Weekly_price.x, IRice_raw_weekly_ref_crd$Weekly_price.y)



##Indian Rice - Monthly

IRice_raw_monthly <- as.data.frame(subset(Raw_monthly_avg, subset=Raw_monthly_avg$commodity=="I_Rice"))

IRice_raw_monthly_ref<-as.data.frame(subset(IRice_raw_monthly, subset= IRice_raw_monthly$Data_Source=="Enumerator"))
IRice_raw_monthly_crd<- as.data.frame(subset(IRice_raw_monthly, subset= IRice_raw_monthly$Data_Source=="Crowd")) 

IRice_raw_monthly_ref_crd <- full_join(IRice_raw_monthly_ref, IRice_raw_monthly_crd, by = "month")

ggplot(IRice_raw_monthly_ref_crd, aes(x = monthly_price.y, y = monthly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Monthly Crowdsourced Price",
    y = "Monthly Enumerator Price",
    title = "Indian Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(monthly_price.x ~ monthly_price.y, data = IRice_raw_monthly_ref_crd))

cor.test(IRice_raw_monthly_ref_crd$monthly_price.x, IRice_raw_monthly_ref_crd$monthly_price.y, method = "pearson")
stat.desc(IRice_raw_monthly_ref_crd$monthly_price.x)
stat.desc(IRice_raw_monthly_ref_crd$monthly_price.y)
t.test(IRice_raw_monthly_ref_crd$monthly_price.x, IRice_raw_monthly_ref_crd$monthly_price.y, paired = TRUE, alternative = "two.sided")
var.test(IRice_raw_monthly_ref_crd$monthly_price.x, IRice_raw_monthly_ref_crd$monthly_price.y)


##------Counting the number of daily datapoints-----
test1<-subset(Raw_daily, Raw_daily$Data_Source=="Crowd" & Raw_daily$commodity=="Ymaize")
nrow(test1[!is.na(test1$price),])
test2<-subset(Raw_daily, Raw_daily$Data_Source=="Crowd" & Raw_daily$commodity=="Wmaize")
nrow(test2[!is.na(test2$price),])
test3<-subset(Raw_daily, Raw_daily$Data_Source=="Crowd" & Raw_daily$commodity=="Thailand_Rice")
nrow(test3[!is.na(test3$price),])
test4<-subset(Raw_daily, Raw_daily$Data_Source=="Crowd" & Raw_daily$commodity=="I_Rice")
nrow(test4[!is.na(test4$price),])





























































##Thailand - Daily

TRice_raw_daily <- as.data.frame(subset(Raw_daily_avg, subset=Raw_daily_avg$commodity=="Thailand_Rice"))

TRice_raw_daily_ref<-as.data.frame(subset(TRice_raw_daily, subset= TRice_raw_daily$Data_Source=="Enumerator"))
TRice_raw_daily_crd<- as.data.frame(subset(TRice_raw_daily, subset= TRice_raw_daily$Data_Source=="Crowd")) 

TRice_raw_daily_ref_crd <- full_join(TRice_raw_daily_ref, TRice_raw_daily_crd, by = "Date")

ggplot(TRice_raw_daily_ref_crd, aes(x = Daily_price.y, y = Daily_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Daily Crowdsourced Price",
    y = "Daily Enumerator Price",
    title = "Thailand Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(Daily_price.x ~ Daily_price.y, data = TRice_raw_daily_ref_crd))


##Thailand Rice - Weekly

TRice_raw_weekly <- as.data.frame(subset(Raw_weekly_avg, subset=Raw_weekly_avg$commodity=="Ymaize"))

TRice_raw_weekly_ref<-as.data.frame(subset(TRice_raw_weekly, subset= TRice_raw_weekly$Data_Source=="Enumerator"))
TRice_raw_weekly_crd<- as.data.frame(subset(TRice_raw_weekly, subset= TRice_raw_weekly$Data_Source=="Crowd")) 

TRice_raw_weekly_ref_crd <- full_join(TRice_raw_weekly_ref, TRice_raw_weekly_crd, by = "week")

ggplot(TRice_raw_weekly_ref_crd, aes(x = Weekly_price.y, y = Weekly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Weekly Crowdsourced Price",
    y = "Weekly Enumerator Price",
    title = "Thailand Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(Weekly_price.x ~ Weekly_price.y, data = TRice_raw_weekly_ref_crd))



##Thailand Rice Monthly

TRice_raw_monthly <- as.data.frame(subset(Raw_monthly_avg, subset=Raw_monthly_avg$commodity=="Ymaize"))

TRice_raw_monthly_ref<-as.data.frame(subset(TRice_raw_monthly, subset= TRice_raw_monthly$Data_Source=="Enumerator"))
TRice_raw_monthly_crd<- as.data.frame(subset(TRice_raw_monthly, subset= TRice_raw_monthly$Data_Source=="Crowd")) 

TRice_raw_monthly_ref_crd <- full_join(TRice_raw_monthly_ref, TRice_raw_monthly_crd, by = "month")

ggplot(TRice_raw_monthly_ref_crd, aes(x = monthly_price.y, y = monthly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Monthly Crowdsourced Price",
    y = "Monthly Enumerator Price",
    title = "Thailand Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(monthly_price.x ~ monthly_price.y, data = TRice_raw_monthly_ref_crd))





#For Maize monthly relationship of groundtruth data


Mze_raw_monthly <- as.data.frame(subset(Raw_monthly_avg, commodity=="Ymaize" | commodity=="Wmaize"))

Mze_raw_monthly <- Mze_raw_monthly %>%
  group_by(month,Data_Source) %>%
  summarize(monthly_price = round (mean(monthly_price, na.rm = TRUE)))


Mze_raw_monthly_ref<-as.data.frame(subset(Mze_raw_monthly, subset= Mze_raw_monthly$Data_Source=="Enumerator"))
Mze_raw_monthly_crd<- as.data.frame(subset(Mze_raw_monthly, subset= Mze_raw_monthly$Data_Source=="Crowd")) 

Mze_raw_monthly_ref_crd <- full_join(Mze_raw_monthly_ref, Mze_raw_monthly_crd, by = "month")

ggplot(Mze_raw_monthly_ref_crd, aes(x = monthly_price.y, y = monthly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Monthly Crowdsourced Price",
    y = "Monthly Enumerator Price",
    title = "Maize"
  )+
  theme(text = element_text(size = 22))

summary(lm(monthly_price.x ~ monthly_price.y, data = Mze_raw_monthly_ref_crd))

cor.test(Mze_raw_monthly_ref_crd$monthly_price.x, Mze_raw_monthly_ref_crd$monthly_price.y, method = "pearson")
stat.desc(Mze_raw_monthly_ref_crd$monthly_price.x)
stat.desc(Mze_raw_monthly_ref_crd$monthly_price.y)
t.test(Mze_raw_monthly_ref_crd$monthly_price.x, Mze_raw_monthly_ref_crd$monthly_price.y, alternative = "two.sided")
var.test(Mze_raw_monthly_ref_crd$monthly_price.x, Mze_raw_monthly_ref_crd$monthly_price.y)



#For Rice Monthly Relationship of overall data

Rice_raw_monthly <- as.data.frame(subset(Raw_monthly_avg, commodity=="Thailand_Rice" | commodity=="I_Rice"))

Rice_raw_monthly <- Rice_raw_monthly %>%
  group_by(month,Data_Source) %>%
  summarize(monthly_price = round (mean(monthly_price, na.rm = TRUE)))


Rice_raw_monthly_ref<-as.data.frame(subset(Rice_raw_monthly, subset= Rice_raw_monthly$Data_Source=="Enumerator"))
Rice_raw_monthly_crd<- as.data.frame(subset(Rice_raw_monthly, subset= Rice_raw_monthly$Data_Source=="Crowd")) 

Rice_raw_monthly_ref_crd <- full_join(Rice_raw_monthly_ref, Rice_raw_monthly_crd, by = "month")

ggplot(Rice_raw_monthly_ref_crd, aes(x = monthly_price.y, y = monthly_price.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Monthly Crowdsourced Price",
    y = "Monthly Enumerator Price",
    title = "Rice"
  )+
  theme(text = element_text(size = 22))

summary(lm(monthly_price.x ~ monthly_price.y, data = Rice_raw_monthly_ref_crd))

cor.test(Rice_raw_monthly_ref_crd$monthly_price.x, Rice_raw_monthly_ref_crd$monthly_price.y, method = "pearson")
stat.desc(Rice_raw_monthly_ref_crd$monthly_price.x)
stat.desc(Rice_raw_monthly_ref_crd$monthly_price.y)
t.test(Rice_raw_monthly_ref_crd$monthly_price.x, Rice_raw_monthly_ref_crd$monthly_price.y, alternative = "two.sided")
var.test(Rice_raw_monthly_ref_crd$monthly_price.x, Rice_raw_monthly_ref_crd$monthly_price.y)



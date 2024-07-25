
#############################################################################

### Monthly food prices from artificial intelligence (AI) strongly correlates with high-frequency crowdsourced prices within a fragile context.###    

#############################################################################     

################################################################################################################################################
################################################################################################################################################

##
##  This script: validation_code_fnl_v1##   
##                                                            
##  Authors: Julius Adewopo, Bo Pieter Johannes Andree, Helen Peter, Gloria Solano-Hermosilla, Fabio Micale
#
##  Date: February 2024 #
##                                                            

################################################################################################################################################
################################################################################################################################################






###******Run the "Raw_crowd_ref_analysis_v1.R codes first to get the input vars*****

library(ggplot2)
library(lubridate)
library(stats)
library(dplyr)
library(pastecs)
library(stats)
library(MASS)
library(scales)  # for date formatting
library(grid)    # for margin
library(quantmod) #for OHLC open and close changes



fpca_val_data<-Raw_daily[Raw_daily$Date >="2021-03-01" & Raw_daily$Date <="2021-10-31", ]
mze_crd<-subset(fpca_val_data, Commodity=="Maize" & Data_Source=="Crowd")
mze_crd<-data.frame(mze_crd, month = month(mze_crd$Date, label=TRUE))
mze_crd<-mze_crd[ mze_crd$Market_type=="City_market"|mze_crd$Market_type=="Supermarket"|mze_crd$Market_type=="Open air_or_covered market"|mze_crd$Market_type=="Neighborhood_shops_kiosk"|mze_crd$Market_type=="Mobile_shops_street vendors"| mze_crd$Market_type=="Specialized_stores",]#Select for only retail prices
mze_crd<-subset(mze_crd,!is.na(mze_crd$price))#Remove NAs
hist(mze_crd$price, 100) #Histogram check prior to outlier removal
mze_crd<-mze_crd[mze_crd$price<500 & mze_crd$price>50,] #Removing spurious outliers based on the histogram and ground level understanding that maize was not above N1000/kg at retail during the period..
hist(mze_crd$price, 100) #Histogram check  after outlier removal


##For raw AI data
AI_full<-read.csv("WB_monthly_data.csv")
Mze_AI<-subset(AI_full, AI_full$Commodity=="maize")
Mze_AI$Date<-as.Date(Mze_AI$Date, format = "%m/%d/%Y")
mze_AI2<-Mze_AI
mze_AI2$Date<-as.Date(mze_AI2$Date, format = "%m/%d/%Y")
mze_AI2<-mze_AI2[mze_AI2$Date >="2021-03-30" & mze_AI2$Date <="2021-11-30", ]
mze_AI2<-data.frame(mze_AI2, month = month(mze_AI2$Date, label=TRUE))
mze_AI2<-subset(mze_AI2, Commodity=="maize")
mze_AI2$monthly_change=NA

mze_AI2_OHLC <- as.data.frame(mze_AI2 %>%
                                group_by(Date) %>%
                                summarize(Open = mean(price_O, na.rm = TRUE),
                                          Low = mean(price_L, na.rm = TRUE),
                                          High = mean(price_H, na.rm = TRUE),
                                          Close = mean(price_C, na.rm = TRUE)))


# Create a column for monthly change (assuming you have a column named 'monthly_change' in mze_AI2)
mze_AI2_OHLC$monthly_change <- ifelse(Cl(mze_AI2_OHLC) > Op(mze_AI2_OHLC), "up", "down")
mze_AI2_OHLC$monthly_change <- factor(mze_AI2_OHLC$monthly_change, levels = c("up", "down"))


mze_enum<-subset(fpca_val_data, Commodity=="Maize" & Data_Source=="Enumerator")
mze_enum$Date<-as.Date(mze_enum$Date, format = "%m/%d/%Y")
mze_enum<-data.frame(mze_enum, month = month(mze_enum$Date, label=TRUE))
mze_enum<-mze_enum[mze_enum$Market_type=="City_market"|mze_enum$Market_type=="Supermarket"|mze_enum$Market_type=="Open air_or_covered market"|mze_enum$Market_type=="Neighborhood_shops_kiosk"|mze_enum$Market_type=="Mobile_shops_street vendors"| mze_enum$Market_type=="Specialized_stores",]#Select for only retail prices
hist(mze_enum$price,100)
mze_enum<-subset(mze_enum,!is.na(mze_enum$price))#Remove NAs
mze_enum<-mze_enum[mze_enum$price<500 & mze_enum$price>50,] #Removing spurious outliers based on the histogram and ground level understanding that maize was not above N1000/kg at retail during the period..

mze_crd_100kg<-mze_crd[mze_crd$Market=="Wholesale",] #Selecting for only 100kg Retail Package
mze_enum_100kg<-mze_enum[mze_enum$Market=="Wholesale",] #Selecting for only 100Kg retail Package



##Averaging enumerator and crowd data by day
mze_enum2 <-mze_enum %>%
  group_by(Date) %>%
  summarize(price = mean(price, na.rm = TRUE))

mze_crd2 <- mze_crd %>%
  group_by(Date) %>%
  summarize(price = mean(price, na.rm = TRUE)) #Daily averages per commodity


mze_enum2_100kg <-mze_enum_100kg %>%
  group_by(Date) %>%
  summarize(price = mean(price, na.rm = TRUE))

mze_crd2_100kg <- mze_crd_100kg %>%
  group_by(Date) %>%
  summarize(price = mean(price, na.rm = TRUE)) #Daily averages per commodity



ggplot() +
  # Scatter plot for mze_crd
  geom_point(data = mze_crd_100kg, aes(x = Date, y = price), color="orange", size = 1.5, alpha = 0.7)+
  geom_segment(data = mze_AI2, aes(x = Date, xend = Date, y = price_L, yend = price_H), color = "black", alpha = 0.8, size = 1) +
  geom_segment(data = mze_AI2_OHLC, aes(x = Date, xend = Date, y = Low, yend = High, color = monthly_change), alpha = 1, size = 3)+
  scale_color_manual(values = c("up" = "red", "down" = "green"))



###All datapoints for Retail

ggplot() +
  # Scatter plot for mze_crd
  geom_point(data = mze_crd, aes(x = Date, y = price), color = "orange", size = 1.5, alpha = 0.7) +
  
  # Scatter plot for mze_enum2
  geom_point(data = mze_enum, aes(x = Date, y = price), pch = 16, color = "chartreuse4", size = 2.0, alpha = 0.7, stroke = 1) +
  
  geom_point(data = mze_AI2, aes(x = Date, y = price_C), pch = 16, color = "red", size = 2.0, alpha = 0.7, stroke = 1) +
  ylim(0,500)+

  # Customizing axes and labels
  labs(x = "", y = "Price per Kg (Naira)") +
  
  # Adding plot titles
  ggtitle("") +
  
  # Adding minor ticks and showing each month on x-axis
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"), minor_breaks = "1 week") +
  
  # Customizing color scale
  scale_color_manual(values = c("up" = "red", "down" = "blue")) +
  
  # Customizing theme
  #theme_minimal() +
  theme(
    text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    #axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.x=element_blank(),
    legend.position = "none",
    #legend.box.margin = margin(10, 0, 0, 0),
    
    # Remove gridlines
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis line thickness
    #axis.line = element_line(size = 1.3, colour="darkred"),
    panel.border = element_rect(colour = "darkred", fill=NA, size=2)
  )


###All datapoints for wholesale - 100Kg  packages (Figure 2b)

datapoints_whls_mze<-ggplot() +
  # Scatter plot for mze_crd
  geom_point(data = mze_crd_100kg, aes(x = Date, y = price), color = "orange", size = 1.5, alpha = 0.7) +
  
  # Scatter plot for mze_enum2
  geom_point(data = mze_enum_100kg, aes(x = Date, y = price), pch = 16, color = "darkblue", size = 2.0, alpha = 0.75, stroke = 1) +
  
  # OHLC bar plot for mze_AI2 with color encoding for monthly change
  geom_point(data = mze_AI2, aes(x = Date, y = price_C), pch = 21, color = "darkred", fill="red", size = 2.0, alpha = 0.65, stroke = 1.5) +
  ylim(0,500)+
  
 
  # Customizing axes and labels
  labs(x = "", y = "Price per Kg (Naira)") +
  
  # Adding plot titles
  ggtitle("") +
  
  # Adding minor ticks and showing each month on x-axis
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"), minor_breaks = "1 week") +
  
  # Customizing theme
  #theme_minimal() +
  theme(
    text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    #axis.text.x=element_blank(),
    legend.position = "none",
    #legend.box.margin = margin(10, 0, 0, 0),
    
    # Remove gridlines
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis line thickness
    #axis.line = element_line(size = 1.3, colour="darkred"),
    panel.border = element_rect(colour = "darkred", fill=NA, size=2)
  )


ggsave(plot = datapoints_whls_mze, filename = "Fig2b.png",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)


## Daily Average Datapoints for Retail

ggplot() +
  # Scatter plot for mze_crd
  geom_point(data = mze_crd2, aes(x = Date, y = price), color = "orange", size = 2.0, alpha = 0.7) +
  
  # Scatter plot for mze_enum2
  geom_point(data = mze_enum2, aes(x = Date, y = price), pch = 16, color = "chartreuse4", size = 2.0, alpha = 0.7, stroke = 1) +
  
  geom_point(data = mze_AI2, aes(x = Date, y = price_C), pch = 16, color = "red", size = 2.0, alpha = 0.7, stroke = 1) +
  
  # Customizing axes and labels
  labs(x = "", y = "Price per Kg (Naira)") +
  
  # Adding plot titles
  ggtitle("") +
  
  # Adding minor ticks and showing each month on x-axis
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"), minor_breaks = "1 week") +
  
  # Customizing color scale
  scale_color_manual(values = c("up" = "red", "down" = "blue")) +
  
  # Customizing theme
  #theme_minimal() +
  theme(
    text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    #axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.x=element_blank(),
    legend.position = "none",
    #legend.box.margin = margin(10, 0, 0, 0),
    
    # Remove gridlines
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis line thickness
    #axis.line = element_line(size = 1.3, colour="darkred"),
    panel.border = element_rect(colour = "darkred", fill=NA, size=2)
  )



## Daily Average Datapoints for Wholesale - 100kg 

ggplot() +
  # Scatter plot for mze_crd
  geom_point(data = mze_crd2_100kg, aes(x = Date, y = price), color = "orange", size = 2.0, alpha = 0.7) +
  
  # Scatter plot for mze_enum2
  geom_point(data = mze_enum2_100kg, aes(x = Date, y = price), pch = 16, color = "darkblue", size = 2.0, alpha = 0.7, stroke = 1) +
  ylim(0,300)+
  geom_point(data = mze_AI2, aes(x = Date, y = price_C), pch = 21, color = "darkred", fill = "red", size = 2.0, alpha = 0.7, stroke = 1) +
  

  # Customizing axes and labels
  labs(x = "", y = "Price per Kg (Naira)") +
  
  # Adding plot titles
  ggtitle("") +
  
  # Adding minor ticks and showing each month on x-axis
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"), minor_breaks = "1 week") +
  
  
  # Customizing theme
  #theme_minimal() +
  theme(
    text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    #axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.x=element_blank(),
    legend.position = "none",
    #legend.box.margin = margin(10, 0, 0, 0),
    
    # Remove gridlines
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis line thickness
    #axis.line = element_line(size = 1.3, colour="darkred"),
    panel.border = element_rect(colour = "darkred", fill=NA, size=2)
  )



###Finalized trend Chart with OHLC (100kg - Wholesale) - Figure 2c

groundtruth_OHLC_mze<-ggplot() +
  # Scatter plot for mze_crd
  geom_smooth(data = mze_crd2_100kg, aes(x = Date, y = price), method = "loess", se = TRUE, color = "orange", fill="orange", linetype = "solid", size = 1) +
  geom_smooth(data = mze_enum2_100kg, aes(x = Date, y = price), method = "loess", se = TRUE, color = "darkblue", fill = "darkblue", linetype = "solid", size = 1) +
  ylim(0,300)+
  # OHLC bar plot for mze_AI2 with color encoding for monthly change
  geom_segment(data = mze_AI2, aes(x = Date, xend = Date, y = price_L, yend = price_H), color = "black", alpha = 0.8, size = 1) +
  geom_segment(data = mze_AI2_OHLC, aes(x = Date, xend = Date, y = Low, yend = High, color = monthly_change), alpha = 1, size = 3) +

  # Customizing axes and labels
  labs(x = "", y = "Price per Kg (Naira)") +
  
  # Adding plot titles
  ggtitle("") +
  
  # Adding minor ticks and showing each month on x-axis
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"), minor_breaks = "1 week") +
  
  # Customizing color scale
  scale_color_manual(values = c("up" = "red", "down" = "green")) +
  
  # Customizing theme
  #theme_minimal() +
  theme(
    text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    #legend.box.margin = margin(10, 0, 0, 0),
    
    # Remove gridlines
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis line thickness
    #axis.line = element_line(size = 1.3, colour = "darkred"),
    panel.border = element_rect(colour = "darkred", fill=NA, size=2)
  )


ggsave(plot = groundtruth_OHLC_mze, filename = "Fig2c.png",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)

###Finalized trend Chart with OHLC (All Retail)

ggplot() +
  # Scatter plot for mze_crd
  #geom_point(data = mze_crd2, aes(x = Date, y = Daily_price), color = "orange", size = 1.75, alpha = 0.9) +
  
  # Scatter plot for mze_enum2
  # geom_point(data = mze_enum2, aes(x = Date, y = Enum_Price), pch = 16, color = "chartreuse4", size = 1.75, alpha = 0.9, stroke = 1) +
  # Smooth line and confidence band for mze_enum2
  geom_smooth(data = mze_crd2, aes(x = Date, y = price), method = "loess", se = TRUE, color = "orange", fill="orange", linetype = "solid", size = 1) +
  geom_smooth(data = mze_enum2, aes(x = Date, y = price), method = "loess", se = TRUE, color = "darkblue", fill = "darkblue", linetype = "solid", size = 1) +
  ylim(0,300)+
  # OHLC bar plot for mze_AI2 with color encoding for monthly change
  geom_segment(data = mze_AI2, aes(x = Date, xend = Date, y = price_L, yend = price_H), color = "black", alpha = 0.8, size = 1) +
  geom_segment(data = mze_AI2_OHLC, aes(x = Date, xend = Date, y = Low, yend = High, color = monthly_change), alpha = 1, size = 3) +
  #geom_point(data = mze_AI2_OHLC, aes(x = Date, y = Open, color = monthly_change), pch = 15, size = 2.5, alpha = 0.7) +
  #geom_point(data = mze_AI2_OHLC, aes(x = Date, y = Close, color = monthly_change), pch = 16, size = 3.5, alpha = 1, stroke = 1) +
  
  # Customizing axes and labels
  labs(x = "", y = "Price per Kg (Naira)") +
  
  # Adding plot titles
  ggtitle("") +
  
  # Adding minor ticks and showing each month on x-axis
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b %Y"), minor_breaks = "1 week") +
  
  # Customizing color scale
  scale_color_manual(values = c("up" = "red", "down" = "green")) +
  
  # Customizing theme
  #theme_minimal() +
  theme(
    text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    #legend.box.margin = margin(10, 0, 0, 0),
    
    # Remove gridlines
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis line thickness
    #axis.line = element_line(size = 1.3, colour = "darkred"),
    panel.border = element_rect(colour = "darkred", fill=NA, size=2)
  )



#Counting the number of raw points
count_mze_whsl<-subset (Raw_daily, Raw_daily$Market=="Wholesale"& Raw_daily$Commodity =="Maize")
nrow(count_mze_whsl[!is.na(count_mze_whsl$price),])

count_mze_rtl<-subset (Raw_daily, Raw_daily$Market=="Retail"& Raw_daily$Commodity =="Maize")
nrow(count_mze_rtl[!is.na(count_mze_rtl$price),])

count_mze_fgt<-subset (Raw_daily, Raw_daily$Market=="Farmgate"& Raw_daily$Commodity =="Maize")
nrow(count_mze_fgt[!is.na(count_mze_fgt$price),])

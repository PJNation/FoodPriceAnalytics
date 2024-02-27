

#############################################################################

### Monthly food prices from artificial intelligence (AI) strongly correlates with high-frequency crowdsourced prices within a fragile context.###    

#############################################################################     

################################################################################################################################################
################################################################################################################################################

##
##  This script: Final_WB_FPCA_Code ##   
##                                                            
##  Authors: Julius Adewopo, Bo Pieter Johannes Andree, Helen Peter, Gloria Solano-Hermosilla, Fabio Micale
#
##  Date: February 2024 #
##                                                            

################################################################################################################################################
################################################################################################################################################



library(stats)
library(tseries)
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
library(pastecs)
library(ggthemes)

setwd("C:/Users/PJNat/Dropbox/Projects/WB_FTE_Consulting/Data/working_csv")
WB_mth <- read.csv("WB_monthly_data.csv")
FPCA_wk <- read.csv("fpca_all.csv")

#Reformat both datasets to properly reference dates
#Formating the datasets to reference dates
WB_mth$Date <- as.Date(WB_mth$Date, format = "%m/%d/%Y")
FPCA_wk$Date <- as.Date(FPCA_wk$Date, format = "%m/%d/%Y")
FPCA_wk$Date_Wk <- as.Date(FPCA_wk$Date_Wk, format = "%m/%d/%Y")
head(WB_mth)
head(FPCA_wk)


WB_mth <- WB_mth %>% mutate(MonthYear = format(Date, "%Y-%m-%d"))
FPCA_wk <- FPCA_wk %>% mutate(MonthYear = format(Date, "%Y-%m-%d"))

###Notes
#For WB dataset, we'll reference the price_C, i.e. the closing price at the end of the month which is considered to reflect the prevaling market price action for the month

#Assessing for regional (combined across states) relationship to infer on overall price relationship (as a proxy for national)

##Maize

WB_mze <- subset(WB_mth, Commodity=="maize")#Subset by WB data by Commodity #Maize 
FPCA_mze <- subset(FPCA_wk, subset= (FPCA_wk$Commodity=="maize"))#Subset FPCA by Commodity #Maize
FPCA_mze_white <- subset(FPCA_wk, Commodity_type=="maize_white")
FPCA_mze_yellow <- subset(FPCA_wk, Commodity_type=="maize_yellow")
stat.desc(FPCA_mze$price)

WB_mze2 <-WB_mze %>%
  group_by(MonthYear,Commodity) %>%
  summarize(price_C = mean(price_C, na.rm = TRUE))


WB_FPCA_mze <- merge(WB_mze2, FPCA_mze, by = "MonthYear", all= TRUE)#Merge Datasets

# Calculate monthly averages for each dataset
WB_mze_avg <- as.data.frame(WB_mze2 %>%
                              group_by(MonthYear) %>%
                              summarize(Monthly_WB_mze = mean(price_C, na.rm = TRUE)))

FPCA_mze_avg <- as.data.frame(FPCA_mze %>%
                                group_by(MonthYear) %>%
                                summarize(Monthly_FPCA_mze = mean(price, na.rm = TRUE)))

Avg_WB_FPCA_mze <- merge(WB_mze_avg, FPCA_mze_avg, by = "MonthYear", all = TRUE)
Avg_WB_FPCA_mze$MonthYear <- as.Date(Avg_WB_FPCA_mze$MonthYear)


mze_trend<-ggplot(Avg_WB_FPCA_mze, aes(x = MonthYear)) + 
  geom_point(aes(y = Monthly_WB_mze, color = "Monthly Avg"), size = 3.0, alpha=0.8) +
  geom_line(aes(y = Monthly_WB_mze, color = "Monthly Avg"), lty = "solid", lwd=1.1) +
  geom_point(aes(y = Monthly_FPCA_mze, color = "Weekly Avg"), size = 3.0) +
  geom_line(aes(y = Monthly_FPCA_mze, color = "Weekly Avg"), lty = "dashed", lwd=1.1) +
  scale_color_manual(
    values = c("Monthly Avg" = "darkgreen", "Weekly Avg" = "orange"),
    labels = c("AI-Estimated Price", "Crowdsourced Price")
  ) +
  labs(
    x = "Date (By Year)",
    y = "Average Price (???/Kg)",
    color = "Data Source"
  ) +
  #theme_minimal()+
  #scale_x_date(breaks = scales::breaks_width("4 months"), date_labels = "%Y-%m-%d")+
  theme(
    text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(arrow = arrow(angle = 30,
                                           length = unit(0.15, "inches"),
                                           ends = "last", 
                                           type = "closed")),
    legend.position = c(0.05, 0.9),
    legend.justification = c(0, 1),
    legend.box.margin = margin(10, 0, 0, 0)
    
    # theme(
    #  text = element_text(size = 18),
    # legend.position = c(0.1, 0.9),
    #legend.justification = c(0, 1) # Adjust the font size as needed
  )

ggsave(plot = mze_trend, filename = "figure_4a.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)


##fit linear model for quick assessment of Relationship
a<-lm(Avg_WB_FPCA_mze$Monthly_WB_mze ~ Avg_WB_FPCA_mze$Monthly_FPCA_mze)
plot(a)
summary(a)

wt_mze<-subset(WB_FPCA_mze, Commodity_type=="maize_white")
summary (lm(wt_mze$price_C ~ wt_mze$price))

ylw_mze<-subset(WB_FPCA_mze, Commodity_type=="maize_yellow")
summary (lm(ylw_mze$price_C ~ ylw_mze$price))

cor.test(Avg_WB_FPCA_mze$Monthly_WB_mze,Avg_WB_FPCA_mze$Monthly_FPCA_mze, method = "pearson")
stat.desc(Avg_WB_FPCA_mze$Monthly_FPCA_mze)
t.test(Avg_WB_FPCA_mze$Monthly_WB_mze, Avg_WB_FPCA_mze$Monthly_FPCA_mze, paired = TRUE, alternative = "two.sided")
var.test(Avg_WB_FPCA_mze$Monthly_WB_mze, Avg_WB_FPCA_mze$Monthly_FPCA_mze)



## Plotting Relationship Charts for Maize

#Creating Facet charts (States and Commodity Types) for Maize prices to assess correlation between both data sources
WB_FPCA_mze_alt <- merge(WB_mze, FPCA_mze, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_mze_alt <- WB_FPCA_mze_alt %>%
  group_by(Commodity_type, State, MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs; 
##Note that "Mean_weekly_Price" is the weekly crowdsourced price averaged into months, and "Mean_Monthly_Price" is the average monthly WB price for each month





##Regional Chart for Maize

mze_related<-Avg_WB_FPCA_mze %>%
  ggplot(aes(x = Monthly_FPCA_mze, y = Monthly_WB_mze)) +
  geom_point(size=3) +
  geom_smooth(method = "lm",) +
  #geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Crowdsourced Price (???/kg)", y = "AI-Estimated Price (???/kg)",
       title = "") +
  
  theme_minimal()+
  theme(
    axis.line = element_line(arrow = arrow(angle = 30,
                                           length = unit(0.15, "inches"),
                                           ends = "last", 
                                           type = "closed")), text = element_text(size = 15))

ggsave(plot = mze_related, filename = "figure_4b.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)

 
lm_mze<- lm(Monthly_FPCA_mze ~ Monthly_WB_mze, data = Avg_WB_FPCA_mze)
summary(lm_mze)

##Disaggregated Chart for Maize by State and Commodity

compare_state<-WB_FPCA_mze_alt%>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price, color = Commodity_type)) +
  geom_point(shape=21, size=2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  facet_grid(Commodity_type ~ State, scales = "free") +
  labs(x = "Crowdsourced Price (???/kg)", y = "AI-estimated Price (???/kg)",
       title = "") +
  #theme_minimal()+
  theme_linedraw()+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(),
        legend.position = "none")


ggsave(plot = compare_state, filename = "figure_5a.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)




#Tabulate relationship results by State and Commodity Types
results_table_mze <- data.frame(
  Commodity_type = character(),
  State = character(),
  R_squared = numeric(),
  Correlation_Coefficient = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (commodity in unique(WB_FPCA_mze_alt$Commodity_type)) {
  for (state in unique(WB_FPCA_mze_alt$State)) {
    subset_data <- WB_FPCA_mze_alt %>%
      filter(Commodity_type == commodity, State == state)
    
    lm_fit <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = subset_data)
    summary_fit <- summary(lm_fit)
    
    r_squared <- summary_fit$r.squared
    correlation_coefficient <- cor(subset_data$Mean_Weekly_Price, subset_data$Mean_Monthly_Price)
    p_value <- coef(summary_fit)["Mean_Weekly_Price", "Pr(>|t|)"]
    avg_WB <- mean (subset_data$Mean_Monthly_Price,na.rm=TRUE)
    avg_crd <- mean (subset_data$Mean_Weekly_Price,na.rm=TRUE)
    
    results_table_mze <- rbind(results_table_mze, data.frame(
      Commodity_type = commodity,
      State = state,
      avg_wB = avg_WB,
      avg_crd = avg_crd,
      R_squared = r_squared,
      Correlation_Coefficient = correlation_coefficient,
      P_Value = p_value
    ))
  }
}
print(results_table_mze)



##-----------Plotting price relationships for Maize by Market segment

#Wholesale Market Segment - Maize-Regional

FPCA_mze_whsl<-subset(FPCA_mze, Market=="wholesale")
WB_FPCA_mze_whsl <- merge(WB_mze, FPCA_mze_whsl, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_mze_whsl <- WB_FPCA_mze_whsl %>%
  group_by(MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs

WB_FPCA_mze_whsl %>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Mean Crowdsourced Price", y = "Mean WB-AI Price",
       title = "") +
  theme_minimal()+
  theme(
    text = element_text(size = 22))

lm_mze_whsl <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = WB_FPCA_mze_whsl)
summary(lm_mze_whsl)





#Retail Market Segment - Maize-Regional
FPCA_mze_rtl<-subset(FPCA_mze, Market=="retail")
WB_FPCA_mze_rtl <- merge(WB_mze, FPCA_mze_rtl, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_mze_rtl <- WB_FPCA_mze_rtl %>%
  group_by(MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs


WB_FPCA_mze_rtl %>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Mean Crowdsourced Price", y = "Mean WB-AI Price",
       title = "") +
  theme_minimal()+
  theme(
    text = element_text(size = 22))

lm_mze_rtl <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = WB_FPCA_mze_rtl)
summary(lm_mze_rtl)


#Farmgate Market Segment - Maize-Regional
FPCA_mze_fgt<-subset(FPCA_mze, Market=="farm_gate")
WB_FPCA_mze_fgt <- merge(WB_mze, FPCA_mze_fgt, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_mze_fgt <- WB_FPCA_mze_fgt %>%
  group_by(MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs


WB_FPCA_mze_fgt %>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Mean Crowdsourced Price", y = "Mean WB-AI Price",
       title = "") +
  theme_minimal()+
  theme(
    text = element_text(size = 22))

lm_mze_fgt <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = WB_FPCA_mze_fgt)
summary(lm_mze_fgt)



#For scatter plots
combined_mzesgmt <- bind_rows(
  mutate(WB_FPCA_mze_fgt, Segment = "Farmgate"),
  mutate(WB_FPCA_mze_rtl, Segment = "Retail"),
  mutate(WB_FPCA_mze_whsl, Segment = "Wholesale"))


combined_mzesgmt$Segment <- factor(combined_data2$Segment, levels = c("Farmgate", "Retail", "Wholesale"))#order sequence
options(repr.plot.width = 10, repr.plot.height = 6, repr.dpi = 300)
compare_mktsgmnt<-ggplot(combined_mzesgmt, aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~Segment, scales = "fixed") +
  labs(
    x = "Crowdsourced Price (???/kg)",
    y = "AI-estimated Price (???/kg)",
    title = ""
  )+
  theme_minimal() +
  theme_linedraw()+
  theme_light()+
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

ggsave(plot = compare_mktsgmnt, filename = "figure_5b.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)




###---------------Implementing analytics for Rice prices--------------

WB_rice <- subset(WB_mth, Commodity=="rice")#Subset by WB data by Commodity #Rice 
WB_rice2 <-WB_rice %>%
  group_by(MonthYear,Commodity) %>%
  summarize(price_C = mean(price_C, na.rm = TRUE))


#Subset FPCA by Commodity #Rice
FPCA_rice <- subset(FPCA_wk, subset= (FPCA_wk$Commodity=="rice"))
FPCA_rice_indian <- subset(FPCA_wk, Commodity_type=="rice_indian")
FPCA_rice_thailand <- subset(FPCA_wk, Commodity_type=="rice_thailand")


#Merge Datasets
WB_FPCA_rice <- merge(WB_rice2, FPCA_rice, by = "MonthYear", all= TRUE)



# Calculate monthly averages for each dataset
WB_rice_avg <- as.data.frame(WB_rice2 %>%
                               group_by(MonthYear) %>%
                               summarize(Monthly_WB_rice = mean(price_C, na.rm = TRUE)))

FPCA_rice_avg <- as.data.frame(FPCA_rice %>%
                                 group_by(MonthYear) %>%
                                 summarize(Monthly_FPCA_rice = mean(price, na.rm = TRUE)))

Avg_WB_FPCA_rice <- merge(WB_rice_avg, FPCA_rice_avg, by = "MonthYear", all = TRUE)
Avg_WB_FPCA_rice$MonthYear <- as.Date(Avg_WB_FPCA_rice$MonthYear)


rice_trend<-ggplot(Avg_WB_FPCA_rice, aes(x = MonthYear)) + 
  geom_point(aes(y = Monthly_WB_rice, color = "Monthly Avg"), size = 2) +
  geom_line(aes(y = Monthly_WB_rice, color = "Monthly Avg"), lty = "solid", lwd=1.1) +
  geom_point(aes(y = Monthly_FPCA_rice, color = "Weekly Avg"), size = 2) +
  #geom_line(aes(y = Monthly_FPCA_rice, color = "Weekly Avg"), lty = "dashed", lwd=1.1) +
 scale_color_manual(
  values = c("Monthly Avg" = "darkgreen", "Weekly Avg" = "orange"),
  labels = c("AI-Estimated Price", "Crowdsourced Price")
) +
  labs(
    x = "Date (By Year)",
    y = "Average Price (???/Kg)",
    # title = "Average Monthly commodity prices from WB's AI estimate and Crowdsourced Data in Nigeria",
    color = "Data Source"
  ) +
  #theme_minimal()+
  #scale_x_date(breaks = scales::breaks_width("4 months"), date_labels = "%Y-%m-%d")+
  theme(
    text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(arrow = arrow(angle = 30,
                                           length = unit(0.15, "inches"),
                                           ends = "last", 
                                           type = "closed")),
    
    legend.position = c(0.05, 0.9),
    legend.justification = c(0, 1),
    legend.box.margin = margin(10, 0, 0, 0)
  )

ggsave(plot = rice_trend, filename = "Appendix_2a.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)

#fit linear model for quick assessment of Relationship
b<-lm(Avg_WB_FPCA_rice$Monthly_WB_rice ~ Avg_WB_FPCA_rice$Monthly_FPCA_rice)
plot(b)
abline(b)
summary(b)


thai_rice<-subset(WB_FPCA_rice, Commodity_type=="rice_thailand")
summary (lm(thai_rice$price_C ~ thai_rice$price))

indian_rice<-subset(WB_FPCA_rice, Commodity_type=="rice_indian")
summary (lm(indian_rice$price_C ~ indian_rice$price))


cor.test(Avg_WB_FPCA_rice$Monthly_WB_rice, Avg_WB_FPCA_rice$Monthly_FPCA_rice, method = "pearson")
stat.desc(Avg_WB_FPCA_rice$Monthly_FPCA_rice)
t.test(Avg_WB_FPCA_rice$Monthly_WB_rice, Avg_WB_FPCA_rice$Monthly_FPCA_rice, paired = TRUE, alternative = "two.sided")
var.test(Avg_WB_FPCA_rice$Monthly_WB_rice, Avg_WB_FPCA_rice$Monthly_FPCA_rice)



## plotting Relationship Charts for Rice

#Creating Facet charts (States and Commodity Types) for Rice prices to assess correlation between both data sources
WB_FPCA_rice_alt <- merge(WB_rice, FPCA_rice, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_rice_alt <- WB_FPCA_rice_alt %>%
  group_by(Commodity_type, State, MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs
##Note that "Mean_weekly_Price" is the weekly crowdsourced price averaged into months, and "Mean_Monthly_Price" is the average monthly WB price for each month


##Regional Chart showing price relationship
rice_relate<-Avg_WB_FPCA_rice %>%
  ggplot(aes(x = Monthly_FPCA_rice, y = Monthly_WB_rice)) +
  geom_point(size=3) +
  geom_smooth(method="lm",)+
  #geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Crowdsourced Price (???/kg)", y = "AI-estimated Price (???/kg)",
       title = "") +
  theme_minimal()+
  theme(
    text = element_text(size = 22), axis.line = element_line(arrow = arrow(angle = 30,
                                                                               length = unit(0.15, "inches"),
                                                                               ends = "last", 
                                                                               type = "closed")))

ggsave(plot = rice_relate, filename = "Appendix_2b.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)

##Chart showing segmenting of Rice by State and Commodity types

rice_state<-WB_FPCA_rice_alt%>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price, color = Commodity_type)) +
  geom_point(shape=21, size=2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  facet_grid(Commodity_type ~ State, scales = "free") +
  labs(x = "Monthly Crowdsourced Price (???/kg)", y = "Monthly AI Price (???/kg)",
       title = "") +
  #theme_minimal()+
  theme_linedraw()+
  theme_light()+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(),
        text = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line())

ggsave(plot = rice_state, filename = "Appendix_3.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)

#Tabulate State segmented relationship results for Rice

results_table_rice <- data.frame(
  Commodity_type = character(),
  State = character(),
  R_squared = numeric(),
  Correlation_Coefficient = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (commodity in unique(WB_FPCA_rice_alt$Commodity_type)) {
  for (state in unique(WB_FPCA_rice_alt$State)) {
    subset_data <- WB_FPCA_rice_alt %>%
      filter(Commodity_type == commodity, State == state)
    
    lm_fit <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = subset_data)
    summary_fit <- summary(lm_fit)
    
    r_squared <- summary_fit$r.squared
    correlation_coefficient <- cor(subset_data$Mean_Weekly_Price, subset_data$Mean_Monthly_Price)
    p_value <- coef(summary_fit)["Mean_Weekly_Price", "Pr(>|t|)"]
    
    results_table_rice <- rbind(results_table_rice, data.frame(
      Commodity_type = commodity,
      State = state,
      R_squared = r_squared,
      Correlation_Coefficient = correlation_coefficient,
      P_Value = p_value
    ))
  }
}
print(results_table_rice)





##-------------Plotting Price Relationships by Market Segment---------------

#Market Facet for rice at wholesale
FPCA_rice_whsl<-subset(FPCA_rice, Market=="wholesale")
WB_FPCA_rice_whsl <- merge(WB_rice, FPCA_rice_whsl, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_rice_whsl <- WB_FPCA_rice_whsl %>%
  group_by(MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs

#Wholesale-Rice-Regional

WB_FPCA_rice_whsl %>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Mean Crowdsourced Price", y = "Mean WB-AI Price",
       title = "") +
  theme_minimal()+
  theme(
    text = element_text(size = 22))

lm_rice_whsl <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = WB_FPCA_rice_whsl)
summary(lm_rice_whsl)



#Retail-Rice-Regional
FPCA_rice_rtl<-subset(FPCA_rice, Market=="retail")
WB_FPCA_rice_rtl <- merge(WB_rice, FPCA_rice_rtl, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_rice_rtl <- WB_FPCA_rice_rtl %>%
  group_by(MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs

WB_FPCA_rice_rtl %>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Mean Crowdsourced Price", y = "Mean WB-AI Price",
       title = "") +
  theme_minimal()+
  theme(
    text = element_text(size = 22))

lm_rice_rtl <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = WB_FPCA_rice_rtl)
summary(lm_rice_rtl)


#Farmgate-Rice-Regional
FPCA_rice_fgt<-subset(FPCA_rice, Market=="farm_gate")
WB_FPCA_rice_fgt <- merge(WB_rice, FPCA_rice_fgt, by = c("MonthYear", "Commodity", "State") , all= TRUE)

WB_FPCA_rice_fgt <- WB_FPCA_rice_fgt %>%
  group_by(MonthYear) %>%
  summarize(Mean_Weekly_Price = mean(price, na.rm = TRUE), 
            Mean_Monthly_Price = mean(price_C, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weekly_Price) & !is.na(Mean_Monthly_Price)) #filter out NAs


WB_FPCA_rice_fgt %>%
  ggplot(aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), formula = y ~ x)+
  labs(x = "Mean Crowdsourced Price", y = "Mean WB-AI Price",
       title = "") +
  theme_minimal()+
  theme(
    text = element_text(size = 22))

lm_rice_fgt <- lm(Mean_Monthly_Price ~ Mean_Weekly_Price, data = WB_FPCA_rice_fgt)
summary(lm_rice_fgt)



#For scatter plots
combined_ricesgmt <- bind_rows(
  mutate(WB_FPCA_rice_fgt, Segment = "Farmgate"),
  mutate(WB_FPCA_rice_rtl, Segment = "Retail"),
  mutate(WB_FPCA_rice_whsl, Segment = "Wholesale"))


combined_ricesgmt$Segment <- factor(combined_data4$Segment, levels = c("Farmgate", "Retail", "Wholesale"))#order sequence
options(repr.plot.width = 10, repr.plot.height = 6, repr.dpi = 300)
ggplot(combined_ricesgmt, aes(x = Mean_Weekly_Price, y = Mean_Monthly_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~Segment, scales = "fixed") +
  labs(
    x = "Crowdsourced Price (???/kg)",
    y = "AI-estimated Price (???/kg)",
    title = ""
  )+
  theme_minimal() +
  theme_linedraw()+
  theme_light()+
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )






###----------Appendix Plot of Maize & Rice price data trend for Crowdsourced FPCA data-------

FPCA_wk %>%
  group_by(Commodity, State, Date_Wk) %>%
  summarize(Mean_Weekly_Price = mean(price), 
            CI_Lower = quantile(price, 0.025), 
            CI_Upper = quantile(price, 0.975)) %>%
  ggplot(aes(x = Date_Wk, y = Mean_Weekly_Price, color = Commodity)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2, linetype=0) +
  facet_grid(Commodity ~ State, scales = "free_y") +
  labs(x = "Date", y = "Mean Price", title = "Mean Weekly Prices with Confidence Intervals") +
  theme_minimal()+
  theme(
    text = element_text(size = 22))




##Counting Complete Data records by State for Maize
test_kd<-subset (WB_FPCA_mze_alt, WB_FPCA_mze_alt$State=="Kaduna"& WB_FPCA_mze_alt$Commodity_type=="maize_yellow")
nrow(test_kd[!is.na(test_kd$Mean_Weekly_Price),])

test_kd2<-subset (WB_FPCA_mze_alt, WB_FPCA_mze_alt$State=="Kaduna"& WB_FPCA_mze_alt$Commodity_type=="maize_white")
nrow(test_kd2[!is.na(test_kd2$Mean_Weekly_Price),])


test_kn<-subset (WB_FPCA_mze_alt, WB_FPCA_mze_alt$State=="Kano"& WB_FPCA_mze_alt$Commodity_type=="maize_yellow")
nrow(test_kn[!is.na(test_kn$Mean_Weekly_Price),])

test_kn2<-subset (WB_FPCA_mze_alt, WB_FPCA_mze_alt$State=="Kano"& WB_FPCA_mze_alt$Commodity_type=="maize_white")
nrow(test_kn2[!is.na(test_kn2$Mean_Weekly_Price),])

test_kt<-subset (WB_FPCA_mze_alt, WB_FPCA_mze_alt$State=="Katsina"& WB_FPCA_mze_alt$Commodity_type=="maize_yellow")
nrow(test_kt[!is.na(test_kt$Mean_Weekly_Price),])

test_kt2<-subset (WB_FPCA_mze_alt, WB_FPCA_mze_alt$State=="Katsina"& WB_FPCA_mze_alt$Commodity_type=="maize_white")
nrow(test_kt2[!is.na(test_kt2$Mean_Weekly_Price),])







##Counting Complete Data records by State for Rice
rtest_kd<-subset (WB_FPCA_rice_alt, WB_FPCA_rice_alt$State=="Kaduna"& WB_FPCA_rice_alt$Commodity_type=="rice_thailand")
nrow(rtest_kd[!is.na(rtest_kd$Mean_Weekly_Price),])

rtest_kd2<-subset (WB_FPCA_rice_alt, WB_FPCA_rice_alt$State=="Kaduna"& WB_FPCA_rice_alt$Commodity_type=="rice_indian")
nrow(rtest_kd2[!is.na(rtest_kd2$Mean_Weekly_Price),])

rtest_kn<-subset (WB_FPCA_rice_alt, WB_FPCA_rice_alt$State=="Kano"& WB_FPCA_rice_alt$Commodity_type=="rice_thailand")
nrow(rtest_kn[!is.na(rtest_kn$Mean_Weekly_Price),])

rtest_kn2<-subset (WB_FPCA_rice_alt, WB_FPCA_rice_alt$State=="Kano"& WB_FPCA_rice_alt$Commodity_type=="rice_indian")
nrow(rtest_kn2[!is.na(rtest_kn2$Mean_Weekly_Price),])


rtest_kt<-subset (WB_FPCA_rice_alt, WB_FPCA_rice_alt$State=="Katsina"& WB_FPCA_rice_alt$Commodity_type=="rice_thailand")
nrow(rtest_kt[!is.na(rtest_kt$Mean_Weekly_Price),])

rtest_kt2<-subset (WB_FPCA_rice_alt, WB_FPCA_rice_alt$State=="Katsina"& WB_FPCA_rice_alt$Commodity_type=="rice_indian")
nrow(rtest_kt2[!is.na(rtest_kt2$Mean_Weekly_Price),])




#############################################################################

### Monthly food prices from artificial intelligence (AI) strongly correlates with high-frequency crowdsourced prices within a fragile context.###    

#############################################################################     

################################################################################################################################################
################################################################################################################################################

##
##  This script: Paper_charts.R ##   
##                                                            
##  Authors: Julius Adewopo, Bo Pieter Johannes Andree, Helen Peter, Gloria Solano-Hermosilla, Fabio Micale
#
##  Date: February 2024 #
##                                                            

################################################################################################################################################
################################################################################################################################################






###******Run the "Raw_crowd_ref_analysis.R codes first to get the input vars*****




combined_data <- bind_rows(
     mutate(YMze_Raw_day2, Time_Scale = "Daily"),
     mutate(YMze_Raw_week, Time_Scale = "Weekly"),
     mutate(YMze_Raw_month, Time_Scale = "Monthly"))


combined_data$price <- as.numeric(round(rowMeans(combined_data[c(3,6,8)], na.rm=TRUE)))

combined_data$Time_Scale <- factor(combined_data$Time_Scale, levels = c("Daily", "Weekly", "Monthly"))#order sequence
options(repr.plot.width = 10, repr.plot.height = 6, repr.dpi = 300)
groundtruth_crd_enum<-ggplot(data = combined_data, aes(x = Date, y = price, color = Data_Source)) +
  geom_point(size = 1.5, alpha=0.6) +
  geom_smooth() +
  scale_color_manual(values = c("Enumerator" = "blue", "Crowd" = "orange", alpha=0.2)) +
  facet_wrap(~Time_Scale, scales = "fixed") +
  #facet_grid(Data_Source ~ Time_Scale, scales = "free", space = "free")+
  labs(
    x = "Date",
    y = "Price (N/kg)", #Can change the unit symbol to (???/kg)
   title = "",
    color = "Data Source"
  ) +
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


ggsave(plot = groundtruth_crd_enum, filename = "figure_3a.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)



#For scatter plots
combined_data2 <- bind_rows(
  mutate(YMze_raw_daily_ref_crd, Time_Scale = "Daily"),
  mutate(YMze_raw_weekly_ref_crd, Time_Scale = "Weekly"),
  mutate(YMze_raw_monthly_ref_crd, Time_Scale = "Monthly"))


combined_data2$price_x <- as.numeric(round(rowMeans(combined_data2[c(4,10,13)], na.rm=TRUE)))
combined_data2$price_y <- as.numeric(round(rowMeans(combined_data2[c(7,11,14)], na.rm=TRUE)))



combined_data2$Time_Scale <- factor(combined_data2$Time_Scale, levels = c("Daily", "Weekly", "Monthly"))#order sequence
options(repr.plot.width = 10, repr.plot.height = 6, repr.dpi = 300)
mze_relate<-ggplot(combined_data2, aes(x = price_y, y = price_x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~Time_Scale, scales = "fixed") +
  labs(
    x = "Crowdsourced Price (N/kg)", # Can change unit symbol to (???/kg)
    y = "Enumerator Price (N/kg)",
    title = "Yellow Maize"
  )+
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


ggsave(plot = mze_relate, filename = "figure_3b.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)


#TRice Combined plots



combined_data3 <- bind_rows(
  mutate(TRice_Raw_day2, Time_Scale = "Daily"),
  mutate(TRice_Raw_week, Time_Scale = "Weekly"),
  mutate(TRice_Raw_month, Time_Scale = "Monthly"))


combined_data3$price <- as.numeric(round(rowMeans(combined_data3[c(3,6,8)], na.rm=TRUE)))

combined_data3$Time_Scale <- factor(combined_data3$Time_Scale, levels = c("Daily", "Weekly", "Monthly"))#order sequence
options(repr.plot.width = 10, repr.plot.height = 6, repr.dpi = 300)
groundtruth_crd_enum2<-ggplot(data = combined_data3, aes(x = Date, y = price, color = Data_Source)) +
  geom_point(size = 1.5, alpha=0.6) +
  geom_smooth() +
  scale_color_manual(values = c("Enumerator" = "blue", "Crowd" = "orange", alpha=0.2)) +
  facet_wrap(~Time_Scale, scales = "fixed") +
  #facet_grid(Data_Source ~ Time_Scale, scales = "free", space = "free")+
  labs(
    x = "Date",
    y = "Price (N/kg)", #Can change symbol to (???/kg)
    title = "",
    color = "Data Source"
  ) +
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

ggsave(plot = groundtruth_crd_enum2, filename = "Appendix_1a.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)


#For scatter plots
combined_data4 <- bind_rows(
  mutate(TRice_raw_daily_ref_crd, Time_Scale = "Daily"),
  mutate(TRice_raw_weekly_ref_crd, Time_Scale = "Weekly"),
  mutate(TRice_raw_monthly_ref_crd, Time_Scale = "Monthly"))


combined_data4$price_x <- as.numeric(round(rowMeans(combined_data4[c(4,10,13)], na.rm=TRUE)))
combined_data4$price_y <- as.numeric(round(rowMeans(combined_data4[c(7,11,14)], na.rm=TRUE)))



combined_data4$Time_Scale <- factor(combined_data4$Time_Scale, levels = c("Daily", "Weekly", "Monthly"))#order sequence
options(repr.plot.width = 10, repr.plot.height = 6, repr.dpi = 300)
rice_relate<-ggplot(combined_data4, aes(x = price_y, y = price_x)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~Time_Scale, scales = "fixed") +
  labs(
    x = "Crowdsourced Price (N/kg)", #Can change symbol to (???/kg)
    y = "Enumerator Price (N/kg)", #Can change symbol to (???/kg)
    title = "Thailand Rice"
  )+
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


ggsave(plot = rice_relate, filename = "Appendix_1b.pdf",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)
















#####Scratch Below


# Plotting the combined data with additional scatterplot for each Data_Source
ggplot(data = combined_data, aes(x = Date, y = price, color = Data_Source)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linetype = "solid", color = "black") +
  facet_grid(Data_Source ~ Time_Scale, scales = "free", space = "free") +
  labs(
    x = "Date",
    y = "Price per kg (Naira)",
    title = "Yellow Maize Prices Over Time",
    color = "Data Source"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
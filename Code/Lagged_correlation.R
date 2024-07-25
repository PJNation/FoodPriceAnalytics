
#############################################################################

### Monthly food prices from artificial intelligence (AI) strongly correlates with high-frequency crowdsourced prices within a fragile context.###    

#############################################################################     

################################################################################################################################################
################################################################################################################################################

##
##  This script: Lagged_correlation ##   
##                                                            
##  Authors: Julius Adewopo, Bo Pieter Johannes Andree, Helen Peter, Gloria Solano-Hermosilla, Fabio Micale
#
##  Date: July 2024 #
##                                                            

################################################################################################################################################
################################################################################################################################################



##Run the codelines in the R script "Final_WB_FPCA_Code_PO" first

library(dplyr)
library(ggplot2)
library(tidyr)




# Merge datasets by date
merged_data <- merge(WB_mze_avg, FPCA_mze_avg, by = "MonthYear", suffixes = c("_1", "_2"))




# Function to create lagged and lead variables
lag<-1:5
lags<-5

create_lagged_data <- function(data, lags) {
  for (lag in lags) {
    data <- data %>%
      mutate(!!paste0("value_1_lag_", lag) := lag(Monthly_WB_mze, lag)) %>%
      mutate(!!paste0("value_1_lead_", lag) := lead(Monthly_WB_mze, lag))
  }
  return(data)
}


# Define lags and leads
lags_leads <- 1:5



# Create lagged and lead data
lagged_data <- create_lagged_data(merged_data, lag)


# Initialize results data frame
results <- data.frame(Lag = c(-lags_leads, lags_leads), Correlation = NA, Regression_Coefficient = NA)

# Compute correlation coefficients and regression coefficients
for (lag in lags_leads) {
  lag_col <- paste0("value_1_lag_", lag)
  lead_col <- paste0("value_1_lead_", lag)
  
  # Compute correlation for lags
  results$Correlation[results$Lag == -lag] <- cor(lagged_data$Monthly_FPCA_mze, lagged_data[[lag_col]], use = "complete.obs")
  # Compute regression for lags
  model <- lm(Monthly_FPCA_mze ~ lagged_data[[lag_col]], data = lagged_data)
  results$Adjusted_R_Squared[results$Lag == -lag] <- summary(model)$adj.r.squared
  
  # Compute correlation for leads
  results$Correlation[results$Lag == lag] <- cor(lagged_data$Monthly_FPCA_mze, lagged_data[[lead_col]], use = "complete.obs")
  # Compute regression for leads
  model <- lm(Monthly_FPCA_mze ~ lagged_data[[lead_col]], data = lagged_data)
  results$Adjusted_R_Squared[results$Lag == lag] <- summary(model)$adj.r.squared
}

# Plot the results
ggplot(results, aes(x = Lag)) +
  geom_line(aes(y = Correlation, color = "Correlation")) +
  geom_line(aes(y = Adjusted_R_Squared, color = "Adjusted R-Squared")) +
  labs(title = "Lagged and Lead Relationship between monthly Crowdsourced and AI-imputed Commodity Prices",
       x = "Lag (Months)",
       y = "Coefficient",
       color = "Legend") +
  theme_minimal()

# Print correlation matrix
cor_matrix <- results %>%
  select(Lag, Correlation) %>%
  spread(key = Lag, value = Correlation)

print(cor_matrix)

library(reshape2)

#Lagged and Lead Relationship between monthly Crowdsourced and AI-imputed Commodity Prices

p1 <- ggplot(results, aes(x = Lag)) +
  geom_line(aes(y = Correlation, color = "Correlation"), linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = Correlation, color = "Correlation"), size = 3) +
  geom_line(aes(y = Adjusted_R_Squared, color = "Adjusted R-Squared"), linewidth = 1.2) +
  geom_point(aes(y = Adjusted_R_Squared, color = "Adjusted R-Squared"), size = 3) +
  labs(title = "",
       x = "Lag (Months)",
       y = "Coefficient",
       color = "Metric") +
  #theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = c(0.5, 0.2),
        legend.justification = "center",
        axis.line = element_line(color="black", size = 0.5),
        #panel.grid.major = element_line(color="gray", size = 0.2),
        #panel.grid.minor = element_line(color="gray", size = 0.1),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.minor.y = element_blank(),
        #panel.background = element_blank(),
        #plot.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks = element_line(color="black", size = 0.5)) +
  scale_color_manual(values = c("Correlation" = "darkgray", "Adjusted R-Squared" = "darkgreen"))

ggsave(plot = p1, filename = "Fig5.png",
       width = 6.5, height = 4.5, units = 'in', dpi = 900)

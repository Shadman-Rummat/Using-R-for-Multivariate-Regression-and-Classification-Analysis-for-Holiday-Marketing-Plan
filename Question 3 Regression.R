install.packages("tidyverse")
install.packages("gridExtra")
install.packages("emmeans")
install.packages("reshape2")
install.packages("ggcorrplot")

library(tidyverse)
library(gridExtra)
library(emmeans)

#Loading dataset
booking <- as.data.frame(read_csv("E:/UoB/Semester 2/MABS/Assessments/Assignment 2/booking_regression_q2.csv"))

#summarizing dataset
head(booking)
summary(booking)
str(booking)

#calculating correlation
round(cor(booking[, c("number_of_weekend_nights", "number_of_week_nights", "lead_time", "average_price")]), digits = 2)

library(gridExtra)

p1 <- ggplot(booking, aes(x = average_price, y = lead_time)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(title = "Lead Time vs Average Price", x = "Average Price", y = "Lead Time") +
  theme_minimal()

p2 <- ggplot(booking, aes(x = number_of_week_nights, y = lead_time)) +
  geom_point(alpha = 0.4, color = "seagreen") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(title = "Lead Time vs Week Nights", x = "Week Nights", y = "Lead Time") +
  theme_minimal()

p3 <- ggplot(booking, aes(x = number_of_weekend_nights, y = lead_time)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(title = "Lead Time vs Weekend Nights", x = "Weekend Nights", y = "Lead Time") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 2)

#visualizing correlation with correlation matrix
library(ggplot2)
library(reshape2)
library(ggcorrplot)

# Select relevant numeric columns
selected_data <- booking[, c("number_of_weekend_nights", "number_of_week_nights", "lead_time", "average_price")]

# Compute correlation matrix
cor_matrix <- round(cor(selected_data, use = "complete.obs"), 2)

# Plot using ggcorrplot
ggcorrplot(cor_matrix, 
           method = "square", 
           lab = TRUE, 
           lab_size = 4, 
           colors = c("red", "white", "blue"),
           title = "Correlation Heatmap",
           ggtheme = theme_minimal())
#visualizing pairplot

install.packages("gpairs") 
library(gpairs) 

gpairs(selected_data)


#calculating regression (lead time by week.weekend.avg_price)

m1 <- lm(lead_time ~ number_of_week_nights + number_of_weekend_nights + average_price, data= booking)
summary(m1)


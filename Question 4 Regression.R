
#calculating correlation with car parking space
round(cor(booking[, c("number_of_weekend_nights", "number_of_week_nights", "lead_time", "average_price", "car_parking_space")]), digits = 2)

#visualizing correlation with correlation matrix
library(ggplot2)
library(reshape2)
library(ggcorrplot)

# Select relevant numeric columns
selected_data_2 <- booking[, c("number_of_weekend_nights", "number_of_week_nights", "lead_time", "average_price","car_parking_space")]

# Compute correlation matrix
cor_matrix_2 <- round(cor(selected_data_2, use = "complete.obs"), 2)

# Plot using ggcorrplot
ggcorrplot(cor_matrix_2, 
           method = "square", 
           lab = TRUE, 
           lab_size = 4, 
           colors = c("red", "white", "blue"),
           title = "Correlation Heatmap",
           ggtheme = theme_minimal())
#calculating regression (lead time by week.weekend.avg_price.parking)

m2 <- lm(lead_time ~ number_of_week_nights + number_of_weekend_nights + average_price+ car_parking_space, data= booking)
summary(m2)

#Introducing new variable : hotels with car parking

booking$has_car_parking <- factor(booking$car_parking_space > 0)

#calculating regression (lead time by week.weekend.avg_price.has_car_parking)

m3 <- lm(lead_time ~ number_of_week_nights + number_of_weekend_nights + average_price+ has_car_parking, data= booking)
summary(m3)

#adding interaction (car parking and average price)
m4 <- lm(lead_time ~ number_of_week_nights + number_of_weekend_nights + average_price+ has_car_parking+ average_price*has_car_parking , data= booking)
summary(m4)

#adding interaction (car parking and week nights)
m5 <- lm(lead_time ~ number_of_week_nights + number_of_weekend_nights + average_price+ has_car_parking+ (number_of_week_nights *has_car_parking) , data= booking)
summary(m5)

#adding interaction (car parking and weekend nights)
m6 <- lm(lead_time ~ number_of_week_nights + number_of_weekend_nights + average_price+ has_car_parking+ (number_of_weekend_nights *has_car_parking) , data= booking)
summary(m6)


summary(m1)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared
summary(m4)$r.squared
summary(m5)$r.squared
summary(m6)$r.squared


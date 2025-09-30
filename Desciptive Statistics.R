booking.df <-read.csv('E:\\UoB\\Semester 2\\MABS\\Assessments\\Assignment 2\\booking_regression_q2.csv')

summary(booking.df)

# Factorizing the categorical variables
booking.df$type_of_meal <- factor(booking.df$type_of_meal)
booking.df$room_type <- factor(booking.df$room_type)
booking.df$market_segment_type <- factor(booking.df$market_segment_type)
booking.df$booking_status <- factor(booking.df$booking_status)

summary(booking.df)

# Histogram + Density for number_of_adults
hist(booking.df$number_of_adults, 
     breaks = 30, 
     prob = TRUE, 
     main = "Histogram of Number of Adults", 
     xlab = "Number of Adults", 
     col = "lightblue")
lines(density(booking.df$number_of_adults, na.rm = TRUE), col = "red", lwd = 2)

# Boxplot for number_of_adults
boxplot(booking.df$number_of_adults,
        main = "Boxplot for Number of Adults",
        xlab = "Number of Adults",
        col = "lightgreen",
        horizontal = TRUE)

# Histogram + Density for number_of_children
hist(booking.df$number_of_children, 
     breaks = 30, 
     prob = TRUE, 
     main = "Histogram of Number of Children", 
     xlab = "Number of Children", 
     col = "lightblue")
lines(density(booking.df$number_of_children, na.rm = TRUE), col = "red", lwd = 2)

# Boxplot for number_of_children
boxplot(booking.df$number_of_children,
        main = "Boxplot for Number of Children",
        xlab = "Number of Children",
        col = "lightgreen",
        horizontal = TRUE)

# Histogram for no_of_week_nights
hist(booking.df$number_of_week_nights,
     breaks = 30,
     prob = TRUE,
     main = "Histogram of Weekday Stay",
     xlab = "Number of Weekday Stay",
     col = "lightblue")
lines(density(booking.df$number_of_week_nights, na.rm = TRUE), col = "red", lwd = 2)

# Boxplot
boxplot(booking.df$number_of_week_nights,
        main = "Boxplot for Weekday Stay",
        xlab = "Number of Nights",
        col = "lightgreen",
        horizontal = TRUE)
# Histogram for weekend nights
hist(booking.df$number_of_weekend_nights,
     breaks = 30,
     prob = TRUE,
     main = "Histogram of Weekend Stay",
     xlab = "Number of Weekend Nights",
     col = "lightblue")
lines(density(booking.df$number_of_weekend_nights, na.rm = TRUE), col = "red", lwd = 2)

# Boxplot
boxplot(booking.df$number_of_weekend_nights,
        main = "Boxplot of Weekend Stay",
        xlab = "Weekend Nights",
        col = "lightgreen",
        horizontal = TRUE)
# Histogram of average price
hist(booking.df$average_price,
     breaks = 30,
     prob = TRUE,
     main = "Histogram of Average Price",
     xlab = "Average Price",
     col = "lightblue")
lines(density(booking.df$average_price, na.rm = TRUE), col = "red", lwd = 2)

# Boxplot
boxplot(booking.df$average_price,
        main = "Boxplot of Average Price",
        xlab = "Average Price",
        col = "lightgreen",
        horizontal = TRUE)

library(ggplot2)

ggplot(booking.df, aes(x = market_segment_type)) +
  geom_bar(fill = "steelblue", color = "black") +  # Black border for bars
  labs(title = "Booking Count by Market Segment",
       x = "Market Segment",
       y = "Number of Bookings") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centered, bold title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),      # Slanted readable x labels
    axis.title = element_text(size = 12)
  )

#Histogram of lead time

library(ggplot2)


ggplot(booking.df, aes(x = lead_time)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Lead Time", x = "Lead Time (days)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot

ggplot(booking.df, aes(x = lead_time)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Lead Time", x = "Lead Time (days)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




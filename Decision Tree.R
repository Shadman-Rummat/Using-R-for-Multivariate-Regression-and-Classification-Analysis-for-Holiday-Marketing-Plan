booking.df <-read.csv('E:\\UoB\\Semester 2\\MABS\\Assessments\\Assignment 2\\booking_regression_q2.csv')

summary(booking.df)

# Factorizing the categorical variables
booking.df$type_of_meal <- factor(booking.df$type_of_meal)
booking.df$room_type <- factor(booking.df$room_type)
booking.df$market_segment_type <- factor(booking.df$market_segment_type)
booking.df$booking_status <- factor(booking.df$booking_status)

summary(booking.df)

#removing irrelevant columns from decision tree
booking.df <- booking.df[, !(names(booking.df) %in% c("Booking_ID", "date_of_reservation"))]

str(booking.df)

table(booking.df$booking_status)

#running the C5.0 model
library(C50)

# Build the C5.0 decision tree model
booking_model <- C5.0(booking_status ~ ., data = booking.df)

booking_model

# View a summary of the model
summary(booking_model)

#plot the model
plot(booking_model)

# limiting the number of cases per leaf node
booking_model2 <- C5.0(
  booking.df[, -which(names(booking.df) == "booking_status")],
  booking.df$booking_status,
  control = C5.0Control(minCases = 2000)
)

# Display basic info about the tree
booking_model2

# Display detailed summary of the tree
summary(booking_model2)

# Plot the simplified tree
plot(booking_model2)

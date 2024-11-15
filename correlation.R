# Load the dataset
horse_data <- read_csv("D:\\MSC\\TRD Original\\horse.csv")

# Rename columns
names(horse_data)[5] <- "pulse"
names(horse_data)[4] <- "rectal_temp"

# Filter out unrealistic values
df5 <- subset(horse_data, rectal_temp < 100 & pulse < 150)

# Check normality of the 'rectal_temp' data using a histogram
hist(df5$rectal_temp, prob = TRUE, 
     main = "Histogram of Rectal Temp with Normal Curve", 
     xlab = "Rectal Temp", col = "lightblue", border = "black")

# Overlay a normal curve on the histogram
x <- seq(min(df5$rectal_temp), max(df5$rectal_temp), length = 100)
curve_y <- dnorm(x, mean = mean(df5$rectal_temp), sd = sd(df5$rectal_temp))
lines(x, curve_y, col = "red")

# Perform Spearman correlation test
cor_test_result <- cor.test(df5$rectal_temp, df5$pulse, method = "spearman")
print(cor_test_result)

# Scatterplot with trend line (using base R)
plot(df5$rectal_temp, df5$pulse, 
     xlab = "Rectal Temp", ylab = "Pulse", 
     main = "Correlation Scatterplot of Rectal Temp vs. Pulse", 
     pch = 19, col = "blue")
abline(lm(df5$pulse ~ df5$rectal_temp), col = "red")

# Scatterplot with trend line (using ggplot2 for enhanced visualization)
ggplot(df5, aes(x = rectal_temp, y = pulse)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation Scatterplot of Rectal Temp vs. Pulse", 
       x = "Rectal Temp", 
       y = "Pulse")
# Load necessary library
library(readr)

# Load the dataset
horse_data <- read_csv("C:\\Users\\maali\\OneDrive\\Documents\\Main_Coursework\\horse.csv")

# Rename columns
names(horse_data)[4] <- "rectal_temp"
names(horse_data)[5] <- "pulse"

# Filter out unrealistic values
df5 <- subset(horse_data, rectal_temp < 100 & pulse < 150)

# Create a binary grouping variable based on rectal_temp threshold
df5$rectal_temp_group <- ifelse(df5$rectal_temp < 38, "Low Temp", "High Temp")

# Create a binary condition for pulse > 80
df5$high_pulse <- ifelse(df5$pulse > 80, 1, 0)

# Create a contingency table of counts for the two variables
contingency_table <- table(df5$high_pulse, df5$rectal_temp_group)
print("Contingency Table:")
print(contingency_table)

# Perform a chi-squared test of independence
chi_squared_result <- chisq.test(contingency_table)
print("Chi-squared Test Result:")
print(chi_squared_result)

# Perform a proportion test (z-test for proportions)
prop_test_result <- prop.test(contingency_table)
print("Proportion Test Result:")
print(prop_test_result)

# Visualize the proportions with a bar plot
proportions <- prop.table(contingency_table, margin = 2)  # Calculate proportions by group
barplot(proportions, beside = TRUE, col = c("lightblue", "lightgreen"),
        legend = rownames(proportions), 
        main = "Proportion of High Pulse Between Rectal Temperature",
        xlab = "Rectal Temperature", ylab = "Proportion")
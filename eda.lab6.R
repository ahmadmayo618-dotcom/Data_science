# ------------------------------------------
# STEP 1: Create Hypothetical Dataset (550 rows, 7 columns)
# ------------------------------------------

set.seed(123)  # for reproducibility

data <- data.frame(
  ID = 1:550,
  Age = sample(18:60, 550, replace = TRUE),
  Gender = sample(c("Male", "Female"), 550, replace = TRUE),
  Income = round(rnorm(550, mean = 50000, sd = 15000), 2),
  Score = sample(1:100, 550, replace = TRUE),
  City = sample(c("Lahore", "Karachi", "Islamabad", "Faisalabad"), 550, replace = TRUE),
  Purchased = sample(c("Yes", "No"), 550, replace = TRUE)
)

head(data)

# ------------------------------------------
# STEP 2: Data Cleaning
# ------------------------------------------

# Check missing values
sum(is.na(data))



# Handle missing values (replace NA with mean or mode)
data$Income[is.na(data$Income)] <- mean(data$Income, na.rm = TRUE)
data$Age[is.na(data$Age)] <- round(mean(data$Age, na.rm = TRUE))

# Remove duplicates (if any)
data <- data[!duplicated(data), ]

# Convert categorical columns to factors
data$Gender <- as.factor(data$Gender)
data$City <- as.factor(data$City)
data$Purchased <- as.factor(data$Purchased)
# ------------------------------------------
# STEP 3: Data Transformation
# ------------------------------------------

# Normalize numeric columns (e.g., Age, Income, Score)
data

# Create new derived column (e.g., Income per Age)
data$Income_per_Age <- round(data$Income / data$Age, 2)

# ------------------------------------------
# STEP 4: Exploratory Data Analysis (EDA)
# ------------------------------------------

library(ggplot2)
library(dplyr)
# Assume data already created (from your previous code)
# and ggplot2 + dplyr libraries loaded

library(ggplot2)
library(dplyr)

# 1️⃣ Numerical vs Numerical  (Scatter Plot)
ggplot(data, aes(x = Income, y = Score)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  ggtitle("Income vs Score (Bivariate Relationship)")

# 2️⃣ Categorical vs Numerical  (Box Plot)
ggplot(data, aes(x = Gender, y = Income, fill = Gender)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Income Distribution by Gender")

# 3️⃣ Categorical vs Categorical  (Bar Plot)
ggplot(data, aes(x = City, fill = Purchased)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  ggtitle("Purchased Status by City")

# 4️⃣ Correlation (Numeric Features Only)
num_data <- data %>% select(Age, Income, Score)
cor(num_data)

# Optional: visualize with corrplot if available
library(corrplot)
corrplot(cor(num_data), method = "number", tl.col = "black")

# 5️⃣ Grouped Summary (mean Income by City)
data %>%
  group_by(City) %>%
  summarise(Average_Income = mean(Income, na.rm = TRUE),
            Average_Score = mean(Score, na.rm = TRUE))


# Summary statistics
summary(data)

# 1️⃣ Histogram of Age
ggplot(data, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  theme_minimal() +
  ggtitle("Distribution of Age")

# 2️⃣ Income vs Score Scatter Plot
ggplot(data, aes(x = Income, y = Score, color = Gender)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  ggtitle("Income vs Score by Gender")

# 3️⃣ Bar plot of Purchased vs City
ggplot(data, aes(x = City, fill = Purchased)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  ggtitle("Purchased Distribution by City")
# Assume: data already created from your previous code
library(ggplot2)
library(dplyr)

# -----------------------------------------
# 1️⃣ Numerical Variables Analysis
# -----------------------------------------

# Summary statistics
summary(select(data, Age, Income, Score))

# Histogram for Age
ggplot(data, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  theme_minimal() +
  ggtitle("Age Distribution")

# Histogram for Income
ggplot(data, aes(x = Income)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 25) +
  theme_minimal() +
  ggtitle("Income Distribution")

# Boxplot for Score
ggplot(data, aes(y = Score)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  ggtitle("Score Summary (Detect Outliers)")

# -----------------------------------------
# 2️⃣ Categorical Variables Analysis
# -----------------------------------------

# Count of Gender
table(data$Gender)

# Bar chart for Gender
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Gender Count")

# Bar chart for City
ggplot(data, aes(x = City, fill = City)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("City Distribution")

# Bar chart for Purchased
ggplot(data, aes(x = Purchased, fill = Purchased)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Purchased Count")

# -----------------------------------------
# 3️⃣ Outlier Detection (numerical)
# -----------------------------------------

# Using boxplot stats
boxplot(data$Income, main = "Income Outliers Check", col = "lightblue")
boxplot.stats(data$Income)$out  # shows outlier values
# Assume dataset 'data' already created
library(ggplot2)
library(dplyr)

# -----------------------------------------
# 1️⃣ 3 Variables — Scatter Plot with Color and Size
# -----------------------------------------
ggplot(data, aes(x = Age, y = Income, color = Gender, size = Score)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  ggtitle("Age vs Income colored by Gender & sized by Score")

# -----------------------------------------
# 2️⃣ Facet Plot — Multiple Graphs for Each City
# -----------------------------------------
ggplot(data, aes(x = Age, y = Income, color = Purchased)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ City) +
  theme_minimal() +
  ggtitle("Age vs Income across Different Cities")

# -----------------------------------------







# ------------------------------------------
# STEP 5: Clean Summary
# ------------------------------------------

cat("✅ Cleaning Done\n")
cat("✅ EDA Completed\n")

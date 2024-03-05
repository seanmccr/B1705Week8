# ----- B1705 Week 8 | Machine Learning: Workflow | 05.03.2024 -----
# ----- Pre-lecture Work -----



# ----- Lecture Work ------
# ----- 1. Workflow Introduction & Handling Missing Values -----
##### Removal: Demonstration -----
# Sample dataset of athlete performances
athlete_data <- data.frame(
  athlete_id = 1:10,
  performance_score = c(25, NA, 30, 32, NA, 27, 29, NA, 31, 33),
  age = c(24, 25, 22, NA, 23, NA, 27, 26, 28, 29)
)

# Display original dataset
print(athlete_data)

# Remove records with any missing values
cleaned_data <- na.omit(athlete_data)

# Display cleaned dataset
head(cleaned_data)

rm(cleaned_data)

##### Imputation: Demonstration -----
set.seed(123)

# Generate synthetic data
runner_id <- 1:100
finish_time <- rnorm(100, mean = 240, sd = 20)

# Introduce missing values
sample_indices <- sample(1:100, 20)
finish_time[sample_indices] <- NA

# Combine into a data frame
marathon_data <- data.frame(runner_id, finish_time)

# Calculate the median of the available finish times
median_time <- median(marathon_data$finish_time, na.rm = TRUE)

# Impute missing finish times with the median value
marathon_data$finish_time[is.na(marathon_data$finish_time)] <- median_time

# Verify imputation
head(marathon_data)

##### Prediction: Demonstration -----

set.seed(123)

# Generate synthetic data for cricket players
player_id <- 1:100  # 100 players
batting_average <- round(rnorm(100, mean = 35, sd = 5),1)
bowling_average <- round(rnorm(100, mean = 25, sd = 3),1)
sample_indices <- sample(1:100, 20)
bowling_average[sample_indices] <- NA 

# Combine
cricket_data <- data.frame(player_id, batting_average, bowling_average)

# Split data into sets with known and unknown bowling averages
known_bowling <- cricket_data[!is.na(cricket_data$bowling_average), ]
unknown_bowling <- cricket_data[is.na(cricket_data$bowling_average), ]

# Build model to predict bowling_average using batting_average from the known dataset
model <- lm(bowling_average ~ batting_average, data = known_bowling)

# Predict the missing averages
predictions <- predict(model, newdata = unknown_bowling)

head(cricket_data)
rm(model, unknown_bowling, known_bowling)

# ----- 2. Outlier Detection -----
##### Identifying Outliers: Demonstration -----
set.seed(123)
swimming_data <- data.frame(
  athlete_id = 1:300,
  swimming_time = c(rnorm(290, mean = 50, sd = 2), runif(10, min = 30, max = 70)) 
)

swimming_data$swimming_time <- round(swimming_data$swimming_time,1)

# visually inspect outliers
boxplot(swimming_data$swimming_time, main = "Boxplot of Swimming Times",
        ylab = "Swimming Time (seconds)", col = "lightblue")

# using IQR method
Q1 <- quantile(swimming_data$swimming_time, 0.25)
Q3 <- quantile(swimming_data$swimming_time, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- subset(swimming_data, swimming_time < lower_bound | swimming_time > upper_bound)

# Display outliers
print("Identified Outliers:")

print(outliers)

##### Dealing with Outliers: Demonstration (1) -----

# Here’s a reminder of some common strategies:
  
# Exclusion: Removing outliers if they are determined to be due to data entry errors or if they are not relevant to the specific analysis.
# Transformation: Applying a mathematical transformation (such as log transformation) to reduce the impact of outliers.
# Imputation: Replacing outliers with estimated values based on other data points.
# Separate Analysis: Conducting analyses with and without outliers to understand their impact.

##### Dealing with Outliers: Demonstration (2) -----
# olympic_swimming_data with identified outliers

# Analysing the impact of outliers
summary(swimming_data$swimming_time)

hist(swimming_data$swimming_time, main = "Histogram of Swimming Times", xlab = "Time", breaks = 20, col = "gray")

# I'll exclude the outliers
cleaned_data <- swimming_data[!(swimming_data$swimming_time < lower_bound | swimming_data$swimming_time > upper_bound), ]

# Re-analyse without outliers
summary(cleaned_data$swimming_time)

hist(cleaned_data$swimming_time, main = "Histogram of Swimming Times (Without Outliers)", xlab = "Time", breaks = 20, col = "lightblue")

# Compare results
boxplot(swimming_data$swimming_time, cleaned_data$swimming_time,
        names = c("Original", "Cleaned"),
        main = "Comparative Boxplot: Original vs. Cleaned Data",
        ylab = "Swimming Time (seconds)")

# Checking distribution
hist(athlete_data$performance_score, main = "Histogram of Performance Scores", xlab = "Score", breaks = 20, col = "darkgreen")

# Applying log transformation to reduce skewness caused by outliers
athlete_data$transformed_score <- log(athlete_data$performance_score)

# Visualise transformed data
hist(athlete_data$transformed_score, main = "Histogram of Log-Transformed Performance Scores", xlab = "Log(Score)", breaks = 20, col = "lightgreen")

# Show how transformation has affected distribution and impact of outliers
boxplot(athlete_data$performance_score, athlete_data$transformed_score,
        names = c("Original", "Log-Transformed"),
        main = "Boxplot: Original vs. Log-Transformed Data",
        ylab = "Performance Score")

rm(outliers)

# ----- 3. Data Normalisation -----
##### Normalisation: Demonstration -----
# Sample basketball players' height and weight
set.seed(123)
player_ids <- 1:50
heights_cm <- round(rnorm(50, mean = 200, sd = 10),1)  # Heights centimeters
weights_kg <- round(rnorm(50, mean = 100, sd = 15),1)  # Weights kilograms

basketball_data <- data.frame(player_ids, heights_cm, weights_kg)

# Min-max normalisation function
min_max_normalisation <- function(x) {
  return ((x -min(x)) / (max(x) - min(x)))
}

# Apply normalisation to height and weight
basketball_data$norm_heights <- min_max_normalisation(basketball_data$heights_cm)
basketball_data$norm_weights <- min_max_normalisation(basketball_data$weights_kg)

# View normalised data
head(basketball_data)

# ----- 4. Data Standardisation -----
##### Standardisation: Demonstration -----
library(dplyr)
library(reshape2)
set.seed(123)

# Generating synthetic data for swimmers' performance
swimmers_data <- data.frame(
  swimmer_id = 1:10,
  freestyle_100m_time = runif(10, 40, 55),  # Time in seconds
  butterfly_200m_time = runif(10, 110, 130),  # Time in seconds
  backstroke_50m_time = runif(10, 25, 35)  # Time in seconds
)
head(swimmers_data)

# Standardising data
standardised_data <- as.data.frame(scale(swimmers_data[, -1]))  # Excluding swimmer_id for standardisation

# Adding swimmer_id back to the standardised dataset
standardised_data$swimmer_id <- swimmers_data$swimmer_id

# Rearranging columns
standardised_data <- standardised_data %>% select(swimmer_id, everything())

# Display standardised data
head(standardised_data)

# Visualisation
library(ggplot2)
melted_data <- melt(standardised_data, id.vars = 'swimmer_id')
ggplot(melted_data, aes(x = variable, y = value, fill = swimmer_id)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Standardised Swimming Performance Metrics", x = "Event", y = "Standardised Time") +
  theme_minimal()

# ----- Dealing with Imbalanced Data -----
##### Theory -----
# Loading Libraries
library(caret)
library(DMwR2)

set.seed(123)
# Generate synthetic data
num_athletes <- 200
speed <- rnorm(num_athletes, mean = 25, sd = 5)  # Speed in km/h
agility <- rnorm(num_athletes, mean = 30, sd = 7)  # Agility measurement
injury <- c(rep(0, 190), rep(1, 10))  # Binary injury status, imbalanced

athletes_data <- data.frame(speed, agility, injury)

# View imbalance in the dataset
table(athletes_data$injury)

##### Synthetic Data Generation -----
# Apply SMOTE to balance the data
set.seed(123)
library(performanceEstimation)
athletes_data_smote <- smote(injury ~ speed + agility, data = athletes_data, perc.over = 2, k = 5, perc.under=2)

# View the new balance
table(athletes_data_smote$injury)

##### Re-Sampling -----
# Separate majority and minority classes
data_majority <- athletes_data[athletes_data$injury == 0, ]
data_minority <- athletes_data[athletes_data$injury == 1, ]

# Upsample minority class
data_minority_upsampled <- data_minority[sample(nrow(data_minority), 190, replace = TRUE), ]
data_balanced <- rbind(data_majority, data_minority_upsampled)

# View the new balance
table(data_balanced$injury)











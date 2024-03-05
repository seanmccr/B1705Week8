# ----- B1705 Week 8 | Machine Learning: Algorithms | 05.03.2024 -----
# ----- Pre-lecture Work -----
##### Linear Regression -----

model <- lm(mpg ~ wt + hp, data = mtcars)
summary(model)

##### K-nearest Neighbours -----
library(class)
data(iris)
train <- sample(1:150, 100)
knn.model <- knn(train = iris[train, 1:4], test = iris[-train, 1:4], cl = iris[train, 5], k = 3)
table(iris[-train, 5], knn.model)

##### Decision Trees -----
library(rpart)
model <- rpart(Species ~ ., data = iris, method = "class")
printcp(model)  # Display the results

# ----- Evaluating ML Models -----
##### Confusion Matrix' -----
library(caret)
confusionMatrix(data = knn.model, reference = iris[-train, 5])

# ----- Lecture Work -----
# Before continuing, read relevant theory sections for each of the following sectioons on Myplace (73 - ML: Algorithsm (Practical))

##### Linear Regression: Example -----
# Set the seed for reproducibility
set.seed(123)

# Generate a synthetic dataset
hours_studied <- 1:100
exam_score <- 50 + 0.75 * hours_studied + rnorm(100, mean = 0, sd = 10)

# Combine into a data frame
study_data <- data.frame(hours_studied, exam_score)

# Perform linear regression
model <- lm(exam_score ~ hours_studied, data = study_data)

# Create a scatter plot of the data and the regression line
library(ggplot2)
ggplot(study_data, aes(x = hours_studied, y = exam_score)) +
  geom_point() +  # Plot the data points
  geom_smooth(method = "lm", col = "blue") +  
  theme_minimal() +
  labs(title = "Linear Regression: Exam Score vs. Hours Studied",
       x = "Hours Studied",
       y = "Exam Score") +
  theme(plot.title = element_text(hjust = 0.5)) 

rm(model)

##### Logistic Regression: Example -----
# Set the seed for reproducibility
set.seed(123)

# Generate a synthetic dataset
num_matches <- 100
goals_scored <- rpois(num_matches, lambda = 2)
points_total <- rnorm(num_matches, mean = 30, sd = 10)

# Create a binary outcome with association with goals_scored and points_total
# Increase coefficients for the variables in the logistic function
win_loss <- ifelse(runif(num_matches) < plogis(0.95 * goals_scored + 0.62 * points_total - 15), 1, 0)

# Combine into a data frame
match_data <- data.frame(goals_scored, points_total, win_loss)

# Perform logistic regression
model <- glm(win_loss ~ goals_scored + points_total, data = match_data, family = "binomial")

# Summary of the logistic regression model
summary(model)

# Visualisation
library(ggplot2)

# Create a new data frame for predictions
goals_range <- seq(min(goals_scored), max(goals_scored), length.out = 100)
points_avg <- mean(points_total)
prediction_data <- expand.grid(goals_scored = goals_range, points_total = points_avg)

# Add predictions
prediction_data$win_prob <- predict(model, newdata = prediction_data, type = "response")

# Plotting
ggplot(prediction_data, aes(x = goals_scored, y = win_prob)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Probability of Winning vs. Goals Scored",
       x = "Goals Scored",
       y = "Predicted Probability of Winning") +
  theme_minimal()

##### K-nearest Neighbours: Example  -----
# Load libraries
library(caret)
library(ggplot2)
library(class)

# Set the seed for reproducibility
set.seed(123)

# Generate synthetic
num_athletes <- 200
sport <- factor(sample(c('Basketball', 'Soccer', 'Swimming', 'Running'), num_athletes, replace = TRUE))
height <- rnorm(num_athletes, mean = ifelse(sport == 'Basketball', 190, ifelse(sport == 'Soccer', 175, ifelse(sport == 'Swimming', 180, 170))), sd = 5)
weight <- rnorm(num_athletes, mean = ifelse(sport == 'Basketball', 85, ifelse(sport == 'Soccer', 70, ifelse(sport == 'Swimming', 75, 65))), sd = 5)

# Combine into a data frame
athletes_data <- data.frame(sport, height, weight)

# Visualise
ggplot(athletes_data, aes(x = height, y = weight, color = sport)) + geom_point() +
  labs(title = "Observed Physical Attributes by Sport", x = "Height (cm)", y = "Weight (kg)") +
  theme_minimal()

# Split data into training and testing sets
set.seed(123)
training_index <- createDataPartition(athletes_data$sport, p = .8, list = FALSE)
training <- athletes_data[training_index, ]
testing <- athletes_data[-training_index, ]

# Normalise data
preproc <- preProcess(training[, -1])
training_norm <- predict(preproc, training)
testing_norm <- predict(preproc, testing)

# Train KNN model
set.seed(123)
knn_model <- knn(train = training_norm[, -1], test = testing_norm[, -1], cl = training_norm$sport, k = 5)

# Visualise classification
testing_norm$predicted_sport <- knn_model
ggplot(testing_norm, aes(x = height, y = weight, color = predicted_sport)) + geom_point() +
  labs(title = "Predicted Sport by Weight and Height", x = "Height (cm)", y = "Weight (kg)") +
  theme_minimal()

# --- --- --- --- ---
## look at some model metrics

# Actual vs. Predicted
actual_sports <- testing_norm$sport
predicted_sports <- knn_model

# Confusion Matrix
confusion_matrix <- table(Predicted = predicted_sports, Actual = actual_sports)
print("Confusion Matrix:")
print(confusion_matrix)

# Accuracy Calculation
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Install and load caret if not already installed/loaded for additional metrics
library(caret)

# Other performance metrics
performance_metrics <- confusionMatrix(data = as.factor(predicted_sports), reference = as.factor(actual_sports))
print(performance_metrics)

##### Decision Trees: Example  -----
library(tidyverse)
# Set the seed for reproducibility
set.seed(123)
# Generate data
num_teams <- 100
avg_player_height <- rnorm(num_teams, mean = 200, sd = 10)
avg_player_experience <- rnorm(num_teams, mean = 5, sd = 2)
games_won <- rpois(num_teams, lambda = 30)

success <- as.factor(ifelse(avg_player_height > 195 & games_won > 20 & avg_player_experience > 4.5, "Yes", "No"))

# Combine into a dataframe
train_data <- data.frame(success, avg_player_height, avg_player_experience, games_won)

# Visualise data
ggplot(train_data, aes(x = avg_player_height, y = avg_player_experience, color = success)) +
  geom_point() +
  labs(title = "Basketball Success Factors",
       x = "Average Player Height (cm)",
       y = "Average Player Experience (years)") +
  theme_minimal()

# Load libraries
library(rpart)
library(rpart.plot)

# Create model
tree_model <- rpart(success ~ avg_player_height + avg_player_experience + games_won,
                    data = train_data, method = "class")

# Assuming avg_player_height and avg_player_experience are your intended variables and they are correctly named in train_data
tree_model <- rpart(success ~ avg_player_height + avg_player_experience + games_won,
                    data = train_data, method = "class")

# Visualize the decision tree
rpart.plot(tree_model, main = "Decision Tree for Basketball Team Success", extra = 104)

##### Neural Networks: Example (1) -----
library(neuralnet)

# Set the seed for reproducibility
set.seed(123)

# Creating synthetic dataset
num_countries <- 10
GDP_per_capita <- runif(num_countries, 1000, 50000) 
population_millions <- runif(num_countries, 1, 50)
avg_training_hours <- runif(num_countries, 10, 15)
medals_won <- round(0.001 * GDP_per_capita + 0.02 * population_millions + 0.5 * avg_training_hours + rnorm(num_countries, 0, 2))

olympic_data <- data.frame(GDP_per_capita, population_millions, avg_training_hours, medals_won)

# Normalise data for performance
maxs <- apply(olympic_data, 2, max)
mins <- apply(olympic_data, 2, min)
scaled_olympic_data <- as.data.frame(scale(olympic_data, center = mins, scale = maxs - mins))

# Define and train simple neural network with one hidden layer with two neurons
set.seed(123)
nn_model <- neuralnet(medals_won ~ GDP_per_capita + population_millions + avg_training_hours, 
                      data = scaled_olympic_data, 
                      hidden = 2,  # Two neurons in the hidden layer
                      linear.output = TRUE,
                      threshold = 0.01)

# Visualise neural network
plot(nn_model)

##### Neural Networks: Example (2) -----
# Set the seed for reproducibility
set.seed(123)

# Creating a synthetic dataset
num_countries <- 50
GDP_per_capita <- runif(num_countries, 500, 80000)
population_millions <- runif(num_countries, 0.5, 300) 
avg_training_hours <- runif(num_countries, 5, 20) 
medals_won <- round(0.0001 * GDP_per_capita + 0.05 * population_millions + 0.3 * avg_training_hours + rnorm(num_countries, 0, 5))

olympic_data <- data.frame(GDP_per_capita, population_millions, avg_training_hours, medals_won)

# load necessary package
library(neuralnet)

# Normalise data
maxs <- apply(olympic_data, 2, max)
mins <- apply(olympic_data, 2, min)
scaled_olympic_data <- as.data.frame(scale(olympic_data, center = mins, scale = maxs - mins))

# Train neural network
set.seed(123)
nn_model <- neuralnet(medals_won ~ GDP_per_capita + population_millions + avg_training_hours, data = scaled_olympic_data, hidden = c(5,3), linear.output = TRUE)

# Visualise  neural network
plot(nn_model)


##### Support Vector Machines: Example -----
# Set the seed for reproducibility
# Load library
# Load necessary library
library(e1071)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data for wrestlers
num_wrestlers <- 200
body_fat_percentage <- runif(num_wrestlers, 10, 30)  # Body fat percentage
muscle_mass_kg <- runif(num_wrestlers, 40, 120)  # Muscle mass in kg

# Define weight categories based on simple rules and random variation
# Ensure weight_category is a factor
weight_category <- factor(ifelse(body_fat_percentage < 20 & muscle_mass_kg > 80, "Heavyweight",
                                 ifelse(body_fat_percentage >= 20 & muscle_mass_kg <= 80, "Lightweight", "Middleweight")))

wrestlers_data <- data.frame(body_fat_percentage, muscle_mass_kg, weight_category)

# Check for NAs in your dataset
sum(is.na(wrestlers_data))

# Visualize the data
library(ggplot2)
ggplot(wrestlers_data, aes(x = body_fat_percentage, y = muscle_mass_kg, color = weight_category)) + geom_point() +
  labs(title = "Wrestlers' Body Measurements and Weight Categories", x = "Body Fat Percentage", y = "Muscle Mass (kg)") +
  theme_minimal()

# Splitting the dataset into training and testing sets
indices <- sample(1:nrow(wrestlers_data), size = 0.7 * nrow(wrestlers_data))
train_data <- wrestlers_data[indices, ]
test_data <- wrestlers_data[-indices, ]

# Train the SVM model
svm_model <- svm(weight_category ~ ., data = train_data, type = 'C-classification', kernel = 'linear')

# Predict using the SVM model
predictions <- predict(svm_model, test_data)

# Visualize the classification results
test_data$predicted_category <- predictions
ggplot(test_data, aes(x = body_fat_percentage, y = muscle_mass_kg, color = predicted_category)) + geom_point() +
  labs(title = "SVM Predicted Classification Results", x = "Body Fat Percentage", y = "Muscle Mass (kg)") +
  theme_minimal()












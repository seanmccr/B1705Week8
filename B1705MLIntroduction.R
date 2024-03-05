# ----- B1705 Week 8 | Machine Learning: Introduction | 05.03.2024 -----
# ----- Pre-lecture Work -----

##### Supervised Learning -----
# Supervised learning example with linear regression
library(caret)
library(tidyverse)

data("mtcars")
model <- lm(mpg ~ wt + cyl, data = mtcars)
summary(model)

##### Unsupervised Learning -----
# Unsupervised learning example with clustering
library(cluster)
data <- scale(mtcars)  # Normalizing data
fit <- kmeans(data, 3)  # K-means clustering with 3 clusters
#print(fit$cluster)

mtcars$cluster <-fit$cluster

mtcars$cluster <- as.factor(mtcars$cluster)

# Create a scatter plot using ggplot
ggplot(mtcars, aes(x = wt, y = cyl, color = factor(cluster))) +
  geom_point() +  # Add points
  labs(color = "Cluster", x = "Speed", y = "Endurance") + # Labeling
  theme_minimal() +  # Minimal theme
  scale_color_brewer(palette = "Set1")  # Use a color palette for better distinction

# ----- Lecture Work ------

# ----- Supervised Learning -----
##### Regression: Demonstration -----

# create synthetic dataset
library(tidyverse)

# Set the seed for reproducibility
set.seed(123)

player_stats <- tibble(
  player_id = 1:100, # id
  award = sample(c("MVP", "NoAward"), 100, replace = TRUE, prob = c(0.1, 0.9)),
  points_per_game = rnorm(100, mean = 20, sd = 5), # Normally distributed points
  assists = rnorm(100, mean = 5, sd = 2),
  rebounds = rnorm(100, mean = 7, sd = 3)
)

# Create a dependent variable 'performance_score' that's a linear combination of the IVs
player_stats <- player_stats %>%
  mutate(performance_score = 50 + 2 * points_per_game + 3 * assists + 4 * rebounds + rnorm(100, mean = 0, sd = 5))

##### Part 2 -----

# Fit regression model
model <- lm(performance_score ~ points_per_game + assists + rebounds, data = player_stats)

# Summarise  model
summary(model)
rm(model)

# Load the necessary library
library(caret)

# Split data into training and testing sets
set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(player_stats$performance_score, p = .8, 
                                  list = FALSE, 
                                  times = 1)

trainData <- player_stats[trainIndex, ]
testData <- player_stats[-trainIndex, ]

# Fit linear regression model on training data
model <- lm(performance_score ~ points_per_game + assists + rebounds, data = trainData)

# Summarise model
summary(model)


# Predict on testing data
predictions <- predict(model, newdata = testData)

# Evaluate model performance
mse <- mean((predictions - testData$performance_score)^2)
print(mse)

# Clean environment
rm(model, testData, trainData, trainIndex)

##### Regression: Practice -----
team_stats <- read.csv('https://www.dropbox.com/scl/fi/lh1xy2jx747375tex03w3/team_stats.csv?rlkey=cgpbitn33bqazei4uwose12uk&dl=1')

# Fit the linear regression model
model <- lm(wins ~ points_scored + points_allowed + total_rebounds + average_age + efficiency, data = team_stats)

# Model Summary
summary(model)
rm(model)

# Load the necessary library
library(caret)

# Split the data into training and testing sets
set.seed(123) 
trainIndex <- createDataPartition(team_stats$wins, p = .8, 
                                  list = FALSE, 
                                  times = 1)

trainData <- team_stats[trainIndex, ]
testData <- team_stats[-trainIndex, ]

# Fit linear regression model on the training data
model <- lm(wins ~ points_scored + points_allowed + total_rebounds + average_age + efficiency, data = team_stats)

summary(model)

# Predictions coding
predictions <- predict(model, newdata = testData)

mse <- mean((predictions - testData$wins)^2)
print(mse)

rm(model, testData, trainData, trainIndex)

# ----- Unsupervised Learning -----

##### K-Means Clustered: Demonstration -----
# Load library
library(stats)

## Create dataset
# Num teams
num_teams <- 100

# Initialise vectors
goals_scored <- c()
goals_conceded <- c()
possession <- c()

# Generate four distinct clusters
# Cluster 1
goals_scored <- c(goals_scored, rpois(num_teams / 4, lambda = 0.8))
goals_conceded <- c(goals_conceded, rpois(num_teams / 4, lambda = 2))
possession <- c(possession, runif(num_teams / 4, min = 45, max = 50))

# Cluster 2
goals_scored <- c(goals_scored, rpois(num_teams / 4, lambda = 2))
goals_conceded <- c(goals_conceded, rpois(num_teams / 4, lambda = 0.8))
possession <- c(possession, runif(num_teams / 4, min = 55, max = 60))

# Cluster 3
goals_scored <- c(goals_scored, rpois(num_teams / 4, lambda = 1.5))
goals_conceded <- c(goals_conceded, rpois(num_teams / 4, lambda = 1.5))
possession <- c(possession, runif(num_teams / 4, min = 50, max = 55))

# Cluster 4
goals_scored <- c(goals_scored, rpois(num_teams / 4, lambda = 3))
goals_conceded <- c(goals_conceded, rpois(num_teams / 4, lambda = 1))
possession <- c(possession, runif(num_teams / 4, min = 40, max = 45))

# Combine into data frame
football_data <- data.frame(goals_scored, goals_conceded, possession)


# Select relevant columns for clustering 
selected_columns <- football_data[, c("goals_scored", "goals_conceded", "possession")]

# Perform k-means clustering
set.seed(123)
clusters <- kmeans(selected_columns, centers = 4)  # Choose number of clusters

# Attach cluster results to data
football_data$cluster <- clusters$cluster

# Examine clusters
head(football_data)

# Library Load and Plot
library(ggplot2)

ggplot(football_data, aes(x = goals_scored, y = goals_conceded, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "Cluster of Teams based on Goals Scored and Possession",
       x = "Goals Scored",
       y = "Possession",
       color = "Cluster") +
  theme_minimal()

##### Hierarchical Cluster Analysis: Demonstration -----
# Load  library
library(stats)

# Define the number of players
num_players <- 100

# Generate synthetic data
serve_accuracy <- runif(num_players, 50, 90)  # Serve accuracy between 50% to 90%
return_points_won <- runif(num_players, 20, 70)  # Return points won between 20% to 70%
breakpoints_saved <- runif(num_players, 10, 80)  # Breakpoints saved between 10% to 80%

# Create a data frame
tennis_data <- data.frame(serve_accuracy, return_points_won, breakpoints_saved)

##____________________

# Select relevant columns for clustering
selected_columns <- tennis_data[, c("serve_accuracy", "return_points_won", "breakpoints_saved")]

# Perform hierarchical clustering
distances <- dist(selected_columns)  # Calculate distances between players
hc <- hclust(distances)  # Perform hierarchical clustering

# Plot the dendrogram
plot(hc)


# ----- Reinforcement Learning: Demonstration -----
##### Define Parameters -----
# load package
rm(list=ls())
library(hash)

# Initialise parameters
alpha <- 0.1  # Learning rate
gamma <- 0.9  # Discount factor
epsilon <- 0.1  # Exploration rate
num_episodes <- 1000  # Number of episodes for training

##### Initialise Q-Table -----
# Initialise Q-table
Q <- hash()
for (state in 1:4) {
  for (action in 1:2) {
    .set(Q, paste(state, action, sep="-"), 0)
  }
}

##### Defining Environment -----
# Define environment
# States are: 1 = Start, 2 = Neutral, 3 = Neutral, 4 = Goal
# Actions are: 1 = move forward, 2 = move backward (in this simple example, backward movement has no effect in state 1)

get_next_state <- function(current_state, action) {
  if (action == 1) {
    return(min(current_state + 1, 4))
  } else {
    return(max(current_state - 1, 1))
  }
}

get_reward <- function(current_state, action, next_state) {
  if (next_state == 4) {
    return(1)  # Reward for reaching the goal
  } else {
    return(0)  # No reward for other transitions
  }
}

##### Implement Algorithm -----
# Implement Q-learning

for (episode in 1:num_episodes) {
  current_state <- 1  # Start at the beginning of the environment
  while (current_state != 4) {  # Continue until goal state is reached
    # Select action
    if (runif(1) < epsilon) {
      action <- sample(1:2, 1)  # Explore
    } else {
      # Exploit: choose the best action based on current Q-values
      forward_value <- Q[[paste(current_state, 1, sep="-")]]
      backward_value <- Q[[paste(current_state, 2, sep="-")]]
      if (forward_value >= backward_value) {
        action <- 1
      } else {
        action <- 2
      }
    }
    
    # Take action and observe outcome
    next_state <- get_next_state(current_state, action)
    reward <- get_reward(current_state, action, next_state)
    
    # Q-learning update
    old_value <- Q[[paste(current_state, action, sep="-")]]
    next_max <- max(Q[[paste(next_state, 1, sep="-")]], Q[[paste(next_state, 2, sep="-")]])
    new_value <- (1 - alpha) * old_value + alpha * (reward + gamma * next_max)
    .set(Q, paste(current_state, action, sep="-"), new_value)
    
    current_state <- next_state
  }
}

##### Report Values -----
# Display the learned Q-values
Q

# Here’s some more commentary on that output:
  
#  1-1 : 0.81

# When the agent is in state 1 and takes action 1, the expected return is 0.81.
# 1-2 : 0.728

# In state 1, if the agent takes action 2, the expected return is approximately 0.728.
# 2-1 : 0.9

# From state 2, taking action 1 yields the highest expected return so far, 0.9.
# 2-2 : 0.728

# In state 2, action 2 has an expected return of about 0.726.
# 3-1 : 1

# This indicates that when the agent is in state 3 and takes action 1, it achieves the maximum possible return of 1. This could indicate reaching the goal or receiving a maximum reward in this state-action scenario.
# 3-2 : 0.802

# In state 3, taking action 2 results in an expected return of approximately 0.801.
# 4-1 : 0

# For state 4, taking action 1 gives a return of 0. This could suggest that action 1 in state 4 is not beneficial or possibly that it leads to a terminal state with no further rewards.
# 4-2 : 0

# Similarly, action 2 in state 4 also results in a return of 0, indicating no expected reward from this state-action pair, which might be another indication of a terminal or non-rewarding state.






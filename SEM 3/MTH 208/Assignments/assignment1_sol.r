# ===============================
# Problem 1: Rainfall Analysis in Kanpur
# ===============================

# Read the CSV file containing rainfall data
rain_data <- read.csv("kanpur_rainfall.csv")

# Extract all unique month names from the data
months <- unique(rain_data$Month)

# Create a numeric vector to store average rainfall per month
avg_rain <- numeric(length(months))

# Loop through months and compute the average rainfall for each
for (i in 1:length(months)) {
  # Select rainfall values for the i-th month
  values <- rain_data$Rainfall_mm[rain_data$Month == months[i]]
  
  # Compute and store the average
  avg_rain[i] <- mean(values)
}

# Identify the month with the highest average rainfall
max_month <- months[which.max(avg_rain)]

# Display the results
cat("Month with highest average rainfall:", max_month, "\n")


# ===============================
# Problem 2: Winning a Volleyball Game
# ===============================

# Define a function to simulate one game of volleyball
volleyball <- function(p) {
  
  # Initialize scores and rally counter
  ajay <- 0
  bharat <- 0
  rallies <- 0
  
  # Continue game until a player has at least 15 points AND a lead of 2
  while (!( (ajay >= 15 | bharat >= 15) && abs(ajay - bharat) >= 2 )) {
    
    # Increment rally count
    rallies <- rallies + 1
    
    # Simulate rally outcome
    if (runif(1) < p) {
      ajay <- ajay + 1
    } else {
      bharat <- bharat + 1
    }
  }
  
  # Return the total rallies
  return(rallies)
}

# Probabilities to simulate
p_values <- c(0.50, 0.55, 0.60)

# Create a results vector (label with probabilities)
ans <- numeric(length(p_values))
names(ans) <- as.character(p_values)

# Loop over probabilities
for (j in seq_along(p_values)) {
  
  # Vector to store rallies for multiple games
  rallies_vec <- numeric(2000)
  
  # Simulate 2000 games for this probability
  for (i in 1:2000) {
    rallies_vec[i] <- volleyball(p_values[j])
  }
  
  # Compute average rallies for this probability
  ans[j] <- mean(rallies_vec)
}

# Display final results
ans

# Load dataset
ship <- read.csv("ships.csv")


# 1. Overall correlation between service and incidents
cor(ship$service,ship$incidents)

# 2. Correlations within each ship type
ships_split <- split(ship, ship$type)

# Define a function to calculate correlation for specific columns
# We only want columns 2 to 5 (year, period, service, incidents)
calc_cor <- function(df) {
  cor(df[, c("year", "period", "service", "incidents")], use = "complete.obs")
}

# Apply the function to each item in the list
cor_by_type <- lapply(ships_split, calc_cor)

# Print the results
print(cor_by_type)

# 3. Base R plot colored by ship type
plot(ship$incidents, ship$service, xlab = "Incidents", ylab = "Service", main = "Service VS Incidents" )
plot(ship$incidents, ship$service,col = factor(ship$type), xlab = "Incidents", ylab = "Service", main = "Service VS Incidents" )



subset(ship, )
# -----------------------------------------------------------
# Explanation:
abline(lm(ship$incidents ~ ship$service))
#It doesn't support the hypothesis as correlation was positively correlated. Now using abline function it showing
# horizantal line passing through service at zero
# -----------------------------------------------------------


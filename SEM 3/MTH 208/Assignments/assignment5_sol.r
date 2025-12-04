# Quiz 5

#######################################
# Problem 1

logprob <- function(a,b) {
  
  # # Numerically unstable
  #return(log(exp(a) / (exp(a) + exp(b))))
  
  # Numerically stable
  m <- max(a, b)
  return(a - (m + log(exp(a - m) + exp(b - m))))
}

a <- -1000
b <- -999
logprob(a,b)

#######################################
# Problem 2

data <- read.csv("q5_data.csv")
z_vec <- as.numeric(data$z_vec)

# Numerically unstable
exp_z   <- exp(z_vec)
prob_vec <- exp_z / sum(exp_z)

# Sanity check
sum(prob_vec)  # should be 1 (within floating point tolerance)

# Numerically stable
z_shift <- z_vec - max(z_vec)          # stability shift
exp_z   <- exp(z_shift)
prob_vec <- exp_z / sum(exp_z)

# Sanity check
sum(prob_vec)  # should be 1 (within floating point tolerance)


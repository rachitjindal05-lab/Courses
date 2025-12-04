# Derive p_day from:
#   p_one = (A + B) / N
#   p_day = 1 - (1 - p_one)^E
# Simulate day-by-day until first hit; return list(days, p_day).
cityDejaVu <- function() {
  N <- 200000L
  A <- 120L
  B <- 40L
  E <- 500L
  
  p_one <- (A + B) / N
  p_day <- 1 - (1 - p_one)^E
  
  # Geometric-style simulation (days ~ Geom(p_day) with support 1,2,...)
  days <- 0L
  repeat {
    days <- days + 1L
    if (runif(1) < p_day) break
  }
  list(days = as.integer(days), p_day = as.numeric(p_day))
}

# Print p_day 
print(cityDejaVu()$p_day)

set.seed(12345)
# run cityDejaVu 200 times to compute the mean of "days";
# save it in mean_days and print it
n_runs <- 200L
days <- replicate(n_runs, cityDejaVu()$days)
print(mean(days))

# Alternatively, using sample

cityDejaVu <- function() {
  N <- 200000L
  A <- 120L
  B <- 40L
  E <- 500L
  
  p_one <- (A + B) / N
  p_day <- 1 - (1 - p_one)^E
  
  # Geometric-style simulation (days ~ Geom(p_day) with support 1,2,...)
  days <- 0L
  repeat {
    days <- days + 1L
    # Simulate today's encounters; treat 1..A+B as "hit" labels by symmetry
    hit <- any(sample(N, E, replace = TRUE) <= A+B)
    if (hit) break
  }
  list(days = as.integer(days), p_day = as.numeric(p_day))
}

# Print p_day 
print(cityDejaVu()$p_day)

set.seed(12345)
# run cityDejaVu 200 times to compute the mean of "days";
# save it in mean_days and print it
n_runs <- 200L
days <- replicate(n_runs, cityDejaVu()$days)
print(mean(days))

# Alternatively, using a while loop

cityDejaVu <- function() {
  N <- 200000L
  A <- 120L
  B <- 40L
  E <- 500L
  
  p_one <- (A + B) / N
  p_day <- 1 - (1 - p_one)^E
  
  # Geometric-style simulation (days ~ Geom(p_day) with support 1,2,...)
  days <- 0L
  hit  <- FALSE
  while (!hit) {
    days <- days + 1L
    # simulate E encounters uniformly from {1..N}; "hit" if any <= target
    hit <- any(sample(N, E, replace = TRUE) <= A+B)
  }
  list(days = as.integer(days), p_day = as.numeric(p_day))
}

# Print p_day 
print(cityDejaVu()$p_day)

set.seed(12345)
# run cityDejaVu 200 times to compute the mean of "days";
# save it in mean_days and print it
n_runs <- 200L
days <- replicate(n_runs, cityDejaVu()$days)
print(mean(days))





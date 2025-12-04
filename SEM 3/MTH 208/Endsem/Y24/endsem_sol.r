###############
# Task 1
###############

## integrate x exp(-x^2) to obtain the exact value of the integral
exact <- (1-exp(-1))/2

print(exact)

###############
# Task 2
###############

# integrate x exp(-x^2) using Method A

f1_methodA <- function(N) 
{
  x <- runif(N, 0, 1)
  approx <- sum(x*exp(-x^2))/N;
  return (approx)
}

###############
# Task 3
###############

# integrate x exp(-x^2) using Method B

f1_methodB <- function(N) 
{
  #M <- 1
  M <- 1/sqrt(2)
  
  x <- runif(N, 0, 1)
  y <- runif(N, 0, M)
  under <- (y <= x*exp(-x^2))
  n <- sum(under)
  approx <- n*M/N;
  return (approx)
}

###############
# Task 4
###############

# integrate x exp(-x^2) using Method C

f1_methodC <- function(N) 
{
  x <- seq(0, 1, length.out = N)
  y <- x*exp(-x^2)
  
  h <- 1 / (N - 1)
  approx <- h * ( 0.5*y[1] + sum(y[2:(N-1)]) + 0.5*y[N])
  return (approx)
}

###############
# Task 5
###############

# Runs the function f with argument n once and rertuns the output
# value val and the corresponding runtime elapsed
runtime <- function(f, n) {
  t0 <- proc.time()
  val <- f(n)
  t1 <- proc.time()
  elapsed <- (t1 - t0)["elapsed"]
  return(list(value = val, time = as.numeric(elapsed)))
}

###############
# Task 6
###############

base_seed <- 123456789

reps <- 50;
errorA <- numeric(reps)
errorB <- numeric(reps)
errorC <- numeric(reps)
approxA <- numeric(reps)
approxB <- numeric(reps)
approxC <- numeric(reps)
runtimeA <- numeric(reps)
runtimeB <- numeric(reps)
runtimeC <- numeric(reps)

comparison_table <- data.frame(N = numeric(6), 
                               ErrA = numeric(6),
                               VarA = numeric(6),
                               TimeA = numeric(6),
                               ErrB = numeric(6),
                               VarB = numeric(6),
                               TimeB = numeric(6),
                               ErrC = numeric(6),
                               VarC = numeric(6),
                               TimeC = numeric(6))
for (k in 4:9) {
  N = 4^k;
  
  for (r in 1:reps) {
    set.seed(base_seed + k * 100 + r) # different seed per run
    rtA <- runtime(f1_methodA, N)
    approxA[r] <- rtA$value
    runtimeA[r] <- rtA$time
    rtB <- runtime(f1_methodB, N)
    approxB[r] <- rtB$value
    runtimeB[r] <- rtB$time
    rtC <- runtime(f1_methodC, N)
    approxC[r] <- rtC$value
    runtimeC[r] <- rtC$time
  }
  
  errorA <- abs(approxA-exact)/abs(exact);
  errorB <- abs(approxB-exact)/abs(exact);
  errorC <- abs(approxC-exact)/abs(exact);

  comparison_table$N[k-3] <- N
  comparison_table$ErrA[k-3] <- mean(errorA)
  comparison_table$VarA[k-3] <- var(approxA)
  comparison_table$TimeA[k-3] <- mean(runtimeA)
  comparison_table$ErrB[k-3] <- mean(errorB)
  comparison_table$VarB[k-3] <- var(approxB)
  comparison_table$TimeB[k-3] <- mean(runtimeB)
  comparison_table$ErrC[k-3] <- mean(errorC)
  comparison_table$VarC[k-3] <- var(approxC)
  comparison_table$TimeC[k-3] <- mean(runtimeC)
}
print(comparison_table, row.names = FALSE)

###############
# Task 7
###############

library(ggplot2)

# Tidy for ggplot
plot_df <- data.frame(
  N = c(comparison_table$N),
  Method = rep(c("A", "B", "C"), each = nrow(comparison_table)),
  Error = c(comparison_table$ErrA, comparison_table$ErrB, comparison_table$ErrC),
  Variance = c(comparison_table$VarA, comparison_table$VarB, comparison_table$VarC),
  Runtime = c(comparison_table$TimeA, comparison_table$TimeB, comparison_table$TimeC)
)

# Plot with variance shown as vertical error bars
performance_plot <- 
  ggplot(plot_df, aes(x = N, y = Error, color = Method)) +
  
  geom_line(aes(group = Method), size = 1) +
  
  # points first
  geom_point(aes(size = Runtime), alpha = 0.8) +
  
  # error bars on top (so they are visible)
  geom_errorbar(
    aes(ymin = Error - sqrt(Variance),
        ymax = Error + sqrt(Variance)),
    width = 0.1,
    size = 0.6,
    color = "black",         # independent of method color
    alpha = 0.8
  ) +
  
  scale_x_log10() +
  scale_y_log10() +
  #scale_size_area(max_size = 10) +
  labs(
    title = "Error vs N (log–log scale)",
    subtitle = "Error bars = ±1 SD; dot size = runtime.",
    x = "N",
    y = "Relative Error"
  ) +
  theme_minimal()


# Print the plot
performance_plot


ggsave(
  filename = "performance_plot.png",  # output file name
  plot = performance_plot,            # which plot to save
  width = 8,                    # width in inches
  height = 6,                   # height in inches
  dpi = 300                     # resolution in dots per inch
)

###############
# Task 8
###############

## integrate log(exp(50x)-exp(50x-eps)) to obtain the exact value of the integral
eps <- 1.e-8
exact <- 25+log(1-exp(-eps))

print(exact)

###############
# Task 9
###############

# integrate log(exp(50x)-exp(50x-eps)) using Method A

f2_methodA <- function(N, eps) 
{
  x <- runif(N, 0, 1)
  #approx <- sum(log(exp(50*x)-exp(50*x-eps)))/N;
  approx <- sum(50*x+log(1-exp(-eps)))/N;
  return (approx)
}


for (k in 8:13) {
  N = 2^k;
  approxA <- replicate(reps,f2_methodA(N,eps))
  errorA <- abs(approxA-exact)/abs(exact);
  cat(N, "\t", mean(errorA), "\t", var(approxA), "\n");
}


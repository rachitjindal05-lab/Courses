## Problem 1

set.seed(123)
A <- matrix(rnorm(20), nrow=5, ncol=4)   # 5x4 matrix

# Frobenius norm function
frobenius_norm <- function(M) {
  sqrt(sum(M^2))
}

fn <- frobenius_norm(A)
cat("Our Frobenius norm:", fn, "\n")
cat("Base R Frobenius norm:", norm(A, type="F"), "\n")

# 1-norm function
one_norm <- function(M) {
  max(colSums(abs(M)))
}

on <- one_norm(A)
cat("Our 1-norm:", on, "\n")
cat("Base R 1-norm:", norm(A, type="1"), "\n")


## Problem 2

# 1) Factor column ----------------------------------------
# BUG: mtcars$cly (typo) -> use mtcars$cyl
mtcars$cyl_f <- as.factor(mtcars$cyl)

# 2) Mean mpg by cylinder ---------------------------------
# BUG: mean(na.rm=TRUE) is wrong; pass function + argument separately
mpg_by_cyl <- tapply(mtcars$mpg, mtcars$cyl_f, mean, na.rm = TRUE)
print(mpg_by_cyl)

# 3) Power-to-weight & best car ---------------------------
# BUG: ratio inverted; we want hp / wt
# BUG: which.min -> should be which.max for "highest" ratio
# BUG: rownames(best_idx) is wrong; need row name from data frame
mtcars$power_to_weight <- mtcars$hp / mtcars$wt
best_idx <- which.max(mtcars$power_to_weight)
best_car <- rownames(mtcars)[best_idx]
cat("Best car:", best_car, "\n")

# 4) Plot with colors by factor ---------------------------
# Ensure color vector aligns with factor levels
levs <- levels(mtcars$cyl_f)                 # e.g., "4","6","8"
cols <- c("red","blue","darkgreen")          # length matches number of levels
pt_cols <- cols[as.integer(mtcars$cyl_f)]    # map each row to a color

plot(x = mtcars$wt, y = mtcars$mpg,
     col = pt_cols, pch = 19,
     xlab = "Weight (1000 lbs)", ylab = "MPG",
     main = "MPG vs Weight (colored by cylinders)")

legend("topright",
       legend = levs,
       col = cols[seq_along(levs)],
       pch = 19,
       title = "Cylinders")

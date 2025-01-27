#part I
fixed.point <- function(g, x0, TOL = 1e-3, Nmax = 100) {
  iter <- c(x0)
  x_old <- x0
  
  for (n in 1:Nmax) {
    x_new <- g(x_old)
    iter <- c(iter, x_new)
    
    if (abs(g(x_new) - x_new) < TOL || n >= Nmax) {
      break
    }
    
    x_old <- x_new
  }
  
  return(iter)
}
#part II
set.seed(123)
x0<-1
g1 <- function(x) {
  (6 * x + 7 / x^4) / 7
}

g2 <- function(x) {
  x - (x^5 - 7) / (5 * x^4)
}

g3 <- function(x) {
  (7 / x)^(1 / 4)
}
#i: Plotting the functions g1, g2, g3 and y = x in the interval [1, 2]
library(ggplot2)
# Define the interval [1, 2]
x_vals <- seq(1, 2, length.out = 1000)

# Evaluate the functions
g1_vals <- sapply(x_vals, g1)
g2_vals <- sapply(x_vals, g2)
g3_vals <- sapply(x_vals, g3)
y_vals <- x_vals  # y = x line

# Plot g1, g2, g3, and y = x
plot(x_vals, g1_vals, type = "l", col = "blue", ylim = c(0.5, 2.5),
     ylab = "y", xlab = "x", main = "Fixed Point Iterations for g1, g2, g3")
lines(x_vals, g2_vals, col = "red")
lines(x_vals, g3_vals, col = "green")
lines(x_vals, y_vals, col = "black", lty = 2)

legend("topleft", legend = c("g1(x)", "g2(x)", "g3(x)", "y = x"),
       col = c("blue", "red", "green", "black"), lty = c(1, 1, 1, 2))

# Adding the fixed point (p, p)
p <- 7^(1/5)
points(p, p, pch = 16, col = "purple")
text(p, p, labels = "p", pos = 4, col = "purple")
# Adding grid lines for better visual understanding
grid()
#ii
x0 <- 1
TOL <- 1e-10
Nmax <- 100

iter_g1 <- fixed.point(g1, x0, TOL, Nmax)
iter_g2 <- fixed.point(g2, x0, TOL, Nmax)
iter_g3 <- fixed.point(g3, x0, TOL, Nmax)

# Get number of iterations and last iteration values
num_iter_g1 <- length(iter_g1) - 1
num_iter_g2 <- length(iter_g2) - 1
num_iter_g3 <- length(iter_g3) - 1

abs_error_g1 <- abs(iter_g1[length(iter_g1)] - iter_g1[length(iter_g1) - 1])
abs_error_g2 <- abs(iter_g2[length(iter_g2)] - iter_g2[length(iter_g2) - 1])
abs_error_g3 <- abs(iter_g3[length(iter_g3)] - iter_g3[length(iter_g3) - 1])

cat("Number of iterations for g1:", num_iter_g1, "\n")
cat("Absolute error for g1:", abs_error_g1, "\n\n")

cat("Number of iterations for g2:", num_iter_g2, "\n")
cat("Absolute error for g2:", abs_error_g2, "\n\n")

cat("Number of iterations for g3:", num_iter_g3, "\n")
cat("Absolute error for g3:", abs_error_g3, "\n\n")
#iii
# Open a new graphics device
dev.new()

# Create data frames for each function's absolute error over iterations
iterations_g1 <- seq_along(iter_g1) - 1
iterations_g2 <- seq_along(iter_g2) - 1
iterations_g3 <- seq_along(iter_g3) - 1

abs_errors_g1 <- abs(iter_g1 - 7^(1/5))
abs_errors_g2 <- abs(iter_g2 - 7^(1/5))
abs_errors_g3 <- abs(iter_g3 - 7^(1/5))

df_g1 <- data.frame(Iteration = iterations_g1, Error = abs_errors_g1, Function = "g1")
df_g2 <- data.frame(Iteration = iterations_g2, Error = abs_errors_g2, Function = "g2")
df_g3 <- data.frame(Iteration = iterations_g3, Error = abs_errors_g3, Function = "g3")

# Combine all data frames into one for easier plotting
df <- rbind(df_g1, df_g2, df_g3)

# Plotting the convergence of absolute errors with log scale for y-axis
ggplot(df, aes(x = Iteration, y = Error, color = Function)) +
  geom_line() +
  scale_y_log10() +
  labs(title = "Convergence of Fixed-Point Iterations",
       x = "Iteration",
       y = "Absolute Error (log scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")



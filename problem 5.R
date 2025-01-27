#i: Write the MovingParticle function
MovingParticle <- function() {
  # Initialize starting position at the origin (0, 0)
x <- 0
y <- 0
max_steps <- 100
positions <- matrix(c(x, y), nrow = 2)  # Store the initial position
for (i in 1:max_steps) {
direction <- sample(c("up", "down", "left", "right"), 1)
# Update position based on the chosen direction
if (direction == "up") {
y <- y + 1
} else if (direction == "down") {
y <- y - 1
} else if (direction == "left") {
x <- x - 1
} else if (direction == "right") {
x <- x + 1
}
# Append the new position to the positions matrix
positions <- cbind(positions, c(x, y))
# Stop if back at the origin or 100 steps reached
if ((x == 0 && y == 0) && i > 0) {
break
  }
}
return(positions)
}
#ii:Plotting 4 random walk trajectories
par(mfrow = c(2, 2))  # Set up the plotting area for 4 plots
set.seed(123)  # For reproducibility
for (i in 1:4) {
positions <- MovingParticle()
plot(positions[1,], positions[2,], type = "l", col = i,
xlab = "X", ylab = "Y", main = paste("Random Walk", i),
xlim = c(-20, 20), ylim = c(-20, 20))
points(positions[1,1], positions[2,1], col = "blue", pch = 16)  # Start point
points(positions[1,ncol(positions)], positions[2,ncol(positions)], col = "red", pch = 16)  # End point
}
# Part iii: Monte Carlo simulation to estimate the probability of returning to origin
set.seed(123)  # For reproducibility
n_experiments <- 1000
results_10<-0
results_100<-0
results_200<-0

for (i in 1:n_experiments) {
  positions <- MovingParticle()
  steps <- ncol(positions) - 1  # Number of steps taken
  
  if (steps <= 10 && all(positions[, steps + 1] == c(0, 0))) {
    results_10 <- results_10 + 1
  }
  if (steps <= 100 && all(positions[, steps + 1] == c(0, 0))) {
    results_100 <- results_100 + 1
  }
  if (steps <= 200 && all(positions[, steps + 1] == c(0, 0))) {
    results_200 <- results_200 + 1
  }
}
prob_10_steps <- results_10 / n_experiments
prob_100_steps <- results_100 / n_experiments
prob_200_steps <- results_200 / n_experiments

cat("Probability of returning to origin in at most 10 steps:", prob_10_steps, "\n")
cat("Probability of returning to origin in at most 100 steps:", prob_100_steps, "\n")
cat("Probability of returning to origin in at most 200 steps:", prob_200_steps, "\n")

# Part iv: Monte Carlo simulation to estimate average number of steps to return to origin
set.seed(123)  # For reproducibility
total_steps_10 <- 0
total_steps_100 <- 0
total_steps_200 <- 0
count_10 <- 0
count_100 <- 0
count_200 <- 0

for (i in 1:n_experiments) {
  positions <- MovingParticle()
  steps <- ncol(positions) - 1  # Number of steps taken
  
  if (steps <= 10 && all(positions[, steps + 1] == c(0, 0))) {
    total_steps_10 <- total_steps_10 + steps
    count_10 <- count_10 + 1
  }
  if (steps <= 100 && all(positions[, steps + 1] == c(0, 0))) {
    total_steps_100 <- total_steps_100 + steps
    count_100 <- count_100 + 1
  }
  if (steps <= 200 && all(positions[, steps + 1] == c(0, 0))) {
    total_steps_200 <- total_steps_200 + steps
    count_200 <- count_200 + 1
  }
}

avg_steps_10 <- ifelse(count_10 > 0, total_steps_10 / count_10, NA)
avg_steps_100 <- ifelse(count_100 > 0, total_steps_100 / count_100, NA)
avg_steps_200 <- ifelse(count_200 > 0, total_steps_200 / count_200, NA)

cat("Average number of steps to return to origin in at most 10 steps:", avg_steps_10, "\n")
cat("Average number of steps to return to origin in at most 100 steps:", avg_steps_100, "\n")
cat("Average number of steps to return to origin in at most 200 steps:", avg_steps_200, "\n")



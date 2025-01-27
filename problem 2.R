#i
set.seed(123)
n<-100 #100 sample tosses
die_rolls<-sample(1:6, n, replace = TRUE)
#ii
N<-function(){ #Function to simulate the number of rolls needed to observe all faces of the die
  faces_seen<-rep(FALSE, 6) # Vector for each seen faces
  count<-0
  while (sum(faces_seen) < 6) {
    roll <- sample(1:6, 1)
    faces_seen[roll] <- TRUE
    count <- count + 1
  }
  return(count)
}
N_simulations<-1000
n_values<-replicate(N_simulations,N())
mean_estimate<-mean(n_values)
cat("Estimated value of E(N):",mean_estimate,"\n")
#iii
mean_exactvalue<-sum(sapply(1:6,function(k) 6/(6 - k + 1)))
cat("Exact value of E(N):",mean_exactvalue,"\n")
#iv
sample_1<-3
n_sample<-200
samples<-replicate(n_sample, sample(1:6, sample_1, replace = FALSE))
# Calculate the frequency of each minimum value
min_values <- apply(samples, 2, min)
min_counts <- table(min_values)
#probability estimation
proba_bilities<-min_counts/n_sample
cat("Probability estimates for the minimum being 1, 2, 3, 4, 5, or 6:\n")
print(proba_bilities)


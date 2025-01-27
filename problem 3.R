#i
function_1<-function(x){
  return(cos(exp(x))^3+log(5*x,base=3)-atan(x))
}
#ii
N<-10^6
grid<-seq(1,2,length.out=N+1)
#iii
set.seed(123)
m<-c(10,100,1000,1000)
#defining the subgrids
subgrid.1<- sample(grid, m[1], replace = FALSE)
subgrid.2<- sample(grid, m[2], replace = FALSE)
subgrid.3<- sample(grid, m[3], replace = FALSE)
subgrid.4<- sample(grid, m[4], replace = FALSE)
#iv
#evalutaions of f at each subgrid
eval.1<-function_1(subgrid.1)
eval.2<-function_1(subgrid.2)
eval.3<-function_1(subgrid.3)
eval.4<-function_1(subgrid.4)
#averaging 
avg.1<-mean(eval.1)
avg.2<-mean(eval.2)
avg.3<-mean(eval.3)
avg.4<-mean(eval.4)
#storing averages in 4 dimensional vector
monte.carlo<-c(avg.1,avg.2,avg.3,avg.4)
#v
integral_exactvalue<-0.479199
cat("Exact value of the integral:", integral_exactvalue, "\n")
cat("Monte Carlo estimates:", monte.carlo, "\n")
#observations
cat("\nObservations:\n")
cat("The Monte Carlo estimates vary depending on the number of points sampled from the grid.\n")
cat("As the number of points increases (from subgrid.1 to subgrid.4), the estimate tends to get closer to the exact value.\n")

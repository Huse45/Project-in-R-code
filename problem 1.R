#i
set.seed(123)
n<-1000 #random numbers size
y<-runif(n,min=0,max=1) #generate random numbers in interval[0,1]
x<-y*(2-(-2))+(-2)
func_tion<-exp(x^2+x)
estimate_integral1<-mean(func_tion)*(2-(-2))
estimate_integral1
#ii
x_values<-runif(n,min=-2,max=2)
function_samples<-exp((x_values^2)+x_values)
estimate_integral2<-mean(function_samples)*(2-(-2))
estimate_integral2
#iii
fun<- function(x) {
  return(exp(x^2 + x))
}
integral_3<-integrate(fun,lower=-2,upper=2)
direct_value_integral_3<-integral_3$value
#comparing results
cat("part i:",estimate_integral1,"\n")
cat("part ii:",estimate_integral2,"\n")
cat("part iii:",direct_value_integral_3,"\n")
#the integrals from part i and ii can vary due to randomness of generated numbers
#the integral of part iii is more precise because it is calculated by directly analyzing the integral

# (Computer Experiment.) Generate 100 observations from a N(0,1) distribution. 
# Compute a 95 percent confidence band for the cdf F (as described in the appendix). 
# Repeat this 1000 times and see how often the confidence band contains the true distribution function. 
# Repeat using data from a Cauchy distribution.

library(ggplot2)
  
n <- 100
  # Generate 100 observations from a N(0,1) distribution.
  data = rnorm(100,0,1)
  
  # Get the CDF of the original data
  d_max <- max(data)
  d_min <- min(data)
  step <- 0.01
  x <- seq(d_min,d_max,step)
  len <- length(x)
  func <- vector(mode = "numeric", length = len)
  for(i in 1:len) {
    count <- 0
    for (j in 1:n) {
      if  (data[j] <= d_min+i*step ) {
        count = count + 1
      }
    }
    func[i] = count / n
  }
  
  # Compute a 95% percent confident band for the CDF F.
  con_lvl = 0.05
  c = 1.96
  t = log(c/con_lvl) / (c * n)
  ll <- length(func)
  upper <- vector(mode = "numeric", length = ll)
  lower <- vector(mode = "numeric", length = ll)
  for (i in 1:ll) {
    l <- max(0, func[i] - t)
    u <- min(1, func[i] + t)
    upper[i] <- u
    lower[i] <- l
  }
  

# Generate data from Cauchy Distribution
percentage <- function(n) {
  c <- rnorm(n,0,1)
  funcc <- vector(mode = "numeric", length = len)
  for(i in 1:len) {
    count <- 0
    for (j in 1:n) {
      if  (c[j] <= d_min+i*step ) {
        count = count + 1
      }
    }
    funcc[i] = count / n
  }
  
  rsl <- vector(mode = "numeric", length = ll)
  for (i in 1:ll) {
    if (funcc[i] <= upper[i] && funcc[i] >= lower[i]) {
      rsl[i] = 1
    }
  }
  p <- sum(rsl) / ll
  return(p)
}
# Calculate the percentage


final <- replicate(1000,percentage(100))
print(sum(final)/1000)


c <- rnorm(n,0,1)
funcc <- vector(mode = "numeric", length = len)
for(i in 1:len) {
  count <- 0
  for (j in 1:n) {
    if  (c[j] <= d_min+i*step ) {
      count = count + 1
    }
  }
  funcc[i] = count / n
}

plot(func)
points(lower,col = "red",cex = 0.1)
points(upper,col = "blue",cex = 0.1)
points(funcc,col = "green")



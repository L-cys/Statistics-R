# Excercises 8.4.2
# Conduct a simulation to compare the various bootstrap confidence interval methods. 
# Let n = 50 and let T(F) = 􏰔(x − μ)3dF(x)/σ3 be the skewness.
# Draw Y1,...,Yn ∼ N(0,1) and set Xi = eYi, i = 1,...,n. 
# Construct the three types of bootstrap 95 percent intervals for T (F ) from the data X1, . . . , Xn. 
# Repeat this whole thing many times and estimate the true coverage of the three intervals.


library(fBasics)
# Initiate data
init <- function(n,B) {
  Y <- rnorm(n,0,1)
  X <- vector(mode = "numeric", length = n)
  for (i in 1:n) {
    X[i] <- exp(Y[i])
  }
  # Skewness
  skw_hat <- skewness(X)
  # Boostrap
  Tboot <- vector(mode = "numeric", length = B)
  for (i in 1:B) {
    Xstar <- sample(X,n,replace=TRUE)
    Tboot[i] <- skewness(Xstar)
  }
  se_boot <- sd(Tboot)
  
  # Get interval
  Normal <- c(skw_hat - 2*se_boot, skw_hat + 2*se_boot)
  percentile <- c(quantile(Tboot,.025), quantile(Tboot,.975))
  pivotal <- c( 2*skw_hat-quantile(Tboot,.975),
                2*skw_hat-quantile(Tboot,.025) )
  
  # Return 
  out <- c(Normal[1],Normal[2],percentile[1],percentile[2],pivotal[1],pivotal[2])
  #out <- data.frame("Nm_1" <- Normal[1],"Nm_2" <- Normal[2] ,"pctl_1" <- percentile[1],"pctl_2" <- percentile[2], "pvt_1" <- pivotal[1],"pvt_2" <- pivotal[2])
  return(out)
}


all <- replicate(100,init(50,100))
Normal_ <- c(mean(all[1,]),mean(all[2,]))
percentile_ <- c(mean(all[3,]),mean(all[4,]))
pivotal_ <- c(mean(all[5,]),mean(all[6,]))

# Print result
print(Normal_)
print(percentile_)
print(pivotal_)





# Exercise 8.6.7
# Let X1, ..., Xn ∼ Uniform(0, θ). Let θ = Xmax = max{X1, ..., Xn}. 
# Generate a data set of size 50 with θ = 1.
# (a) Find the distribution of θ. Compare the true distribution of θ to the histograms from the bootstrap.
# (b) This is a case where the bootstrapdoes very poorly. In fact, we can prove that this is the case. Show that P(θ. = θ.) = 0
# and yet P(θ.* = θ.) ≈ 0.632.

# Initialize data
n <- 50
theta <- 1
X <- runif(n,0,theta)

# Bootstrap
theta_hat <- max(X)
B <- 100
Tboot <- vector(mode = "numeric", length = B)
for (i in 1:B) {
  Xstar <- sample(X,n,replace=TRUE)
  Tboot[i] <- max(Xstar)
}
hist(Tboot)

# Why bootstrap behave poorly?
# We can tell from the hisogram that the probability of θ = 1 almost reach 0.6 or more.
# However, according to the uniform distribution, the probability of θ equals to any value
# should be zero.
# Init data
LSAT <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
GPA <- c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,3.96)
SAT_AVG = mean(LSAT)
GPA_AVG = mean(GPA)
sum_u <- 0
sum_y <- 0
sum_z <- 0
for (i in 1:15) {
  sum_u = sum_u + (LSAT[i] - SAT_AVG) * (GPA[i] - GPA_AVG)
  sum_y = sum_y + (LSAT[i] - SAT_AVG) * (LSAT[i] - SAT_AVG)
  sum_z = sum_z + (GPA[i] - GPA_AVG) * (GPA[i] - GPA_AVG)
}

cor_plug_in = sum_u / ((sum_y * sum_z) ^ 0.5)

# Bootstrap
x1 <- LSAT
x2 <- GPA
n1 <- length(x1)
n2 <- length(x2)
cor_hat <- cor(x1,x2)
B <- 100
Tboot <- vector(mode = "numeric",length = B)

for (j in 1:B) {
  xx1 <- sample(x1,n1,replace = TRUE)
  xx2 <- sample(x2,n2,replace = TRUE)
  Tboot[j] <- cor(xx1,xx2)
}

# Get se
se_boot <- sd(Tboot)

# Get interval
Normal <- c(cor_hat - 2*se_boot, cor_hat + 2*se_boot)
percentile <- c(quantile(Tboot,.025), quantile(Tboot,.975))
pivotal <- c( 2*cor_hat-quantile(Tboot,.975),
             2*cor_hat-quantile(Tboot,.025) )

  
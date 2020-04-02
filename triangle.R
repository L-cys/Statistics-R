
triangle <- function(a,b,c) {
  flag = 0
  if (a + b > c && a + c > b && b + c > a) {
    flag = 1
  }
  return(flag)
}

all_data <- replicate(100000,runif(3,0,1))
add_row <- matrix(100000:1,ncol = 100000)
for (i in 1:100000) {add_row[i] <- triangle(all_data[3*(i-1)+1],all_data[3*(i-1)+2],all_data[3*(i-1)+3])}
final = sum(add_row) / 100000
print(final)

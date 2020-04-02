f_model = fuction(d1,d2,d3) {
  f1 = rep(0,d3)
  for(i in 1:d1) {
    fi = rnorm(d3)
    fi2 = fi ^ 2
    f1 = f1 + fi2
  }
  
  f2 = rep(0,d3) 
    for (i in 1:d2) {
      fj = rnorm(d3)
      fj2 = fj ^ 2
      f2 = f2 + fj2
    }
  f = (f1/d1)/(f2/d2)
}

f11 = f_model(5,10,50)
f11 = f11[f11>=-1 & f11 <=7]
plot(density(f11),xlim=c(0.23,6),ylim=c(0,2.2),col = 'blue',lwd = 2,main = 'F_distribution',xlab = '',ylab = '')

f22 = f_model(5,10,100)
f22 = f22[f22>=-1 & f22 <=7]
lines(density(f22),xlim=c(0.23,6),col = 'red',lwd = 2)

f33 = f_model(5,10,100)
f33 = f33[f33>=-1 & f33 <=7]
lines(density(f_5_2),xlim=c(0.23,6),col = 'black',lwd = 2)



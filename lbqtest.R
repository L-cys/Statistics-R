library(readxl)
shangzheng <- read_excel("Downloads/shangzheng.xlsx")
View(shangzheng)                                                                           
before <- shangzheng$before08
acf(before)
after <- read_excel("Downloads/shangzheng的副本.xlsx", 
                    +     sheet = "Sheet2")
View(after)    
#Q-test
Box.test(before,lag = 15,'Box-Pierce')
Box.test(before,lag = 25,'Box-Pierce')
Box.test(before,lag = 21,'Box-Pierce')
Box.test(before,lag = 29,'Box-Pierce')
Box.test(before,lag = 8,'Box-Pierce')
Box.test(after,lag = 8,'Box-Pierce')
Box.test(after,lag = 28,'Box-Pierce')
Box.test(after,lag = 24,'Box-Pierce')

#LB-test
Box.test(before,lag = 15,'Ljung-Box')
Box.test(before,lag = 25,'Ljung-Box')
Box.test(before,lag = 21,'Ljung-Box')
Box.test(before,lag = 29,'Ljung-Box')
Box.test(before,lag = 8,'Ljung-Box')
Box.test(after,lag = 8,'Ljung-Box')
Box.test(after,lag = 28,'Ljung-Box')
Box.test(after,lag = 24,'Ljung-Box')
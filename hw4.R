library(tidyverse)
str(iris)
head(iris,n = 5)
#想看每个类别最长的sepal length
sorting <- iris %>% group_by(Sepal.Length) %>% mutate(n = n()) %>% group_by(Species) %>% arrange(desc(Sepal.Length)) %>% slice(1)
#看每个类别sepal width的均值
sorting_1 <- iris %>% group_by(Species) %>% summarize(mean(Sepal.Width))
write_iris <- iris
write_csv(write_iris,"write_iris.csv")
write.table(write_iris,"write_iris.txt")
write.table(write_iris,"write_iris.xlsx",sep = "/t")
save.image("everything.Rdata")


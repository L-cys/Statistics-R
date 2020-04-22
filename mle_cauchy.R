# 利用R计算Cauchy分布两个参数的矩估计和极大似然估计，
# 并运行100次实验做出两个参数估计量的直方图

#柯西分布不存在各阶矩均不存在，故不可使用矩估计进行参数估计

#mle估计
mle_cauchy <- function(para, x) {
  location = para[1]; scale = para[2];
  sum(-dcauchy(x,location,scale,log=TRUE))
}
n = 100000;

estimate<-replicate(100,
    {x<-rcauchy(n,0,1);
    para.start= c(0,1);
    max_likelihood = nlm(mle_cauchy,para.start,x);
    max_likelihood$estimate}
)

location_estimate = estimate[1,];
scale_estimate = estimate[2,];
par(mfrow=c(1,2))
hist(location_estimate,col="red",xlim=c(0,2),xlab='location_estimate');
hist(scale_estimate,col="red",xlim=c(1,3),xlab='scale_estimate');
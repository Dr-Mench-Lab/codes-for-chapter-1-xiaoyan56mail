age<-c(54,42,51,54,49,56,33,58,54,64,49)
par(mfrow=c(2, 1))
hist(age, freq= FALSE, breaks =6)
points(density(age), type="l")
rug(age)
library (vioplot)
vioplot(age, horizontal=TRUE, col="gray")
 
stem(age,scale = 2)
qt(1-0.05/2,df=length(age)-1)
?t.test
t.summary<-t.test(age,mu=50)
t.summary
summary(age)
bs.one.samp.dist(age)



t.dist.pval<-function(t.summary)
{
  par(mfrow=c(1, 1))
  lim.extreme<-max(4, abs(t.summary$statistic)+ 0.5)
  lim.lower<- -lim.extreme;
  lim.upper<-lim.extreme;
  x.curve<- seq(lim.lower, lim.upper, length=200)
  y.curve <-dt(x.curve, df =t.summary$parameter)
  plot(x.curve, y.curve,type="n",ylab= paste ("t-dist( df =", signif(t.summary$parameter, 3),")"),xlab= paste("t-stat=",signif(t.summary$statistic,5 ),",Shaded area is p-value=", signif(t.summary$p.value, 5)))
if ((t.summary$alternative=="less") | (t.summary$alternative =="two.sided"))
{ 
    x.pval.l<-seq(lim.lower, -abs(t.summary$statistic), length=200)
    y.pval.l <- dt(x.pval.l,df=t.summary$parameter)
    polygon(c(lim.lower, x.pval.l, -abs(t.summary$statistic)),c(0,y.pval.l,0), col="gray")
 }
if ((t.summary$alternative=="greater") | (t.summary$alternative =="two.sided"))
{
  x.pval.u <-seq(abs(t.summary$statistic), lim.upper, length=200)
  y.pval.u <- dt(x.pval.u, df=t.summary$parameter)
  polygon(c(abs(t.summary$statistic),x.pval.u, lim.upper),c(0,y.pval.u,0),col="gray")
  }
  points(x.curve,y.curve,type="l",lwd=2,col="blue")
}
t.dist.pval(t.summary)

names(t.summary)
t.summary$statistict
t.summary$parameter
t.summary$p.value
t.summary$conf.int
t.summary$estimate
t.summary$null.value
t.summary$stderr
t.summary$alternative
t.summary$method
t.summary$data.name



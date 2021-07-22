library(pwr)
?power.t.test
power.t.test(n = NULL, delta=1.00-0.54, sd= sd(toco),sig.level =0.05, power =0.50,type ="one.sample", alternative ="two.sided")
power.t.test(n= NULL, delta =0.75-0.54, sd=sd(toco),sig.level =0.05, power =0.50,type ="one.sample", alternative ="two.sided")
library(TeachingDemos)
?power.examp

toco<-c(20.5,18.5,20.0,19.5,19.5,21.0,17.5,22.5,20.0,19.5,18.5,20.0,18.0,20.5)
par(mfrow=c(2, 1))
hist(toco, freq= FALSE, breaks =6)
points(density(toco), type ="l")
rug(toco)
library(vioplot)
vioplot(toco, horizontal=TRUE, col="gray")
stem(toco, scale=2)
qt(1-0.05/2, df=length(toco)-1)
t.summary <-t.test(toco, mu =20,alternative = "less")
t.summary
summary(toco)

bs.one.samp.dist<- function(dat, N= 1e4)
{
  n<-length(dat);
  sam <-matrix(sample(dat, size =N*n, replace= TRUE), ncol=N);
  sam.mean<-colMeans(sam);
  old.par <- par(no.readonly= TRUE)
  par( mfrow=c(2,1),mar=c(3,2,2,1),oma=c(1,1,1,1))
  hist(dat, freq= FALSE, breaks=6,main ="Plot of data with smoothed density curve")
  points(density(dat),type="l")
  rug(dat)
  hist(sam.mean, freq= FALSE, breaks =25,main="Bootstrap sampling distribution of the mean",xlab= paste("Data n=",n,", mean=", signif(mean(dat), digits=5),",se=", signif(sd(dat)/sqrt(n)), digits = 5))
  points(density (sam.mean), type ="l")
  x <- seq (min(sam.mean), max(sam.mean), length =1000)
  points(x, dnorm(x, mean=mean(dat), sd= sd(dat)/sqrt(n)),type ="l",lwd=2, col ="green")
  rug(sam.mean)
  par(old.par)
}



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

bs.one.samp.dist(toco)
##t.dist.pval(t.summary)



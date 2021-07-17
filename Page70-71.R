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
  x <-rgamma(10, shape =.5, scale =20)
  bs.one.samp.dist(x)
                                              

  
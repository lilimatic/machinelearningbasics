#############
## Bayes ####
#############

#We create a function that approximates our computation 
beta_binom = function(n,y,a=2,b=3,main=""){
  #likelihood: y|theta - binom(n,theta)
  #prior: theta - beta(a,b)
  #posterior: theta|y - beta(a+y,n-y+b)
  #Set all values that we consider for thet
  theta=seq(0.001,0.999,0.001)
  #set the distributional assumptions for our prior
  prior=dbeta(theta,a,b)
  if(n>0){likelihood=dbinom(y,n,theta)} 
  #as we have computed the posterior by hand, we add this information to our code
  if(n>0){posterior=dbeta(theta,a+y,n-y+b)} 
  #standardization
  prior=prior/sum(prior)
  if(n>0){likelihood=likelihood/sum(likelihood)}
  if(n>0){posterior=posterior/sum(posterior)}
  ylim=c(0,max(prior))
  if(n>0){ylim=c(0,max(c(prior,likelihood,posterior)))}
  plot(theta,prior,type="l",lty=2,xlab="theta",ylab="density",main=main,ylim=ylim)
  if(n>0){lines(theta,likelihood,lty=3)}
  if(n>0){lines(theta,posterior,lty=1,lwd=2)}
  legend("topright",c("prior","likelihood","posterior"),
         lty=c(2,3,1),lwd=c(1,1,2),inset=0.01,cex=0.4,pt.cex=1)
}

#We now look at different parameter choices for our prior for different sample sizes
par(mfrow=c(2,2))
beta_binom(3,2,3,4,main="(a) Prior: beta(3,4)")
beta_binom(3,2,5,7,main="(b) Prior: beta(5,7)")
beta_binom(30,20,2.5,7.5,main="(c) Prior: beta(2.5,7.5)")
beta_binom(300,200,2.5,7.5,main="(d) Prior: beta(2.5,7.5)")

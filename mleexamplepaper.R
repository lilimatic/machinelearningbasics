#Maximum likelihood function for
# f(x) = (1+alfax)/2
set.seed(123)
#
#
set = runif(30)

#Log-likelihood fct.(Fja verodostojnosti)
alfa_loglike = function(alfa,x){
  loglik = -length(x)*log(2)+sum(log(1+alfa*x))
  return(loglik)
}

#Score-fct: first derivative of the likelihood fuct
alfa_score = function(alfa,x){
  
  dloglik = sum(x/(1+alfa*x))
  
  return(dloglik)
  
}

#Hessian function 
alfa_hessian = function(alfa,x){
  d2loglik = - sum(x*x/(1+alfa*x)^2)
  return(d2loglik)
}

#As the score function has to be equal to zero we need to apply appropriate numerical methods 


#For our initial value, we use the method of moments estimator 
#E(X)= \int_{-1}^{1} f(x,\alfa) = \frac{1}{3} \alfa
#
#mean(set) = \frac{1}{3} \alfa pa je \alfa= 3 * mean(set) nasa ocena. 

#starting value
uzoracka = mean(set)
alfa_0 = 3 * uzoracka
#[1] 0.4758192

#opet vector 
alfa = c()

#first value 
alfa[1] = alfa_0
#some error that we tolerate
eps = 0.003

###
#Newton-Ralphson numerical method for computation
i = 1 
repeat{
  alfa[i+1]=alfa[i] - alfa_score(alfa[i],set)/alfa_hessian(alfa[i],set)
  #if the difference is small we break
  i=i+1
  if(abs(alfa[i]-alfa[i-1])<eps) break
}

alfa[i]
#[1] 0.4943927 is our estimated value 

#we seek the value for our maximum liklihooh and draw an array to look for the graphical optimal value

#Crtamo loglike_array
n = 50
loglike_array = c()
alfa = seq(-1,1,length.out = 50)
for (i in 1:n) {
  loglike_array = c(loglike_array, alfa_loglike(alfa[i],set))
}

#we draw the graph and expect to receive our estimated  value 
plot(alfa,loglike_array,type="l")
#the function is at it's maximum at 0.5, which suits the value we received 






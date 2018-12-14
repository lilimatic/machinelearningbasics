###########
### logistic regression from hand

if(!require("ISLR")) install.packages("ISLR"); library("ISLR")

#sigmoid function

sigmoid = function(eta){
  1/(1+exp(-eta))
  }

#loglikelihood function 
loglik = function(theta,x,y){
  f = sigmoid(x%*%theta)
  loglik = sum((t(-y)%*%log(f)-t(1-y)%*%log(1-f)))/length(y) 
  return(loglik)
}

#GRADIENT FUNCTION

delta = function(theta,x,y){
  f = sigmoid(x%*%theta)
  grad = (t(x)%*%(f - y))/length(y) 
  return(grad)
}

logit = function(x,y){
  
  
  
  #now we take some initial values for theta
  theta = matrix(rep(0, ncol(x)), nrow = ncol(x))
  
  #we perform gradient descent-a nonlinear equation 
  #we perform g.d. with the optim function to solve iteratively for theta
 
  theta_est = optim(matrix(rep(0, 4), nrow = 4), loglik, delta, x=x, y=y)
  #return coefficients
  probability = sigmoid(x%*%theta_est)
  return(probability)
}

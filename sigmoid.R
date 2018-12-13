####Sigmoid simulation

set.seed(123)

#simulate 1000 random values between -10 and 10
x = runif(1000,-10,10)

#program sigmoid function by definition
sigmoid = function(x){
  1/(1+exp(-x))
}

#Plot sigmoid function
plot(x,sigmoid(x), col="pink", lwd=1, main= "Simulated sigmoid function", xlab= "x", ylab=expression(phi))


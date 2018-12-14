#################
### Overffing ###
#################
if(!require("caTools")) install.packages("caTools"); library("caTools")

x = seq(1,100,0.5)
#we generate 4 polynomial functions of oder 3,4,5
poly_3 = poly(x, degree=3)

poly_5 = poly(x, degree=5)

#Asume samo parameters for the true functions
actual_3 = poly_3 %*% c(2,1,2) 

#we selected our values and add some noise 
actual_5 = poly_5 %*% c(2,1,2,-1,6)+ rnorm(3, sd= 0.3)


#Regression 
linear_reg5 = lm(actual_5 ~x)
reg_3       = lm(actual_5 ~ poly(x,3))
reg_5       = lm(actual_5 ~ poly(x,5))

par(mfrow=c(1,2))
plot(x, actual_5, col="black", xlab="x", ylab="f(x)", main= "Examples of over- and underfitting")
abline(linear_reg5, col="green", lwd=2)
lines(x, fitted(reg_5), col="red", lwd=2)
lines(x, fitted(reg_3), col="yellow", lwd=2)

#ading more noise
#we selected our values and add some noise 
actual_5 = poly_5 %*% c(2,1,2,-1,6)+ rnorm(100, sd= 0.5)



#Regression 
linear_reg5 = lm(actual_5 ~x)
reg_3       = lm(actual_5 ~ poly(x,3))
reg_5       = lm(actual_5 ~ poly(x,5))

plot(x, actual_5, col="black", xlab="x", ylab="f(x)", main= "Noisy data")
abline(linear_reg5, col="green", lwd=2)
lines(x, fitted(reg_5), col="red", lwd=2)
lines(x, fitted(reg_3), col="yellow", lwd=2)
 



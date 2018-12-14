#################
### Overffing ###
#################

x = seq(1,100,0.5)
#we generate 4 polynomial functions of oder 3,4,5
#we generate polynomials of order 3 and 5
poly_3 = poly(x, degree=3)

poly_5 = poly(x, degree=5)

#Asume samo parameters for the true functions
#We selected some coeficients and now determine the 

############true function #############
actual_3 = poly_3 %*% c(2,1,2) 

#we selected our values and add some noise 

#We add some noise to the true value
#this is presented in the formof rnorm(3, sd= 0.3)
actual_5 = poly_5 %*% c(2,1,2,-1,6)+ rnorm(3, sd= 0.3)


#Regression 

#We try to approximate our data with
#data = polynomial function of order 5 with some noise

#linear regression model y = beta * x +eps
linear_reg5 = lm(actual_5 ~x)

#polynomial regression model order 3 
#respresent it it by a polynomial of order 3 find (beta, beta^1, beta^2, beta^3)


reg_3       = lm(actual_5 ~ poly(x,3))

#Do the same for a polynomial function of order 5
reg_5       = lm(actual_5 ~ poly(x,5))


#Plot results
par(mfrow=c(1,2))
plot(x, actual_5, col="black", xlab="x", ylab="f(x)", main= "Examples of over- and underfitting")
abline(linear_reg5, col="green", lwd=2)
lines(x, fitted(reg_5), col="red", lwd=2)
lines(x, fitted(reg_3), col="yellow", lwd=2)

#ading more noise
#add eps with rnorm(100, sd= 0.5)
#we selected our values and add some noise 
actual_5 = poly_5 %*% c(2,1,2,-1,6)+ rnorm(100, sd= 0.5)

#Regression 

#same task
linear_reg5 = lm(actual_5 ~x)
reg_3       = lm(actual_5 ~ poly(x,3))
reg_5       = lm(actual_5 ~ poly(x,5))

#plot
plot(x, actual_5, col="black", xlab="x", ylab="f(x)", main= "Noisy data")
abline(linear_reg5, col="green", lwd=2)
lines(x, fitted(reg_5), col="red", lwd=2)
lines(x, fitted(reg_3), col="yellow", lwd=2)
 



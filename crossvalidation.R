########################
### CROSS VALIDATION ### 
########################

#packages
if(!require("splines")) install.packages("splines"); library("splines")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")

#fix values
set.seed(1)

#Predetermination of values
iter = 110
n_df = 35
df = 1:n_df
#Parameter choice
parameter = c(5, -0.12, 0.0037, -3*10^(-5))
#training sample
training = 100
#test sample
test = 10000
sigma_eps = 0.5
#number of folgs
kfolds = 5



#Data generation 
#We create a function that results a data set of values for x and y
#we create n values of random numbers between 0 and 75
#We create a function that generates a data frame of vlues x and y, where y is a function of x
syntheticdata = function(n, parameter, sigma_eps) {
  eps = rnorm(n, 0, sigma_eps)
  x = sort(runif(n, 0, 100))
  y = as.numeric(cbind(1, poly(x, degree = (length(parameter) - 1), raw = TRUE)) %*% parameter + eps)
  return(data.frame(x = x, y = y))
}

#test sample split
xy = res = list()
xy_test = syntheticdata(test, parameter, sigma_eps)
for (i in 1:iter) {
  xy[[i]] = syntheticdata(training, parameter, sigma_eps)
  x = xy[[i]][, "x"]
  y = xy[[i]][, "y"]
  res[[i]] = apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
}


xy = syntheticdata(training, parameter, sigma_eps)
x = xy$x
y = xy$y

#linear regression- linear model
fitted_models = apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
#mse calculation on each
mse = sapply(fitted_models, function(obj) deviance(obj)/nobs(obj))


xy_test = syntheticdata(test, parameter, sigma_eps)
pred = mapply(function(obj, degf) predict(obj, data.frame(x = xy_test$x)), 
              fitted_models, df)
te = sapply(as.list(data.frame(pred)), function(y_hat) mean((xy_test$y - y_hat)^2))

#we generate random integers
folds_i = sample(rep(1:kfolds, length.out = training))
#some part of the data will be left out in n iteration
leaveout = matrix(NA, nrow = kfolds, ncol = length(df))
#####Algorithm
#As described in the paper, one porportion of the sample is left out
for (k in 1:kfolds) {
  test_i = which(folds_i == k)
  train_xy = xy[-test_i, ]
  test_xy = xy[test_i, ]
  x = train_xy$x
  y = train_xy$y
  fitted_models = apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
  x = test_xy$x
  y = test_xy$y
  pred = mapply(function(obj, degf) predict(obj, data.frame(ns(x, df = degf))), 
                fitted_models, df)
  leaveout[k, ] = sapply(as.list(data.frame(pred)), function(y_hat) mean((y - 
                                                                          y_hat)^2))
}
#finally we compute the overall error by averaging over all cross-validations. 
cv = colMeans(leaveout)

#Now we want to plot how the model evolves when we cange the model complexity

#we regard to true variable with respect to y
plot(df, mse, type = "l", lwd = 2, col = gray(0.4), ylab = "Prediction error", 
     xlab = "flexibilty", main = paste0(kfolds,"-fold Cross-Validation"), ylim = c(0.1, 0.8), log = "x")
lines(df, te, lwd = 2, col = "blue", lty = 2)

#This is the cross validation error !
cv_sd = apply(leaveout, 2, sd)/sqrt(kfolds)
errbar(df, cv, cv + cv_sd, cv - cv_sd, add = TRUE, col = "blue",
       lwd = 0.5 ,pch = 19)

#Lastly, the plot oru generalization or test error
lines(df, cv,  col = "red", lwd = 2)
points(df, cv, col = "red", pch = 19)
legend(x = "topright", legend = c("training error", "Generalization (test) error", "Cross validation error"), 
       lty = c(1, 2, 1), lwd = rep(2, 3), col = c(gray(0.4), "blue", "red")
       , cex = 0.85, text.width = 0.4)


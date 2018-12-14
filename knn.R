### Load libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("datasets")) install.packages("datasets")
library(ggplot2)
library(datasets)

### Initialize contants and data
#we choose k =4
k = 5
#we want to distinguish between versicolor and setosa
data(iris)
iris_class = iris[iris["Species"] != "versicolor",]
iris_class$Species = iris_class$Species != "setosa"

### Functions

#we compute the euclidean distance between points
euklidisch = function(X,Y) {
  distance = outer(rowSums(X ** 2), rowSums(Y ** 2), '+') - 2 * tcrossprod(X, Y)
  return(distance)
}

#this is a storge vector for knn 
knn = function(x, y, k=5) {
  my_knn = list()
  my_knn[['points']] = as.matrix(x)
  my_knn[['value']] = as.matrix(y)
  my_knn[['k']] = k
  attr(my_knn, "class") = "knn"
  return(my_knn)
}


#THIS IS THE HEART OF OUR ALGORITHMS
predict.knn = function(my_knn, x) {
#we first compute the distances between one point of the test set
  #and all training
  #compute eucliden
  dist_pair = euklidisch(as.matrix(x), my_knn[['points']])
  #for each row of distance matrix is ordered according to their value
  #at the end we take the first 5 in row, which is equivalent to our first 5 neighbors
  #apply(dist_pair,1,order) orders entities by their distance
  result = crossprod(apply(dist_pair,1,order) <= my_knn[['k']], my_knn[["value"]]) / my_knn[['k']]
  return(result)
}

#we now predict our results
knn_class = knn(iris_class[,c("Sepal.Length", "Sepal.Width")], iris_class$Species, k = k)
predict(knn_class, iris_class[,c("Sepal.Length", "Sepal.Width")])

#Graphical setting for ggplot 

x_ax = seq(min(iris_class[,1]) - 0.2, max(iris_class[,1]) + 0.2, length.out = 200)
y_ax = seq(min(iris_class[,2]) - 0.2, max(iris_class[,2]) + 0.2, length.out = 200)
coord = expand.grid(x = x_ax, y = y_ax)

#predict probs and fill within
coord[['prob']] = predict(knn_class, coord[,1:2])

ggplot() + 
  
  geom_tile(data = coord, mapping = aes(x, y, fill = prob)) + scale_fill_gradient(low = "pink", high = "lightblue") +
 
  geom_point(data = iris_class, mapping = aes(Sepal.Length, Sepal.Width, shape = Species), size = 3 ) + 
  
  xlab('Sepal length') + ylab('Sepal width') + ggtitle('Decision cuts for Nearest Neighbor') +

  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))

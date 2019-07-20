#Problem Statement
#Given Functions
#The first function, makeVector creates a special "vector", which is really a list containing a function
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

#function calculates the mean of the special "vector" created with the above function
#Given Function
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#Problem Statement
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Creating Special object for storing the matrix and cache its inverse.
#This function creates a special "matrix" object that can cache its inverse.

cachedMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  Inverseset <- function(inverse) invers <<- inverse
  Inverseget <- function() invers
  list(set = set,
       get = get,
       Inverseset = Inverseset,
       Inverseget = Inverseget)
}


#function to the computation of the inverse of the matrix which was created as Cached Matrix.
#it sends the inverse based on the cache.

solution <- function(x, ...) {
  ## Return the inverse of the input matrix
  invers <- x$Inverseget()
  if (!is.null(invers)) {
    message("cached data")
    return(invers)
  }
  
  mat <- x$get()
  invers <- solve(mat, ...)
  x$Inverseset(invers)
  invers
}


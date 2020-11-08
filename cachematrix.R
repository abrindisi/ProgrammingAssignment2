## Both functions bellow are created in order to cache the inverse of a matrix
##so we dont need to compute it everytime we need it. 
##This is really useful for example when we need to work with long vectors and
##calculate the mean or other statistic tools or when we require a matrix of it. 


## The following function creates a special "matrix" object that can cache its
##inverse . As scoping ruls means, this function contain another function inside 
##that is not defined in the global environment but in the insides of the other function.

makeCacheMatrix <- function(x = matrix()) {
  inversemat<-NULL ##we create an object without a value defined
  set <- function(y) { ##we create a function inside another function to isolate it from the global environment
    x <<- y 
    inversemat<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversemat <<- inverse
  getinverse <- function() inversemat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by
##the first function above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.
cacheSolve <- function(x, ...) {
  inversemat <- x$getinverse()
  if(!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)##we take the inverse of "x", and then apply the "if"function to condition if the object inversemat has a real value(!is.null(inversemat)) then return it to me 
  }
  data <- x$get() ##Finally we need to ask from the cache memory about the inverse of the matrix and return the searched value to keep working with that input
  inversemat <- solve(data, ...)
  x$setinverse(inversemat)
  inversemat
}



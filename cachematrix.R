## Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix 
# rather than computing it repeatedly.

## makeCacheMatrix creates a list containing a function to :
# 1-set the value of a matrix
# 2-get the value of a matrix
# 3-set the value of the inverse of a matrix
# 4-get the value of the inverse of a matrix

##This function introduces functions to set and get the value of the matrix 
## and its cached inversed value.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
  get <- function() x
setinverse <- function(inverse) inv <-inverse
getinverse <- function() inv
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}
## This function tries to get the cached inversed value of the matrix, 
## if the value is not yet cached, it calculates the inversed value and caches it.
## Then returns the value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

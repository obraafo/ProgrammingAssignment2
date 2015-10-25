## Wrapper functions for matrices that caches the inverse computation as an optimization
## for repeated. This is useful where the inverse is computed multiple times
## and is potentially expensive for large matrices

## Constructs a wrapper around a matrix to cache the inverse computation
## This function returns a list with the following attributs
## 1. get : a function that returns the underlying matrix
## 2. set : a function that sets the underlying matrix
## 3. getInverse : returns the cached inverse
## 4. setInverse : sets the cached value of the inverse of the underlying matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setInverse = setinv, getInverse = getinv)
}


## Calculates the inverse of matrix x. It fetches the value from cache if available
## else it computes on demand and stores in cache to speed up subsequent calls

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("geting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

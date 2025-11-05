## These functions work together to cache the inverse of a matrix.
## 
## makeCacheMatrix creates a special "matrix" object that can store
## a matrix and cache its inverse. It provides functions to set and get
## the matrix and to set and get the cached inverse.
##
## cacheSolve computes the inverse of the special "matrix" created by
## makeCacheMatrix. If the inverse has already been computed and cached,
## it retrieves the cached inverse to avoid redundant calculations.
## Otherwise, it calculates the inverse, stores it in the cache, and returns it.

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix, and to set and get
## its inverse. The cache avoids recalculating the inverse if the matrix has
## not changed.

  inv <- NULL  # cache for the inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset the cache when matrix changes
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## cacheSolve:
## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix. If the inverse has already been calculated and cached,
## it retrieves the cached value to save computation time. Otherwise, it
## calculates the inverse using solve() and stores it in the cache.
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # compute the inverse
  x$setinverse(inv)       # cache it
  inv
}


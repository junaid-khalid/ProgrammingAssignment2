## These function are built on the template provided in form of vectors functions

## makeCacheMatrix() contains set,get,setinverse and getinverse functions. If contents of a matrix are not changed it returns the same matrix stored in cache.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse1) inverse <<- inverse1
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function gets the matrix through getinverse function and returns inverse of the fetched matrix and stores the result through setinverse function
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

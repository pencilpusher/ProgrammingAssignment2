## The following functions provide a cache-able inverse of a 
## matrix (assuming that matrix is square and invertible).

## Creates an ehanced matrix object that has the ability to 
## cache its inverse.
## It will store the inverse on a call to setinverse and will
## clear that inverse value when set is called to update the
## matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates the inverse of an enhanced matrix returned by 
## calling the makeCacheMatrix. 
## The calculated inverse is returned on the first call to 
## this function, or the first call after the matrix has been 
## updated. 
## Subsequent calls will returned the cached inverse value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
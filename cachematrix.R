## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is intended to create a matrix object that can cache its inverse.
## cacheSolve will compute the inverted matrix, or retrieve it if it is already calculated.

## Write a short comment describing this function
## see above
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## see above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getinverse()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
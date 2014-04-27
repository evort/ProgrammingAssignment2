## Functions that allows caching result of matrix inverse 
## instead of computing it again.


## Creates special type of "matrix" object that can cache its inverse.
## It really is a list containing functions:
## - set - set a value of the matrix 
## - get - get a value of the matrix 
## - setinv - set a value of the inverse of matrix 
## - getinv - get a value of the inverse of matrix 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## Function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Create a makeCacheMatrix object with getter and setter methods to cache time consuming 
## matrix inverse calculations, using R scoping rules.

## Create the new object with data and methods.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(minverse) inverse <<- minverse
  get_inverse <- function() inverse
  
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Takes as argument a makeCacheMatrix object, calculate the inverse and cache it.
## At further calls on the same makeCacheMatrix object returns the cached inverse instead that re-calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$get_inverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

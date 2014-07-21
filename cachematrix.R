## To reduce computing load, these 2 functions will reduce the amount of matrix
## inverse that need to be computed by creating special matrices that can
## cache any computed inverse

## makeCacheMatrix() converts a matrix x into a special matrix that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## We start by setting the cached inverse as NULL
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  ## Output the special matrix, which is a list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve takes the special matrix and outputs its inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## If a non-NULL inverse matrix is cached, return the inverse and terminate
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Else, compute the inverse and return the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

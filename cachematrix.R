## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## functions do.
## This function creates a special "matrix" object that can cache its inverse.
## First step is to create a makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cache solve should retrieve the 
## inverse from the cache.
## This is the second step.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
 
  inv <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inver)
  inver
}


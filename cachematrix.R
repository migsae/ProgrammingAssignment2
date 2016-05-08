## Inverse of a Matrix from cache:
## Reduce computational cost of calculation matrix inverse by
## retreiving values from cache instead or re-calculating 
## if possible

## 1) Creates matrix object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 2) Computes matrix inverse created by makeCacheMatrix. 
## If inverse was calculated before retrieve inverse from cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
}

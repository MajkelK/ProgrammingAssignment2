## These two functions are intended to cache the computed
## inverted matrix, which can be then retrieved without the need
## to compute it again.

## The first function, makeCacheMatrix, creates a list of functions used to:
## 1. set the value of matrix;
## 2. get the value of matrix;
## 3. set the value of inverted matrix;
## 4. get the value of inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve function calculates inverted martix. It first checks 
## if it has been calculated already - in which case it skips calculation
## and returns inverted matrix from cache.
## If inverted matrix was not calculated yet, it is stored in cache.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

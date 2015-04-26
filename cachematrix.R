## Functions to cache the Inverse of a Matrix and
## then get the inverse using the cache if it exists
## or else calculate it again if the matrix changes.


## Creates an object that can be used to store and
## retrieve a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse matrix or
## retrieves it from the cache variable of the
## above object if it already exists.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  i
}
## Matrix inversion could turn out to be a costly operation, so caching the inverse,
## rather than recomputing it saves time.

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes and caches the inverse of the special "matrix" returned by
##makeCacheMatrix above, if inverse already not cached and matrix not changed.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting cached inverse matrix!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  return(inv)
}

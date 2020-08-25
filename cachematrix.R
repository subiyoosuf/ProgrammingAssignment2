## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## to create a matrix and get its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL      ## initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}       ## to get matrix x  
  setinverse <- function(mean) {inv <<- mean}
  getinverse <- function() {inv}   ## to get inverse of matrix x
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
## used to get cache data

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
  if(!is.null(inv)) {            ## checking if inverse is NULL
    message("Retrieving the cached data")
    return(inv)                  ## returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

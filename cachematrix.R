## This functions are used to cache inverse of matrix, so if we have to calculate inverse of
## the same matrix repeatedly then this functions are helpful

## This function takes a matrix as argument and returns a list. This function caches the
## matrix provided and also stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inver) inv <<- inver
  getInverse <- function() inv
  
  list(set = set , get = get , setInverse = setInverse , getInverse = getInverse)
}


## This function returns inverse of matrix, if the inverse of the matrix has already been
## evaluated then it returns the cached inverse or else it solves for the inverse and caches
## the inverse for the matrix for further use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data , ...)
  x$setInverse(inv) 
  return(inv)
  
  
}

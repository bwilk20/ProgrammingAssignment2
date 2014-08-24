## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( mx = matrix() ) {
  ## Initial inverse property
  ip <- NULL
  ## set matrix
  set <- function( matrix ) {
    mx <<- matrix
    ip <<- NULL
  }
  ## get matrix
  get <- function() {
    ## Return the matrix
    mx
  }
  ## set inverse of matrix
  setInverse <- function(inverse) {
    ip <<- inverse
  }
  ## get inverse of matrix
  getInverse <- function() {
    
    ## return inverse property
    ip
  }
  
  ## return list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## inverse matrix of x
  mx <- x$getInverse()
  ## return matrix
  if( !is.null(mx) ) {
    return(mx)
  }
  ## load matrix
  data <- x$get()
  ## Compute inverse
  mx <- solve(data) %*% data
  ## set inverse
  x$setInverse(mx)
  
  ## Return matrix
  mx
}

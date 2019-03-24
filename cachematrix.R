## The following pair of functions will store and cache the inverse of a matrix

## The function makeCacheMatrix creates an object that can store a matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInvMatrix <- function(inv) i <<- inv
  getInvMatrix <- function() i
  
  list(set = set,
       get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Computes the inverse of the stored matrix. If the inverse has been already computed,
## the function will return the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInvMatrix()
  
  if ( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setInvMatrix(i)
  i
}

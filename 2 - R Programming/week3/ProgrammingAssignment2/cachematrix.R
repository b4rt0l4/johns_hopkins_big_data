## Creates a specia matrix that can store in values
## about its inverse on a cache.


## makeCacheMatrix creates the special matrix.
makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize inversecache to NULL
  inversecache <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inversecache <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # assign a matrix on inversecache
  setinverse <- function(inverse) inversecache <<- inverse
  # get the stored matrix on inversecache
  getinverse <- function() inversecache
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gets the inverse of a special matrix created with makeCacheMatrix.
## If the value of the inverse matrix its stored on inversematrix variable
## return that value, otherwise calculates the inverse, store it on 
## inversecache and return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## try to get the inverse stored in cache
  inverse <- x$getinverse()
  
  # return inverted matrix if its stored on chache, otherwise
  # create the matrix in working environment
  if (!is.null(inverse)) {
    message("getting cached data")
    
    # display matrix
    return(inverse)
  }
  
  # Calculate the matrix, store on cache and return it.
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  return (inverse)
}

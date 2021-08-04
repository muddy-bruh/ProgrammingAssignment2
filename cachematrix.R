
## The following function creates an object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL ## initializing as NULL
  set <- function(y) ## function to set value of matrix in parent env
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## get function, returning the value of the matrix
  setInverse <- function(inverse) inv <<- inverse ##assign value of inv in parent env
  getInverse <- function() inv ##getting the value of inv upon being called
  list(set = set, 
       get = get,
       setInverse = setInverse,  ##command allows for functions to be referred with $
       getInverse = getInverse)
}

## The following computes calculates the inverse if it hasn't been calculated 
## and retrieves the inverse if it already has
cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()
  if (!is.null(inv)) 
  {
    message("getting inversed matrix (cached data)")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
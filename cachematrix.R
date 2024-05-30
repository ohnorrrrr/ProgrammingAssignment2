## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse property when the matrix is set
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse property
  
  getInverse <- function() inv  # Return the inverse property
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse if it exists
  
  if (!is.null(inv)) {  # Check if the inverse is already cached
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse of the matrix
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}

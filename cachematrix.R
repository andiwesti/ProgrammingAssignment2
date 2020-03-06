## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create function that includes the get and set functions that either
# retrieves current matrix stores new matrix in the environment.
# The getinverse and setinverse functions does the same for the 
# inverse matrix.
# If a new matrix is stored in the environment the cached inverse
# matrix is set to NULL

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function will either retrieve the cached inverse matrix
# or if there is no cached inverse matrix calculate the inverse
# matrix and store it in the environment. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

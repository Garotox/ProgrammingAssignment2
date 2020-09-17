## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a matrix (x) and initially sets its inverse as NULL. When cacheSolve is invoked, the initial matrix inverse is set.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
# This function takes a matrix done within the makeCacheMatrix environment and returns the matrix inverse and sets it in the cache (if it was not there already)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse
  if(!is.null(inv)) {
    message('getting cached data')
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

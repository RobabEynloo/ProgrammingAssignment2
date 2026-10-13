## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function keeps a matrix and its inverse together
## so we dont have to calculate the inverse every time

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(value) {
    inv <<- value
  }
  
  getinverse <- function() {
    inv
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  # return all four functions in a list
}



## Write a short comment describing this function


## This function calculates the inverse of a matrix
## if we already calculated it, just use the saved one

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # get the matrix
  data <- x$get()
  
  # calculate the inverse
  inv <- solve(data, ...)
  
  # save it for next time
  x$setinverse(inv)
  
  # return the result
  inv
}
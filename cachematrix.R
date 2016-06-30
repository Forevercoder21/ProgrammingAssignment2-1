## Put comments here that give an overall description of what your
## functions do
## Two functions that cache the inverse of a matrix


## Write a short comment describing this function
## makeCacheMatrix:
## Creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## Initialization
  m <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() {
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i << inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    m
  }
  
  ## Return a list of the methods
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}



## Write a short comment describing this function
## cacheSolve: 
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been set then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse which is already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}

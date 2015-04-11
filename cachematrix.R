## Programming Assignment 2 - Cache the inverse of a matrix

## his function creates a special "matrix" object that can cache its inverse

## makeCacheMatrix creates a list containing a function to
##  set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set function, to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get function for the matrix, returns the underlying matrix
  get <- function() x
  
  ## setinverse function, sets the inv variable that contains the inverse of the matrix
  setinverse<- function(inverse) inv <<- inverse
  
  ## getinverse function, gets the inv variable that contains the inverse of the matrix
  getinverse <- function() inv
  
  ## create list with the functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## The code doesnt check for matrices that are singular, which cannot be inverted. Such matrices will throw an error from the Solve method
## also this function only works on the special matrix created by the makeCahceMatrix method.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## First step is to try and retrieve from the cache
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached matrix data")
    return(inverse)
  }
  
  ## if we are here, we didnt find the data in cache. This is the first time we are seeing this matrix
  ## We will get the original matrix using the get function
  data <- x$get()
  
  ## calculate the inverse using solve method
  inverse <- solve(data, ...)
  
  ## put the inversed matrix into the cache using the setinverse function. This enables future retrievals from cache.
  x$setinverse(inverse)
  
  ## return the inverse
  inverse
}
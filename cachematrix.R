## The following functions create a cachematrix to store the cached matrix and
## a function to calculate the actual inverse of the matrix

## makeCacheMatrix accepts a matrix as a parameter and defines a list of 
## set, get functions and setinv and getinv functions to set/return the matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(im1) inv_m <<- im1
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a defined matrix list as parameter, checks if the inverse
## matrix exists in the cache and returns the cache data. If the cache does
## not exist then it will be calculated, set in the cache matrix and then returned
## Concept of lexical scoping of R has been used to ensure that based on 
## defined scope, the function is used for storing the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data_m <- x$get()
  inv_m <- solve(data_m)
  x$setinv(inv_m)
  inv_m
}
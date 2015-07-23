## makeCacheMatrix sets up the matrix and defines the required functions
## cacheSolve returns the inverted matrix from Cache or by calculation

## makeCacheMatrix would take a matrix as its argument.A series of functions
## set, get, setinv and getinv are defined
## Lexical scoping feature of R is used to set the inverse matrix at the 
## global/parent level

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


## CacheSolve checks to see if the value for Inverse matrix already exists for
## this matrix. With lexical scoping of R, the Inverse matrix value would already
## be set for a given matrix in Cachematrix if it was invoked for this matrix once.
## If it is not in cache, then inverse is calculated using solve function 
## and this inverse is stored into the CacheMatrix object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data_m <- x$get()
  "ma"
  inv_m <- solve(data_m)
  x$setinv(inv_m)
  inv_m
}
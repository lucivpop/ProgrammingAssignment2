## With these two functions we can create an object
## that stores a matrix and cache's its inverse

## 1. makeCacheMatrix
## receives a matrix
## returns a list object
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  # set - cache the received matrix
  # get - return the cached matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  
  # setSolve - cache the inversed matrix
  # getSolve - return the inversed cached matrix
  setsolve <- function(inversed.x) s <<- inversed.x
  getsolve <- function() s
  
  # returns a list object
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## 2. cacheSolve 
## receives an object (x) of type list and 
## returns the inverse of the matrix from object x
cacheSolve <- function(x, ...) {
  
  # read the cached inversed matrix
  s <- x$getsolve()
  
  # if the cached matrix is not null 
  # return that inversed matrix
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  # else, it gets the matrix and store it in data
  # and invert it and cache it
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  ## Returns the inversed matrix 
  s
}

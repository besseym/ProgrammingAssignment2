## Put comments here that give an overall description of what your
## functions do

## This function creates an object that takes and stores a matix 
##     as well as the solved inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function takes a matrix, produced with makeCacheMatrix, 
##     and argumets used to solve the inverse of the matrix.
## It first checks to see if the inverse has already been solved, if so, 
##     it gets the inverse from the cache and skips the computation. 
##     Otherwise, it calculates the inverse of the data and sets the value of the inverse 
##     in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

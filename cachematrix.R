## This function creates cached data for matrix inversion- if we require the inversion of the same matrix over and over, 
## this function will calculate the inv. matrix value once and store it as cached data
## after this first calculation, next use of this function on the same matrix will not require any calculation

## makeCacheMatrix() receives a matrix as argument, then it will create a list that stores two values-  The matrix x and the inverted matrix i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMat <- function(y) { ## setting x
    x <<- y
    i <<- NULL
  }
  getMat <- function() x 
  setInv <- function(Inv) i <<- Inv ## setting i
  getInv <- function() i
  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## cacheSolve() receives the list returned by makeCacheMatrix() and return the value for the inverted matrix i
## if this is the first call for a certain matrix' the function will calculate the result i, cache it and return it
## then, every call for the same matrix, no calculation will be needed. the function will return the cached i

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {  ##checking if the value is already exist in memory
    message("getting cached data")
    return(i)
  }
  data <- x$getMat() 
  i <- solve(data) ## inv calculation
  x$setInv(i)
  i ## return a matrix that is the inverse of 'x'
}


## makeCacheMatrix creates a matrix 
## x is the input to the matrix
## matrix inverse value "mi" is set to null
## every reference of "mean" from the example code is changed to "matrixinverse"

makeCacheMatrix <- function(x = matrix()) {
mi<-NULL
set <- function(y)
{
  x <<- y
  mi <<- NULL
}
get <- function() x
setmatrixinverse <- function(matrixinverse) mi <<- matrixinverse
getmatrixinverse <- function() mi
list(set = set, get = get,
     setmatrixinverse = setmatrixinverse,
     getmatrixinverse = getmatrixinverse)
}

## every reference of "mean" from the example code is changed to "matrixinverse"
##every reference of "m" from the example code is changed to "mi"

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  mi <- x$getmatrixinverse()
  if(!is.null(mi)) {
    message("cache the inverse of the matrix")
    return(mi)
  }
  data <- x$get()
  mi <- matrixinverse(data, ...)
  x$setmatrixinverse(mi)
  mi
}

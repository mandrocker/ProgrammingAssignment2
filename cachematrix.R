## the make cache matrix creates a wrap object around the input matrxi
## get and set are storing the input matrix while getInverted 
##    and setInverted are storing the calculated inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  get <- function() x
  set <- function(y){
    x <<- y
    ix <- NULL
  }
  getInverted <- function() ix
  
  setInverted <- function(inverted){
    ix <<- inverted
  }
  
  list(set = set, get = get, setInverted = setInverted, 
       getInverted = getInverted)
}

## cacheSolve returns the inverted matrix if already cached in the wrapped matrix.
##  otherwise calculates the inverted matrix and stores it in the wrapper for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ix <- x$getInverted()
    if(!is.null(ix)){
      message("retrieving from the cache")
      return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setInverted(ix)
    ix
}

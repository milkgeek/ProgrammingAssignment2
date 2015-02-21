## The following two functions are use to save compute time 
## by caching inverse of a matrix once it has been initially calculated
## and retrieving the cached value if it has been calculated previously.

## makeCacheMatrix is a special matrix or really just a list with four functions.
## The only important note is that in both set functions the assignments are to the
## definitions in parent environment.
makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setinv <- function(solve) Inverse <<- solve
  getinv <- function() Inverse
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## cacheSolve is a function that checks if the inverse has previously been calculated
## and returns the cached inverse if it has and calculates it if it has not.
## I've added an optional parameter to force calculation regardless.
cacheSolve <- function(x,forcecalc = FALSE, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!forcecalc){
    i <- x$getinv()
    if(!is.null(i)){
      message("retrieving cached inverse")
      return(i)
    }
  }
  message("calculating inverse")
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}

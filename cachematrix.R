## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function is responsibe for storing (cache) the values obatined in the CasheSolve function. 
# That is why we need to use the "<<-" operator, so that they share an environment and the
# Functions are able to read their common variables, such as "inv"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # null (in case CacheSolve has not been used)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x           
  setmatrix <- function(inverse) inv <<- inverse
  getmatrix <- function() inv   #getmatrix is the variable in which the inverse is going to
  list(set = set, get = get,    # be stored in, and it is retrieved by the solveMatrix function
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function obtains the inverse of the argument matrix either from solving it
## or retrieving from the cached value in makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getmatrix()
  if(!is.null(inv)) {   #if inv is not NULL (if there is already a stored value in MakeCacheMatrix)
    message("getting cached data")    #the function simply returns the cached result
    return(inv)
  }
  mat <- x$get()                     # if inv is null, cacheSolve obatains the inverse of
  inv <- solve(mat, ...)             # the matrix x and caches it in the makeCacheMatrix
  x$setmatrix(inv)
  inv
      
}

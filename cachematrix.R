##My program is a pair of functions that cache the inverse of a matrix. Since
##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly.

## The "makeCacheMatrix" function is used to set a matrix that can be inversed
## in the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  Function_a <- function(y) {
    x <<- y
    temp <<- NULL
  }
  Function_b <- function() x
  Function_aInverse <- function(inverse) 
    temp <<- inverse
  Function_bInverse <- function() temp
  list(Function_a = Function_a,
       Function_b = Function_b,
       Function_aInverse = Function_aInverse,
       Function_bInverse = Function_bInverse)
}

## The following function is used to get the inverse matrix for the matrix that
## we created in the first function. Then, the program will print out the result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  temp <- x$Function_bInverse()
  if (!is.null(temp)) {
    message("getting cached data")
    return(temp)
  }
  temp2 <- x$Function_b()
  temp <- solve(temp2, ...)
  x$Function_aInverse(temp)
  return(temp)
}

## This exercise uses two functions.  The first creates a matrix list with the solved matrix function. The second function determines whether or not the cached matrix is the same as one being tested, and if it is, then it passes the already solved matrix as the solution.

## This function solves the matrix and caches its inverse function using the 
## solve() function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- m
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function returns the solution to the solved matrix.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m        
}

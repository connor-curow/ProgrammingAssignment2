# My pair of functions gets the inverse of a matrix and returns that inverse

## Making matrix and specifying inverse; <<- operator is used to assign values
## in parent environment instead of global environment, ensuring the result
## will update each time the function runs

makeCacheMatrix <- function(x = matrix(1:25, 5, 5)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Returning inverse of matrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Returning inversed matrix...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
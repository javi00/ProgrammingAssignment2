## Matrix stored  cache

##creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = solve,
       getsolve = solve)
}


## inverse of a square matrix

cacheSolve <- function(x, ...) {
       
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$solve()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

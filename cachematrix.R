## Caches the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #sets the cached matrix
  setmatrix <- function(matrix) m <<- matrix
  #gets the cached matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Checks to see if the matrix is cached. If it is, it returns the cached
## inverse. If not, it calculates, caches, and returns the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  #checks if matrix is cached and, if so, returns the cached data 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not cached, calculates and returns the inverse of the matrix
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
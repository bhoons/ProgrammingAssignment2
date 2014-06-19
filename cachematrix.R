## The following functions code is an example to show the scoping functionality in R Programming
## The function(s) when called with a matrix returns the inverse of the matrix from the cache is already exists
## If not then it computes the inverse and stores in cache for future retrival.

## Function: makeCacheMatrix -> It caches the inverse of the matrix  
makeCacheMatrix <- function(x = matrix()) {
  mymatrix <- NULL
  set <- function(y) {
    x <<- y
    mymatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mymatrix <<- inverse
  getinverse <- function() mymatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function:cacheSolve -> This function gets the inverse of the matrix cached if it already exists.
##                        if not, then it computes the inverse and then sets the value of the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  else
  {
    message("Getting Un-cached data...")
    m <- solve(x$get())
    x$setinverse(m)
  }
  m
}
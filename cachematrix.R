## The following functions code is an example to show the scoping functionality in R Programming.
## The function(s) below are used to return the inverse of the matrix from the cache if already exists.
## If not, then it computes the inverse and stores in cache for future retrival.

## Function: makeCacheMatrix -> It caches the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize expression 
  mymatrix <- NULL
  
  ## method to get and set matrix
  set <- function(y) {
    x <<- y
    mymatrix <<- NULL
  }
  
  get <- function() x
  
  ## methods to get and set the inverse
  setinverse <- function(inverse) mymatrix <<- inverse
  getinverse <- function() mymatrix
  
  ## List function to return the name-value pair of the methods 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function: cacheSolve -> This function gets the inverse of the matrix cached if it already exists.
##                        if not, then it computes the inverse and then sets the value of the cache.

cacheSolve <- function(x, ...) {
  ## Call method getinverse() from 'makeCacheMatrix' function
  m <- x$getinverse()
  
  ## check if matrix inverse is cached and return if exists
  
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  ## Compute Matrix remote and set the value of m
  
  else
  {
    message("Getting Un-cached data...")
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
  }
  m
}
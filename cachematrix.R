## Put comments here that give an overall description of what your
## functions do

## Cache a matrix and its inverse via a list of getting and setting functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## returns original input x
  get <- function() x
  ## store the cache variable for the inverse
  setinv <- function(inverse) inv <<- inverse
  ## get the stored inverse
  getinv <- function() inv
  ## return list with caching functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retrieve the cached matrix inverse or calculate it and store it otherwise

cacheSolve <- function(cache, ...) {
  inv <- cache$getinv()
  if(!is.null(inv)){
    message("retrieving cached inverse")
    return(inv)
  }
  ## retrieve matrix
  data <- cache$get()
  ## solve for inverse
  inv <- solve(data, ...)
  ## store inverse
  cache$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}

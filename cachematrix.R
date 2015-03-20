## cache the inverse of a matrix

# creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inversed <<- solve
  getinverse <- function() inversed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse_cached <- x$getinverse()
  if(!is.null(inverse_cached)) {
    return(inverse_cached)
  }
  data <- x$get()
  inverse_cached <- solve(data, ...)
  x$setinverse(inverse_cached)
  inverse_cached
}

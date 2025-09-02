## Function to create a special "matrix" object with cache for inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset inverse cache when matrix changes
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to compute the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If cached inverse exists, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute inverse
  mat <- x$get()
  inv <- solve(mat, ...)   # compute inverse
  x$setinverse(inv)        # cache it
  inv
}

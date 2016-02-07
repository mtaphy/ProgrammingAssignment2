## The function makeCacheMatri(x = matrix()) creates a special "matrix" that 
## stores the matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverseM <- NULL
      set <- function(y){
            x <<- y
            inverseM <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverseM <<- solve
      getinverse <- function() inverseM
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}

###  The following function calculates the inverse of the special "matrix" created by the above function.
### It first checks to see if the inverse has already been calculated. 
### If so, it gets the inverse from the cache and skips the computation. 
### If the inverse if not calculated, it calculates it and sets the value of the inverse 
### in the cache via the setinverse() function.

cacheSolve <- function(x, ...) {
      inverseM <- x$getinverse()
      if(!is.null(inverseM)) {
            message("getting cached data")
            return(inverseM)
      }
      data <- x$get()
      inverseM <- solve(data, ...)
      x$setinverse(inverseM)
      inverseM
}

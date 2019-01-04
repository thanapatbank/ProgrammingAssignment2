## This function will collect and compute inverse matrix. Also save value as cache for faster execution when the same value called in second time.

## makeCacheMatrix should be first call to set a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinverse <- function(minv) inv <<- minv
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will call after set matrix in makeCacheMatrix to make inverse matrix in first time. If this function will in second time with
## same value it will get cached data and return back directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <-  solve(data)
        x$setinverse(inv)
        inv
}

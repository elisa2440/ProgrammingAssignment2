## The first function makes a list of functions to cache the inverse of a matrix 
## and the second function returns the inverse of a matrix from cache if its there 
## or it calculates it if not.

## Caches the inverse of a matrix
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Reuturns the inverse of a matrix from cache is available or it calculates it with
## ginv() function from MASS package

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- ginv(data)
        x$setinv(inv)
        inv
}

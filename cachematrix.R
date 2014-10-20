## These functions perform computation and caching
## of the inverse of the input matrix.
##
## Example: To create a makeCacheMatrix:
## > m1 <- makeCacheMatrix(matrix( c(4,3,3,2), 2, 2) )
##
## Example: To get an existing makeCacheMatrix
## > m1$get()
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
##
## Example: To get the inverse of an existing makeCacheMatrix 
## (note that fetches subsequent to the first fetch return 
## the cached ## value)
## > cacheSolve(m1)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(m1)
## getting cached data
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## If the inverse has not been cached, computes, caches
## and returns the result.  If it is cached, returns the
## cached value.

cacheSolve <- function(x, ...) {
        Solve <- x$getinverse()
        if(!is.null(Solve)) {
                message("getting cached data")
                return(Solve)
        }
        data <- x$get()
        Solve <- solve(data, ...)
        x$setinverse(Solve)
        Solve
}

## Cache the inverse of a matrix

## makeCacheMatrix: create a special "matrix" object, which is really 
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
## 
## Parameters: the invertible matrix
## Return value: the list of functions
## Example:
## > x <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
## > x$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## Note: the input matrix is assumed to be invertible

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(matrix) {
                x <<- matrix
                cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: compute the inverse of the special "matrix" object 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache
##
## Parameters: the special "matrix" object
## Return value: the inverse matrix
## Example: 
## > x <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("Cache hint")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}

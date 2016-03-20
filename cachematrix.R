## Cache the inverse of a matrix

## Create a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matrix) {
                x <<- matrix
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Cache hint")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}

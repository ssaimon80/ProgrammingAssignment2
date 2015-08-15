## Cached inverse matrix computation.
## Use makeCacheMatrix() or makeCacheMatrix(x) to create a matrix with inverse 
## caching. 
## Call cacheSolve() on the result of makeCacheMatrix() to compute the matrix
## inverse. The computation is reused in subsequent calls to the function cacheSolve()
## until the matrix is changed with the set() command.

## The function initializes a special "matrix" object which can cache the 
## inverse calculation.
## The object is represented as a list with four elements named 
## "set", "get", "setinverse" and "getinverse" which represent the four
## basic operations that can be performed on the "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_mtx) {
            inverse <<- inverse_mtx
        }
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## The function computes the inverse of a matrix, provided that the matrix x is
## represented as a special "matrix" object returned by makeCacheMatrix().
## If the inverse has already been computed the cached matrix is returned.
## The additional arguments are passed directly to the solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following pair of functions cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
        return(inverse)
        }
        data <-x$get()
        inverse <- solve(data)
        x$setinv(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}

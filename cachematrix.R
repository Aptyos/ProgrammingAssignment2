## The 2 functions "makeCacheMatrix" and "cacheSolve" 
## can be combined to avoid iterations to retrieve the inverse of a same matrix
## Either the inverse has been previously calculated, and it is returned from the cache
## or, it is calculated.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        sm <- NULL
        set <- function(y) {
                x <<- y
                sm <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) sm <<- solve
        getsolve <- function() sm
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" returned by "makeCacheMatrix"
# if it has not been already been calculated; otherwise it returns the result from the cache.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        sm <- x$getsolve()
        if(!is.null(sm)) {
                message("getting cached data")
                return(sm)
        }
        data <- x$get()
        sm <- solve(data)
        x$setsolve(sm)
        sm
}
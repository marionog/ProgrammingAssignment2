## This function creates a special "matrix" object that can cache its inverse.

##  The function makeCacheMatrix creates a special "vector", which is really a 
## list containing a function to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse matrix
# - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function cacheSolve calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse of the
## matrix has already been calculated. If so, it gets the result from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets its value in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

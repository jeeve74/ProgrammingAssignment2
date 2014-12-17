## These two functions that are used to create a special object that stores a matrix
##      and cache's its inverse.

## makeCacheMatrix creates a special "vector" which is really an object that:
##      sets the value of the vector
##      gets the value of the vector
##      sets the value of the inverse
##      gets the value of the inverse

makeCacheMatrix <- function(origMatrix = matrix()) {
        invMatrix <- NULL
        set <- function(newMatrix) {
                origMatrix <<- newMatrix
                invMatrix <<- NULL
        }
        get <- function() origMatrix
        setSolve <- function(solve) invMatrix <<- solve
        getSolve <- function() invMatrix
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve calculates the inverse of the special "vector" created with the above function. 
##      If the inverse has already been calculated, then it gets the inverse from the cache 
##      and skips the computation. Otherwise, it calculates the inverse
##      and sets the value of the inverse in the cache via the setSolve function.

cacheSolve <- function(origMatrix, ...) {
        invMatrix <- origMatrix$getSolve()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- origMatrix$get()
        invMatrix <- solve(data, ...)
        origMatrix$setSolve(invMatrix)
        invMatrix
}
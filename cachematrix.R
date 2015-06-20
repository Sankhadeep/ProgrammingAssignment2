## Put comments here that give an overall description of what your
## functions do

## The function will create a list which will:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setInv <- function(inv) z <<- inv
    getInv <- function() z
    list(set = set,
        get = get,
        setInv = setInv,
        getInv = getInv)
}


## This function will calculate the matrix inverse using the function makeCacheMatrix. 
##The following function will use the cached result if it is available.

cacheSolve <- function(x, ...) {
    z <- x$getInv()
    if(!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    m <- x$get()
    z <- solve(m, ...)
    x$setInv(z)
    z
}

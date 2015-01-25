## The two functions defined below are to be used to inverse the given matrix
## and store the result of computation in cache. The cached value will be
## returned if given the same matrix as for the cached inverse

## Usage example:
## > x <- matrix(rnorm(16),4,4)
## > cachedX <- makeCacheMatrix(x)
## > cacheSolve(cachedX)

## The function creates a special matrix type to store it's inverse internally

makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    set <- function(y) {
        x <<- y
        inv <<- matrix()
    }
    get <- function() x
    setInv <- function(invMatr) inv <<- invMatr
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The function calculates the inverse of the special "matrix" if necessary
## or returns the stored result if created previously

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!identical(inv, matrix())) {
        message("getting cached data")
        return(inv)
    }
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setInv(inv)
    inv
}
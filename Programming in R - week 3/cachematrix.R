## This set of functions is used to store a special "matrix" object and then
## cache its inverse. This is advantageous because computing the inverse of a
## matrix is time consuming. Being able to retrieve the inverse from memory
## quickly can end up saving a lot of time.

## makeCacheMatrix takes a square invertible matrix, X, and stores it as a
## special object that can be used to cache the inverse of X.

makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    set <- function(Y) {
        X <<- Y
        inv <<- NULL
    }
    get <- function() X
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve returns the inverse of a matrix object create by makeCacheMatrix.
## If the inverse has already been calculated, it will be retrieved from the
## cache instead of being computed by the solve function.

cacheSolve <- function(X, ...) {
    inv <- X$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data, ...)
    X$setinv(inv)
    inv
}

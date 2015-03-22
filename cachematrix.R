## This function creates a  "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    inv <- NULL
    set <- function(x) {
        mtx <<- x;
        inv <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inv <<- inv;
    getinv <- function() return(inv);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function is created to computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(mtx, ...) {
    inv <- mtx$getinv()
    if(!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    data <- mtx$get()
    inv <- solve(data, ...)
    mtx$setinv(inv)
    return(inv)
}

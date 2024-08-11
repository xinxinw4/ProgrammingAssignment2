## This pair of functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv_m <<- inverse
    getinv <- function() inv_m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes and returns the inverse of the special "matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    if (identical(x$get, x$set)) {
        inv_m <- x$getinv()
        if (!is.NULL(inv_m)) {
            return(inv_m)
        }
    }
    else {
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setinv(inv_m)
    return (inv_m)
    }
}


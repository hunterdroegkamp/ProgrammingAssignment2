## A pair of functions used to create a special object that stores a matrix
## and caches its inverse

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) m <<- inverse
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Computes the inverse of the matrix from makeCacheMatrix, or retrieves it
## from the cache if already computed

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}

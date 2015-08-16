## makCacheMatrix - returns a list of fumctions that operate on a cached matrix.
## it provides the following functions:
##      get     --- returns the cached Matrix
##      set     --- Caches the Matrix and sets the inverse to NULL
##      setinv  --- Caches the invers of the Matrix
##      getInv  --- returns the cached matrix inverse



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(xxx) inv <<- xxx
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Cachesolve returns the cached inverse of a matrix.
## If the matrix inverse was never cached before, the inverse will be computed and stored


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## Check if the inverse was already cached
        if(!is.null(inv)) {
                message("getting cached matrix inverse ")
                return(inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

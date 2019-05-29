## These functions manage a Matrix with a cache of its inverse state.

## Creates a Matrix capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL    # Inverse cache
    set <- function(y) {    # Set new value to matrix, delete cache
        x <<- y
        i <<- NULL
    }
    get <- function() x    # Simply returns the value of x
    
    setinverse <- function(y) i <<- y
    getinverse <- function() i
    
    list(    # Returns a list of functions to get and set the matrix and its inverse
         set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}


## Returns the inverse of the supplied CacheMatrix. If a cached version is available it will be
## used, otherwise one will be created.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()    # Returns the current cache
    if(!is.null(i)) {    # If there is a cache, display a message and return it
        message("Retrieving from cache")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)    
    x$setinverse(i)    # Calculate the inverse of the data and save it to the cache, then return it
    return(i)
}

## The following functions will take a matrix as input and invert it.
## The inverted matrix will then be stored in a function to serve as 
## as cache. Any additional matrixes passed to the functions to be 
## inverted if not cached.
## makeCacheMatrix contains four methods
##    `set` - stores the invertible-matrix
##    `get` - retrieves the invertible-matrix
##    `setinv` - stores the inverse-matrix
##    `getinv` - retrieves the inverse-matrix
## To use create an object of type makeCacheMatrix
## ex: for the invertible matrix
##     [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## m = matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
## cache.matrix <- makeCacheMatrix(invertible.matrix)

## Example output can be viewed through the cacheTestMatrix() 
## function.

## Create a list of functions to set and get the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
        setinv = setinv,
        getinv = getinv)
}

## If the matrix has been inverted, use a cached variable form 
## the makeCacheMatrix function, otherwise invert the matrix 
## and store it in the makeCacheMatrix function.
cacheSolve <- function(x, m, ...) {
    inv <- x$getinv()
if(!is.null(inv)
    & is.matrix(m)
        && is.matrix(x$get())
        && dim(m) == dim(x$get())
        && all(m == x$get())
  ) {
    message("getting cached data")
    return(inv)
  } else {
    x$set(m)
    message("updating cached matrix")
  }
  
    message("updating cached matrix inverse")
    datamatrix <- x$get()
    inv <- solve(datamatrix, ...)
    x$setinv(inv)
  
  inv
}

## The following code will iterate through the creation 
## and function calls for several matrixes to demonstrate
## the cache.
cacheTestMatrix <- function() {
    ## new cache object
    cache.matrix <- makeCacheMatrix(m)
    ## new invertible matrix
    m = matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
    print(cacheSolve(cache.matrix, m))
    
    ## incompatible matrix to cache comparison
    m = matrix(c(7, 3, 2, 6, 2, 8, 3, 9, 1), nrow = 3, ncol = 3)
    print(cacheSolve(cache.matrix, m))
    print(cacheSolve(cache.matrix, m))
    print(cacheSolve(cache.matrix, m))

    ## compare cache to new non-equal invertible matrix
    m = matrix(c(0, 9, 5, 4), nrow = 2, ncol = 2)
    print(cacheSolve(cache.matrix, m))

    ## new invertible matrix with empty cache
    m = matrix(c(1, -1, -1, -1), nrow = 2, ncol = 2)
    cache.matrix <- makeCacheMatrix(m)
    print(cacheSolve(cache.matrix, m))

    ## new invertible matrix equal to cache
    m2 = matrix(c(1, -1, -1, -1), nrow = 2, ncol = 2)
    print(cacheSolve(cache.matrix, m2))
}

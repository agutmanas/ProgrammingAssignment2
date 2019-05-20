## The two functions, makeCasheMatrix and cacheSolve, set a matrix x, 
## solve its inverse x^(-1) and cache the result. If matrix is reset, the 
## cache of the inverse is cleared

## makeCacheMatrix creates methods to set and get the value of the matrix, 
## and to set (but not solve) its inverse into cache and retrieve the cached 
## inverse. The function returns a list of the four methods: 
## (set, get, setinverse and getinverse)

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(y_inv) x_inv <<- y_inv
    getinverse <- function() x_inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve first checks if an inverse of a function is already stored
## in the cache, 
## If the cache is NULL, the inverse is calculated and stored in the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinverse()
    
    if (!is.null(x_inv)) {
        message("Returning cached data")
        x_inv
    }
    
    x_inv <- solve(x$get())
    x$setinverse(x_inv)
    x_inv
    
}

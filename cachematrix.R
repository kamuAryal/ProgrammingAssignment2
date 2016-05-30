## This R script contains functions to store and inverse a matrix

## makeCacheMatrix returns a special "matrix" object that can cache itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinverse = function(solve) m <<- solve
    getinverse = function() m
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x', the special "matrix" object defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m = x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data = x$get()
    m = solve(data, ...)
    x$setinverse(m)
    return(m)    
}

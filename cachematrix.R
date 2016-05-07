## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    setinv <- function(solved) inv <<- solved
        
    getinv <- function() inv
        
    list(setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinv
    if(is.na(m)){
        message("using cached inverse matrix")
        return(m)
    }
    
    m <- solve(...)
    x$setinv(m)
    m

    ## Return a matrix that is the inverse of 'x'
}

## The functions below allow the creation of a special matrix object, which can 
## cache the inverse of the matrix held within the object once it has been calculated. 
## Once the inverse has been cached, the inversion calcualtion is not repeated, 
## rather the cached inverted matrix is simply retrieved and returned.



## makeCacheMatric takes a matrix as the only argument and makes a special matrix object,
## exposing a set of 4 functions to allow setting/getting of the matrix and a 
## cached variable (which dosen't strictly have to be a matrix but it is in this case)

makeCacheMatrix <- function(mx = matrix()) {
    
    ## initialize inv within this function to NULL,
    ## ensures null returned if no cached item exists   
    inv <- NULL
    
    ## function that updates the input matrix and clears the cached item
    set <- function(y){
        mx <<- y
        inv <<- NULL # sets cached inverse to null
    }
    
    ## function that returns the matrix
    get <- function() mx
    
    ## function that stores the cached item
    setinv <- function(solved) inv <<- solved
    
    ## function that returns the cached item    
    getinv <- function() inv
    
    ## returns the functions above  
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes the special matrix object created by makeCacheMatrix
## and calculates the inverse of the matrix and caches it back in the
## special matrix object, unless the inverse has already been cached, 
## in which case it simply retrieves and returns the inverse

cacheSolve <- function(x, ...) {
    
    ## get the chached inverse (if it exists)
    mx <- x$getinv()

    ## if the cached inverse exists, return it and end execution
    if(!is.null(mx)){
        message("using cached inverse matrix")
        return(mx)
    }
    
    ## get the original matrix out of the special matrix object
    mat <- x$get()
    
    ## calculate the inverse of the matrix and store it in the 
    ## special matrix object
    mx <- solve(mat)
    x$setinv(mx)
    
    ## Return the inverted matrix 
    mx
}

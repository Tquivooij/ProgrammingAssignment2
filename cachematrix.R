## Programming assignment 2 submission by Tim Quivooij
## 28-9-2023

## This function creates a special "matrix" object that can cache its inverse
## based on example function makeVector

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## method to set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## method to get the matrix
    get <- function() {
        ## return the matrix
        x
    }
    ## Method to set the inverse of the matrix
    setinverse <- function(inverse) {
        ## set the inverse
        m <<- inverse
    }
    ## Method to get the inverse of the matrix
    getinverse <- function() {
        ## return the inverse 
        m
    }
    
    ## return a list of the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.
## based on example function cachemean

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
    }
    
    ## get the matrix
    data <- x$get()
    
    ## calculate the inverse 
    m <- solve(data) %*% data
    
    ## set inverse of the matrix
    x$setinverse(m)
    
    ## return the (inverse) matrix
    m
}

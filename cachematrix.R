## This pair of functions creates an matrix that caches the inverse of itself 
## in order to save computationals time on calculating the inverse of the matrix. 

## This function creates a matrix and also caches the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() {x}
        setinverse <- function(inverse){m <<- inverse}
        getinverse <- function(){m}
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes and caches the inverse matrix from makeCacheMatrix. 
## Checks to see if the inverse has been solved and cached before
## performing the computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

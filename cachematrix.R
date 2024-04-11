## This creates a couple of functions that can cache the inverse of a matrix

## this first func makes a special matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ### establishes an inverse obj
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        getInverse <- function () i
        setInverst <- function(inverse) i <<- inverse 
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        
}


## this function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix() function above. If the inverse has 
## has already been calculated and the matrix has not been changed
## then cacheSolve() should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)){
                message("getting cached matrix")
                return(i)
        }
        
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setInverse(i)
        i
}

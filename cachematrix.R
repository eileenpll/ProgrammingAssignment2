## The pair of functions are used to cache the inverse of a matrix
## as the computing can be time-consuming sometimes.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL 
        ## store the inverse of a matrix and 
        ## reset to NULL every time makeCacheMatrix is called 
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        } ## 
        
        get <- function() {x} ## get the original matrix
        
        setinverse <- function(InverseMatrix) {
                inverse <<- InverseMatrix
                } ## called by cacheSolve() 
                  ## The value will be stored after the first access
        
        getinverse <- function() {inverse} 
        ## return the cached value at further attempts
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by 'makeCacheMatrix' above. If the inverse has
## already been calculated (and the matrix has not changed), then
## 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) { ## if the inverse is already cached
                message("getting the inverse of the matrix")
                return(inverse)
        }
        
        matrix <- x$get() 
        ## get the matrix if the inverse is not cached
        
        inverse <- solve(matrix)
        ## compute the inverse of the matrix using solve() function
        
        x$setinverse(inverse)
        
        inverse       
}
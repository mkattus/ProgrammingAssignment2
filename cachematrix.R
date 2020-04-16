## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This function builds a set of functions and returns them in a list. These functions are 
## meant to be called by the 'cacheSolve'function in order to retrieve the input matrix, cache
## the calculated inverse of the matrix, or get the cached matrix inverse. Once 'makeCacheMatrix' ('mCM')
## and 'cacheSolve' are run for a matrix, the matrix and its inverse are stored in the list returned by 'mCM'.

makeCacheMatrix <- function(x = matrix()) {
        Invx <- NULL                                    
        set <- function(y) {                            
                x <<- y                                 
                Invx <<- NULL                           
        }
        get <- function() x                             
        setInverse <- function(inverse) Invx <<- inverse    
        getInverse <- function() Invx                   
        list(set = set, get = get,                      
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve 
## retrieves the inverse from the cache initialized by 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
        Invx <- x$getInverse()                          
        if(!is.null(Invx)) {                            
                message("getting cached data")          
                return(Invx)                            
        }
        data <- x$get()                                 
        Invx <- solve(data, ...)                        
        x$setInverse(Invx)                              
        Invx                                            
}
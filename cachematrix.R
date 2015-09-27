## This function creates a custom type of matrix object that creates a cache of its inverse values through defining a list of functions
## that 'set' the matrix, 'get' the matrix, 'setsolve' the value of the inverse matrix, and 'getsolve' this inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {              ## Defines makeCacheMatrix as some function of x, where x a matrix
        
        inverse.x <- NULL                                ## Sets the default value of inverse.x to NULL in the global environment
        
        set <- function(y) {                             ## Defines set as a function of y, where y is a matrix
                
                x <<- y                                  ## Assigns the value of y to x within the environment of 'function'
                
                inverse.x <<- NULL                       ## Sets the value of inverse.x to NULL
        }
        get <- function() x                              ## Calls the input matrix if y was not found within the environment of 
                                                         ## this function
        
        setInverse <- function(solve) inverse.x <<- solve  ## Defines setInverse the value of 'function(solve)' within the functional
                                                           ## environment where inverse.x has been set to solve
        
        getInverse <- function() inverse.x                 ## Defines getInverse as a function over 'inverse.x'
        
        list(set = set, get = get, setInverse = setInverse,  ## Defines a list that sets the values of 'set', 'get', 'setInverse', and 
             getInverse = getInverse)                        ## 'getInverse' as equal to themselves
}

## Computes the value of the custom matrix created by the makeCacheMatrix. It does so in an efficient mannner by retrieving the value
## of any matrix that has previosuly been calculated and cached. Otherwise, it will calculate and cache the inverse value of any
## matrix that has not already been calculated

cacheSolve <- function(x, ...) {                         ## Returns the output of of the makeCacheMatrix  function and assigns it to
                                                         ## cacheSolve
        
        inverse.x <- x$getInverse()                      ## The value of the inverse matrix of x is assigned to inverse.x if it has already
                                                         ## been calculated and cached
        
        if(!is.null(inverse.x)) {                        ## If inverse.x is not a NULL value (i.e. the calculated value of the inverse of 
                message("getting cached data")           ## matrix x is found in the cache), then a message is displayed informing the
                return(inverse.x)                        ## user that the value of inverse.x is being retrieved from the cached data. This
        }                                                ## value is then returned by the 'return' function and no calculation is performed

        data <- x$get()                                  ## If the value of inverse.x is null, then function() x is assigned to the object
                                                         ## 'data'
        
        inverse.x <- solve(data, ...)                    ## The inverted matrix value(s) of x are calculated and applied to the object
                                                         ## 'inverse.x'
                                                         
        x$setInverse(inverse.x)                          ## The setInverse functions applies all the values of the inverse matrices to the cache
        
        inverse.x                                        ## Returns the inverse matrix value(s) that have been applied to 'inverse.x'
}

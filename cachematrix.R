## `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse. Specifically, 
## it creates a list containing a function to 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix's inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(input_matrix = matrix()) {
    v <- NULL                      # v is the inverse of a matrix and is set to NULL each time makeCacheMatrix is called
    set <- function(new_value) {   # this function or method lets you reuse the object
        input_matrix <<- new_value # by setting the stored inverse matrix "pointer" previously used to NULL
        v <<- NULL                 # and setting a new value for input_matrix
    }
    get <- function() {input_matrix} ## this function returns or "gets" the value of input_matrix
    setinverse <- function(storeValue) {v <<- storeValue} # this is called by cacheSolve() during
    ## the first call of cacheSolve for a matrix whose inverse has not yet been calculated and stored.
    ## access it and it will store the inverse using superassignment for "other-environment" retrieval 
    getinverse <- function() {v}     # This will return the cached inverse matrix to cacheSolve() on future accesses
    list(set = set, get = get,       # this is accessed each time makeCacheMatrix() is called or 
         setinverse = setinverse,    # each time we make a new object. This is a list object whose elements contain
         getinverse = getinverse)    # internal functions or methods so that a "calling function" knows how to 
}                                    # access those methods

## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(cache_matrix, ...) { ## `cache_matrix` is an object created by makeCacheMatrix
    ## The following will (in one of two ways) return a matrix that is the inverse of whatever was stored 
    ## as the output of `makeCacheMatrix(X)`, where `X` is an invertible matrix object
    v <- cache_matrix$getinverse()      # accesses `cache_matrix` and gets the corresponding inverse matrix object
    if(!is.null(v)) {                   # TRUE if the inverse was already cached, i.e. not NULL
        message("getting cached data")  # sends/prints this message to the console (good check to see if it's working)
        return(v)                       # Return() ends `cacheSolve()`, so if called (because v has been previously cached), 
                                        # the code below does not run
    }
    data <- cache_matrix$get() # only runs if cache_matrix$getinverse() returns NULL
    v <- solve(data, ...)      # we have to calculate the inverse if v was NULL
    cache_matrix$setinverse(v) # store the inverse determined in cache_matrix; this is the bit 
                               # that leads to the superassignment (in makeCacheMatrix) of the inverse for future access.                                
    v                          # return the inverse matrix to the code level that called cacheSolve                           
}

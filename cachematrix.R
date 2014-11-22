## `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse. Specifically, 
## it creates a list containing a function to 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix's inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() {x}
    setinverse <- function(storeValue) {v <<- storeValue}
    getinverse <- function() {v}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    v <- x$getinverse()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinverse(v)
    v  
}

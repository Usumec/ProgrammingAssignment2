## These functions take a matrix and create and object that caches the inverse 
## after the first run to avoid recalculating in subsequent runs 

## makeCacheMatrix creates an object containing the original matrix, an empty object to use as 
## cache and a set of functions to manipulate these objects

makeCacheMatrix <- function(x = matrix()) {
        #initialize the inverse object as NULL
        i <- NULL 
        
        #reinitializes the object with new matrix and erases cache
        set <- function(y) { 
                x <<- y
                i <<- NULL
        }
        
        #get the value of the input matrix
        get <- function() x 
        
        #assigns calculated inverse to cache
        setinv <- function(inv) i <<- inv 
        
        #get the value of the cached inverse
        getinv <- function() i 
        
        #returns functions as named list
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv= getinv)
}


## cachesolve takes an object created by the makeCacheMatrix function, looks for cached
## matrix and returns it, if not found calculates the inverse of original matrix
## and sets it in the cache

cacheSolve <- function(x, ...) {
        
        #get cached object
        i <- x$getinv() 
        
        #if the object exists, load from cache and exit
        if (!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        
        #if cache is empty, calculate inverse of original matrix
        mat <- x$get()
        i <- solve(mat, ...)
        
        #save inverse matrix to cache
        x$setinv(i)
        i
        
}

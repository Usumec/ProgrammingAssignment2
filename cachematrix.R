## These functions take a matrix and create and object that caches the inverse 
## after the first run to avoid recalculating in subsequent runs 

## Creates an object containing the original matrix, an empty object to use as 
## cache and a set of functions to manipulate these objects

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #initialize the inverse object as NULL
        set <- function(y) { #reinitializes the object with new matrix and erases cache
                x <<- y
                i <<- NULL
        }
        get <- function() x #get the value of the input matrix
        setinv <- function(inv) i <<- inv #assigns calculated inverse to cache
        getinv <- function() i #get the value of the cached inverse
        list(set = set, #returns functions as named list
             get = get,
             setinv = setinv,
             getinv= getinv)
}


## takes an object created by the makeCacheMatrix function, looks for cached
## matrix and returns it, if not found calculates the inverse of original matrix
## and sets it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv() 
        if (!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinv(i)
        i
        
}

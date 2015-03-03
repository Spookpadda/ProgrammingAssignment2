## These functions cache and retrieve a matrix object and its inverse using 
## a list object including four storage and retrieval functions

## makeCacheMatrix takes a convertible square matrix as input and returns a
## list object of four functions, which when called: Set() - Store a new matrix; 
## Get() - retrieve the current matrix; Setinv() - cache the inverse of the current
## current matrix); Getinv() - retrieve this inverse matrix
## the variables x & inv apply in the parent, makeCacheMatrix(),environment (<<-)

makeCacheMatrix <- function(x = matrix()) {
        ## makes a list of functions to store and retrieve a matrix and its inverse
        inv <- NULL     ## local null variate to monitor the status of the cache
        set <- function(y) { ## function to set a new matrix y
                x <<- y         ## set x (parent env) to new matrix y
                inv <<- NULL    ## set inv(parent env) to no cached inverse
        }
        get <- function() x     ## return current matrix x
        ## cache inverse matrix as inv (parent env)
        setinv <- function(inverse) inv <<- inverse
        ## return NULL or cached inverse matrix
        getinv <- function() inv
        ## return a list of four functions, called by name
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve takes a list created by makeCacheMatrix(), tests whether there is
## an existing cached inverse matrix, using the x$getinv(), or solves the
## inverse of the current matrix, from x$get(), caches this inverse,
## and returns the inverse matrix (from the cache or newly calculated)

cacheSolve <- function(x, ...) {
        ## function works on list created by makeCacheMatrix()
        cache <- x$getinv()       ## get cached inverse or NULL
        if(!is.null(cache)) {     ## test for existing cached inverse
                message("getting cached data") ## if it exists...
                return(cache)     ## return existing cached inverse
        }                               ## if inverse is NULL
        data <- x$get()                 ## get current matrix
        inverse <- solve(data, ...)     ## solve for inverse
        x$setinv(inverse)               ## cache new inverse
        inverse                         ## return new inverse
}

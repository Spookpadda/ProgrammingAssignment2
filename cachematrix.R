## These functions create an list object to cache and retrieve the current 
## matrix and to calculate and cache its inverse

## makeCacheMatrix takes a convertible square matrix as input and returns a
## list object of four functions which when called: Set a new matrix, Get the
## current matrix; Setinv - Cache another matrix (inverse of current matrix);
## (Getinv) - retrieve this inverse matrix
## the variables x & inv apply in the parent, makeCacheMatrix(),environment

makeCacheMatrix <- function(x = matrix()) {
        ## makes a list of functions that cache and retrieve the matrix
        ## and its inverse
        inv <- NULL     ## local null variate; used for no cached inverse
        set <- function(y) { ## function to set a new matrix y
                x <<- y         ## set x (parent env) to new matrix y
                inv <<- NULL    ## set inv(parent env) to no cached inverse
        }
        get <- function() x     ## function to return current matrix x
        ## either from makeCacheMatrix() or $set()
        setinv <- function(inverse) inv <<- inverse
        ## cache inverse matrix as inv (parent env)
        getinv <- function() inv
        ## return NULL or cached inverse matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        ## return a list of four functions, called by name
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

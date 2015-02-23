# Coursera R Prog
# Programming Assignment 2: Lexical Scoping
# Assignment: Caching the Inverse of a Matrix
#
## note: The header, formatting and skeleton code were copied
#    from the assignment example functions 'makeVector' & 'cacheMean'
#
## Overall description:
#       Inverting a matrix can be resource intensive
#      'cacheSolve' function uses output functions from 'makeCacheMatrix'
#           (a) returning matrix inverse if it is already in cache,
#                   thus avoiding having to recalculate invese
#           (b) if matrix inverse is not in cache,
#                   then it is calculated & cached so it is available for future use
#                   without having to recalculate it.




###############################################
## `makeCacheMatrix` creates a matrix object to cache its inverse.
#
##  Input:
#     (1) x: an invertible matrix
##  Returns: List of four functions
#     (1) 'set': fn that sets the value of matrix
#     (2) 'get': fn that gets the value of matrix
#     (3) 'setinverse': fn that sets the value of the inverse of x
#     (3) 'getinverse': fn that gets the value of the inverse of x
###############################################
makeCacheMatrix <- function(x = matrix()) {
    
    xinv <- NULL
    
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }#end fn 'set'
    get <- function() x
    setinverse <- function(inverse) xinv <<- inverse
    getinverse <- function() xinv
    
    #return 4 functions
    return( list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse) )
    
}#end fn 'makeCacheMatrix'


###############################################
## 'cacheSolve' function returns & caches matrix inverse of input matrix.
#     It first checks if matrix inverse is already cached.
#     If so, then the cached matrix is returned (not recalculated).
#     Else, matrix inverse is calculated using 'solve' function
#           (we assume that the input matrix is invertible),
#           then this matrix inverse is cached using 'setinverse' function
#
## Input:
#       (1) x: output of 'makeCacheMatrix'
## Returns:
#       (1) inverse of matrix x$get()
###############################################
cacheSolve <- function(x, ...) {
    
    xinv <- x$getinverse() #NULL if inverse is not in cache
    
    #if xinv is already cached, then return cached matrix
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    
    ##else (if xinv is not already in cache)
    data <- x$get() #get matrix to invert
    xinv <- solve(data, ...) #solve for inverse
    x$setinverse(xinv) #caches inverse
    
    return (xinv) # returns the inverse
    
}#end fn 'cacheSolve'

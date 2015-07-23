## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function with a list of four functions that creates an inverse of a matrix and caches the data
## set() function assigns or re-assigns the matrix used in the makeCacheMatrix function 
## the inverse matrix is reset and has to be recalcualated with getinverse()
## get() function returns the matrix used as argument or returns matrix assigned by set()
## setinverse() function applies solve function on input matrix
## getinverse() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y   
      m <<- NULL   
    }
    get <- function() x   
    setinverse <- function(solve) m <<- solve  
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve returns inverse of matrix from makeCacheMatrix function
## If it is still cached from makeCacheMatrix it will return this data
## Otherwise it performs an inverse on the matrix with the solve function and
## it caches the data


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {   # checks for cached inverse matrix
      message("getting cached data")
      return(m)     # returns cached data
    }
    data <- x$get()    # if no cached data the argument matrix data is used 
    m <- solve(data, ...)    
    x$setinverse(m)    # cache inverse matrix data
    m
}

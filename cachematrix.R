## Put comments here that give an overall description of what your
## functions
##++
## makeCacheMatrix() and cacheSolve() are functions pairs to crearte and compuate the 
## inverse of a square matrix then cache the result for future access. In the event, 
## the matrix has changed, a new inverse is computed and cached, otherwise, the 
## cached value is returned on subsequent invokation to cacheSolve()
##
## Assumptions: 1) a squared matrix is passed to makeCacheMatrix()
##              2) no attempted is made to valide the input matrix
##
## 
##--

## Write a short comment describing this function

##++
## makeCacheMatrix() 
## =================
## a cache function to generate a special vector containing the following getters 
## and setters (the "list(set = set, ...)" command)
##
## set() - a fuction to initialize and cache the matrix to be inverted
## get() - returns the matrix value
## setInv() - initializes the cache with the inverted matrix
## getInv() - returns the inverted matrix from cache or NULL otherwise.
## 
##--

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    } 
     
    get <- function() { 
      x 
    }
    
    setInv <- function(solve) { 
      inv <<- solve 
    }
    
    getInv <- function() { 
      inv 
    }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

##++
## cacheSolve() 
## =================
## A function to compute and return the inverse of a squared matrix. It uses
## makeCacheMatrix() to cache the computed value for subsequent invokation.
## 
## When it's invoked, it checks whether the inverse had already been computed
## and stored in cache. If so, it display a message and returns the inverse. 
## Otherwise, it will compute and store a new inverse, then return the computed
## inverse.
## 
##--
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
      message("getting cached inversed matrix")
      return(inv)
    }
  
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    
    inv
}

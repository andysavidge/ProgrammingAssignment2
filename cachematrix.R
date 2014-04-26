## Put comments here that give an overall description of what your
## functions do

## These functions create a special "matrix" object that can cache its inverse.
## Two functions use [r] scoping rules to assign a matrix inverse solution using <<- operator
## to a 
## makeCacheMatrix is a function that
## cacheSolve is a function that

## Write a short comment describing this function
# The 1st function, makeCacheMatrix creates a special "matrix" object or list containing functions to
# set <- function(y){x <<- y; m <<- NULL}      ## set the value of the matrix
# get <- function() {x}                        ## get the value of the matrix
# setInverse <- function(solve) {m <<- solve}  ## set the value of the solve or inverse matrix
# getInverse <- function() {m}                 ## get the value of the solve or inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      if(det(x) == 0) message("matrix is 'Singular' and cannot be inverted")
      if(nrow(x) != ncol(x)) message("matrix is not 'Square' and does not have an inverse")
      m <- NULL
      set <- function(y) {
            message("inside set()")
            x <<- y
            m <<- NULL
      }
      get <- function() { 
            message("inside get()")
            x
      }
      
      setInverse <- function(solve) {
            message("inside setInverse()")
            m <<- solve
      }
      
      getInverse <- function() {
            message("inside getInverse()")
            m
      }
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

#function calculates the inverse of the special "matrix" created with the above function. 
#It first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
#the cache via the setInverse function.
# m2 <- x$getInverse()          ## gets the inverse (or NULL) of the special "matrix" created with the above function
# if(!is.null(m2)) {return(m2)} ## checks to see if the inverse has already been calculated & if so, 
#                               ## it gets the inverse from the cache and returns it (skips the inverse computation)
# data <- x$get()               ## else gets the data 
# m2 <- solve(data, ...)        ## & then calculate the inverse matrix of the data 
# x$setInverse(m2)              ## & set the value of the inverse in the cache via the setInverse function.
# m2                            ## then lastly return m2

cacheSolve <- function(x, ...) {
      m2 <- x$getInverse()
      if(!is.null(m2)) {
            message("getting cached data")
            return(m2)
      }
      data <- x$get()
      m2 <- solve(data, ...)
      x$setInverse(m2)
      m2
}
#testing functions; make a matrix of numerics called 'a' and instantiate a variable 'b' 
# of type makeCacheMatrix().  at b <- makeCacheMatrix(a), the inverse of 'a' hasn't been calculated yet
# makeCacheMatrix(a) assignment just creates a variable call b.  When you call cacheSolve()
# it runs the code to figure out if the inverse (which I've stored in m2) exists or not.
# If it exists (i.e. !is.null( )) then return the inverse otherwise calculate it using the solve function 
# and then sets the inverse (i.e. x$setInverse(m2)) for future references.
#
#a=1:4
#dim(a)=c(2,2)
#b <- makeCacheMatrix(a)
#cacheSolve(b)
#cacheSolve(b)

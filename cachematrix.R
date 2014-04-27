## These functions create a special "matrix" object that can solve(if needed), cache, and get cached 
## matrix inverses.
## Two functions below use [r] scoping rules to assign a matrix inverse solution using 
## the <<- operator to cache the solution

# makeCacheMatrix takes an argument x of a "matrix" object and then returns
# a list with 4 list items that are callable functions as follows:
# set <- function(y){x <<- y; m <<- NULL}      ## set the value of the matrix
# get <- function() {x}                        ## get the value of the matrix
# setInverse <- function(solve) {m <<- solve}  ## set the value of the solve or inverse matrix
# getInverse <- function() {m}                 ## get the value of the solve or inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      if(det(x) == 0) {                                           # check for OK 'invertible' input matrix
            message("matrix is 'Singular' and cannot be inverted")
      }
      if(nrow(x) != ncol(x)){                                     # check for OK 'square' input matrix
            message("matrix is not 'Square' and does not have an inverse")
      }
      m <- NULL
      set <- function(y) {                                        # function to set to a "cache" 
            message("in function CacheMatrix sub-function set()")
            x <<- y
            m <<- NULL
      }
      get <- function() {                                         # function to get matrix being solved
            message("in function CacheMatrix sub-function get()")
            x
      }
      
      setInverse <- function(solve) {                             # saves solution to "cache" location
            message("in function CacheMatrix sub-function setInverse()")
            m <<- solve
      }
      
      getInverse <- function() {                                   # returns cached solution object
            message("in function CacheMatrix sub-function getInverse()")
            m
      }
      
      list(set = set, get = get,                                   # returns a list of callable functions
           setInverse = setInverse,
           getInverse = getInverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# However, if the inverse has already been calculated then cacheSolve gets the solution from the cache.

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
      m2 <- x$getInverse()        # get x matrix's cache (it may be NULL or not)
      if(!is.null(m2)) {          # if there is a cache
            message("in function cacheSolve getting cached data")
            return(m2)            # return the cache solution & no computation needed
      }
      data <- x$get()             # if there is no cache then get the matrix 
      m2 <- solve(data, ...)      # compute the inverse
      x$setInverse(m2)            # save result back to x's setInverse cache
      m2                          # return result
}
# testing functions; make a matrix of numerics called 'a' and instantiate a variable 'b' 
# via makeCacheMatrix() at b <- makeCacheMatrix(a), the inverse of 'a' hasn't been calculated yet
# makeCacheMatrix(a) assignment just creates a variable call b.  When you call cacheSolve()
# it runs the code to figure out if the inverse (stored in m2) exists or not.
# If it exists (i.e. !is.null( )) then return the inverse otherwise calculate it using the solve function 
# and then sets the inverse (i.e. x$setInverse(m2)) for future references.
#
#a=1:4
#dim(a)=c(2,2)
#b <- makeCacheMatrix(a)
#cacheSolve(b)
#cacheSolve(b)

#GRADING
#Was a valid GitHub URL containing a git repository submitted?
#0 points: A valid GitHub URL was NOT submitted (or URL is broken)
#x1 point: The submitted URL points to a GitHub repository
#Does the GitHub repository contain at least one commit beyond the original fork?
#0 points: No, there are no commits beyond the original fork
#x1 point: Yes, there is at least one commit beyond the original fork
#Does the GitHub repository contain an R file containing code implementing the completed assignment? 
#NOTE: There is no need to run the code here, but rather you should visually inspect the R file in the GitHub repository and check to see that there is code there beyond the original stub that was committed there by the instructor. Do not attempt to judge whether the code is correct or not.
#0 points: No, the R file does not contain any code implementing the completed assignment
#1 point: The R file contains code implementing a partially completed assignment
#?completed?x2 points: The R file contains code implementing the complete assignment
#Does the R file containing the code have any comments explaining what the code does?
#NOTE: The makeCacheMatrix and cacheSolve functions should both be documented with explanatory comments. There maybe other functions in the R file but they do not need to be commented.
#0 points: There are no explanatory comments in the R file
#1 points: One of the functions has some corresponding explanatory comments in the R file
#x2 points: Both functions have corresponding explanatory comments in the R file
#Does the R code implementing the 'makeCacheMatrix' function appear to be correct, to the best of your ability to judge?
#NOTE: Do not run the R code on your own computer. Please examine the R code and determine to the best of 
#your ability, whether the solution presented appears to match the requirements of the assignment. 
#A correct solution (as best you can determine) gets 1 point and a solution that is well-written and 
#easy to read gets an additional 1 point.
#0 points: The solution contains at least one identifiable problem
#x1 point: The solution appears to be correct
#?x2 points: The solution appears to be correct and is written in a well-formatted and easily readable style
#Does the R code implementing the 'cacheSolve' function appear to be correct, to the best of your ability 
#to judge?
#NOTE: Do not run the R code on your own computer. Please examine the R code and determine to the best of your 
#ability, whether the solution presented appears to match the requirements of the assignment. 
#A correct solution (as best you can determine) gets 1 point and a solution that is well-written and 
#easy to read gets an additional 1 point.
#0 points: The solution contains at least one identifiable problem
#x1 point: The solution appears to be correct
#?x2 points: The solution appears to be correct and is written in a well-formatted and easily readable style

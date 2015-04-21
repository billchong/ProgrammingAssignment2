##################################################################################
## Function Name:
##    makeCacheMatrix (x)
## Typical Usage:
##    z <- makeCacheMatrix () which stores NULL matrix
##    z <- makeCacheMatrix(x) which stores x a matrix or linear vector
## Purpose:
##    makeCacheMatrix generates an object with function list and a cached inverse.
##    The object stores a matrix or vector element specified by the input parameter
##    x.
## Input Parameter
##    x - can be a square invertible matrix OR
##        linear vector whose length has an integer square root
###################################################################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set    <- function(y) {
      x <<- yclass
      m <<- NULL
    }
    get    <- function() x
    setinv <- function(invers) inv <<- invers
    getinv <- function () inv
    list (set = set, get= get, setinv = setinv, getinv = getinv)
}

#################################################################################
## Function Name:
##    cacheSolve
## Typical Usage:
##    cacheSolve (z)
## Purpose:
##    CacheSolve returns a cached matrix inverse if one exists OR
##    CacheSolve calculates an inverse for a square invertible matrix and 
##    caches the value for future use if a cached inverse does not exist
## Input Parameter
##    Input parameter x is a makeCacheMatrix object
#################################################################################

cacheSolve <- function(x, ...) {
  
    ##Verify input parameter is a 'list' object. Quit if not a list object.
    if (class(x) != 'list') {
        message ("ERROR: Input parameter is not a makeCacheMatrix object list")
        return (NULL)
    }
    
    ## Return a matrix that is the inverse of 'x' if it is cached
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## No cache inverse exists so we have to solve for inverse
    ## Verify if input parameter x is a matrix then it must be square OR
    ## Verify if input parameter x is a vector then its length has an integer square
    ## root. Convert the vector to a square matrix
    
    if (!is.matrix(x$get()))  {
      len <- length(x$get()) 
      
      ## Loop to find the square dimension required
      for (i in 1:len) {
      
        ## input vector can be transformed to square matrix
        if (len == i^2) {
          data <- matrix (x$get(), i, i)
          break
        }
      
        ## If input vector cannot be made square then cannot invert and quit
        if ((len < i^2)) {
          message ("input parameter cannot be made square")
          return (NULL)
        }
      }
    }
    else
    {
      ## Input parameter is matrix so check if it is square
      ## qUit if not square
      data <- x$get()
      if (nrow(data) != ncol(data)) {
         message("input parameter is not a square matrix")
        return (NULL)
      }
    }
    
    ## Input vector can be made square so we can solve for inverseQ
    inv <- solve(data)
    x$setinv(inv)
    inv
}

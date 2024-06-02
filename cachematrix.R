# Functions makeCacheMatrix() and cacheSolve() are working together to avoid 
# recomputing an inverse of a matrix and this is achieved by caching.


# makeCacheMatrix() takes a square invertible matrix 'x' as an input and returns
# a "special matrix" which is a list containing functions. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(invMat) m <<- invMat
  getInvMatrix <- function() m
  list(set = set, get = get, 
       setInvMatrix = setInvMatrix, 
       getInvMatrix = getInvMatrix)

}

# cacheSolve() solves for an inverse of a matrix that is stored inside the 
# "special matrix" created by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)){
    message("Getting cached matrix")
    return(m)
  }
  data <-x$get()
  m <- solve(data,...)
  x$setInvMatrix(m)
  m
}

#-------------------------------------------------------------------------------
# Testing:

# 1. Create a 2x2 matrix 'a' with random data.
x0 <- rnorm(4)
dim(x0) <- c(2,2)

# 2. Make a special matrix 'xs'
x <- makeCacheMatrix(x0)
cacheSolve(x)

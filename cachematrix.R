## The function makeCacheMatrix, creates a special "matrix", which is really
## a list of 4 functions containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Just informs the user that the matrix has no iverse because isn't square
  if (nrow(x) != ncol(x)) {
    message("matrix is not square. Can't solve inverse")
  }
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## The function "cacheSolve calculates the mean of the special "matrix" created with
## the above function. It gets the inverse from the "matrix" using x$getinverse 
## call. Then, it first checks whether the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse using the solve function of the matrix and sets the value of the iverse  
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
 
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  class(x)
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## An example:
## Ma<- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 2)
## MatCache <-makeCacheMatrix(Ma)
## cacheSolve(MatCache)
## [,1] [,2]
## [1,]  2.2 -0.6
## [2,] -0.4  0.2
## Calling the cacheSolove again, produces
## cacheSolve(MatCache)
## getting cached data  <-- cacheSolve gets the inverse from a stored value.
## [,1] [,2]
## [1,]  2.2 -0.6
## [2,] -0.4  0.2
##
## If matrix is not square, here is an example:
##
## Ma<- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3)
## MatCache <-makeCacheMatrix(Ma)
## matrix is not square. Can't solve inverse
## cacheSolve(MatCache)
## Error in solve.default(data, ...) : 'a' (2 x 3) must be square
## Called from: top level 
## Browse[1]> Q## Put comments here that give an overall description of what your
## functions do


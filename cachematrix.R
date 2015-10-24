## These functions solve the inverse of the matrix.
## Apply these method to solve.
## > ## Define a square matrix. Let it be A.
## > cacheSolve(makeCacheMatrix(A))

## This function solves the information of matrix for the computation of inverse.

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  # Set the value of its inverse
  setinverse <- function(solve) a <<- solve
  # Get the value of the inverse matrix
  getinverse <- function() a
  # List down everything and return
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function solves the list of information and compute the inverse, besides printing.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  b <- x$getinverse()
  # Check the condition computed from makeCacheMatrix
  if(!is.null(b)) {
    message("Getting cache data...")
    return(b)
  }
  answer <- x$get()
  b <- solve(answer, ...)
  x$setinverse(b)
  b
}

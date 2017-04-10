## Two functions below demonstrate how to avoid repetitive inversion of matrix
## if the original matrix to be inverted is not changed

## "makeCacheMatrix" creates a matrix object with 4 associated functions:
## "set" which sets the matrix, "get" which gets the matrix, "setinv" which sets
## inverse of the matrix and "getinv" which gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## "cacheSolve" function will invert the matrix created by "makeChacheMatrix" if
## it doesn't exist or use existing inverse if it was stored before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached inverse")
    return(xinv)
  }
  mat_in <- x$get()
  xinv <- solve(mat_in)
  x$setinv(xinv)
  x$getinv()
  }

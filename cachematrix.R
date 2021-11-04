## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix() creates a special matrix object
# cacheSolve() calculates the inverse of the matrix passed to makeCacheMatrix()

# makeCacheMatrix() is passed a square numeric matrix
# returned object is passed to cacheSolve()

## Write a short comment describing this function

# makeCacheMatrix() defines 4 functions
# for setting and getting a square numeric matrix and its inverse
# im is reset to NULL upon resetting value of x (new matrix input) using <<- operator

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

# cacheSolve() retrieves pre-calculated matrix inversion or calculates the matrix inversion
# if(!is.null(im)) is TRUE (inverse of the matrix has been computed before) - retrieves/returns the cached inverse by accessing x$getinverse
# if(!is.null(im)) is FALSE - computes the inverse using solve() and then placing im in x$setinverse()
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  matrix <- x$get()
  im <- solve(matrix, ...)
  x$setinverse(im)
  im
}

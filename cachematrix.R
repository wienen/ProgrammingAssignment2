## These functions will together solve a matrix inversion and cache the result.
## The inverse will only be calculated on the first time it is requested, after
## this, it will be stored and the stored value will be returned the next time
## the inverse is asked.

## makeCacheMatrix will set up the matrix to store and return its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## on initialization, the inverse is unknown.
	inverse <- NULL

  ## store the matrix and reset its inverse, as this matrix is a new one
  ## NB the precondition is that the matrix has an inverse
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
  ## get the value of the matrix
	get <- function() x
  
  ## store the externally calculated inverse of the matrix
	setinverse <- function(matrixInverse) inverse <<- matrixInverse
  
  ## return the inverse that was stored; return NULL otherwise
	getinverse <- function() inverse
  
  ## bundle the methods in a list for access outside of the function
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## cacheSolve will return the inverse of matrix x by
## checking if the inverse has been calculated before.
## If so, it will return the stored result, otherwise, it will
## calculate the inverse, store it in the cacheMatrix object
## and return it
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    ## the inverse has been cached. Just return it
    return (inverse)
  }
  
  ## the inverse has not yet been calculated
  ## retrieve the matrix
  matrix <- x$get()
  
  ## calculate the inverse
  inverse <- solve(matrix, ...)
  
  ## cache it 
  x$setinverse(inverse)
  
  ## and return it
  inverse
}

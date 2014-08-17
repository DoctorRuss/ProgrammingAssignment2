## R Programming Assignment 2
## A pair of functions to cache the calculation of the 
## inverse of a matrix

## This function takes a matrix (defaulting to the empty matrix)
## and returns a list of functions that operate on this data.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	# get the matrix data back
	get <- function() x
	# set and cache the inverse of the matrix 
	setinverse <- function(inverse) i <<- inverse
	# get the inverse of the matric
	# may be NULL if not already calculated
	getinverse <- function() i
	# return the list of functions on this matrix object
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)

}


## This function returns the inverse of the matrix argument "x"
## If the inverse has been cached, it will return the cached data.
## Otherwise it will call "solve" and set the inverse on the argument "x"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if (is.null(inv)){
    message("Inverse not set: calculating from scratch")
    inv<-solve(x$get())
    x$setinverse(inv)
  }
  inv
}

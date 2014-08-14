## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

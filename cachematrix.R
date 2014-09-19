## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a special object that stores a matrix #which contains a function to
#1) set the value of the matrix
#2) get the value of the matrix
#3) set the value of the inverse
#4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) { #set the value of the matrix
		x <<- y
		m <<- NULL
	}
	get <- function() x #get the value of the matrix
	setinverse <- function(inverse) m <<- inverse #set the value of the inverse
	getinverse <- function() m #get the value of the inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function
#The following function solves for the inverse of the special "matrix" created with the above function. However, it first
#checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the
#computation. Otherwise, it solves for the inverse of the matrix and sets the value of the inverse in the cache via the
#setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() #if the inverse has already been calculated, get it from the cache
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get() #if not calculate the inverse of the matrix and set this in the cache via the setinverse function
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
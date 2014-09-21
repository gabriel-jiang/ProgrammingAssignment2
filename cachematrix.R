## Put comments here that give an overall description of what your
## functions do
#Matrix inversion is usually a costly and potentially time-consuming computation and there may be some benefits to
#caching the inverse of a matrix rather than compute it repeatedly (e.g. in a loop). An example is when the contents
#of a matrix do not change. It may make sense to cache the value of the inverse so that when we need it again, it can be
#looked up in the cache rather than recomputed. The functions given here create a special object that stores a matrix and
#cache's its inverse. They take advantage of the scoping rules of the R language and how they can be manipulated to preserve
#the inside state of an R object. We use the <<- operator which can assign a value to an object in an environment that
#is different from the current environment.

## Write a short comment describing this function
#makeCacheMatrix creates a special "matrix" object that can cache its inverse. This contains a function to
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
#The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. However, it first
#checks to see if the inverse has already been calculated (and the matrix has not changed). If so, cacheSolve retrieves the
#inverse from the cache and skips the computation. Otherwise, it computes the inverse of the square invertible matrix with
#the solve function and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() #if the inverse has already been calculated, get it from the cache
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...) #if not, calculate the inverse of the matrix
	x$setinverse(m) #set the inverse in the cache via the setinverse function
	m
}
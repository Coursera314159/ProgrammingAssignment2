## makeCacheMatrix and cacheSolve are a pair of functions which can be
## used to cache the value of the inverse of a matrix, to avoid costly
## recomputations. The cached value is cleared when the matrix itself is
## changed, to ensure up-to-date values are returned. The functionality
## is provided through a custom 'CacheMatrix' object, created using
## makeCacheMatrix, along with the cacheSolve function which returns the
## cached value of the matrix inverse, only recomputing this value
## when the matrix itself has changed.

## Given an R matrix object, creates a custom 'CacheMatrix' object with
## the ability to cache the value of its inverse. The 'CacheMatrix'
## object provides functions for getting and setting the matrix and for
## getting and setting the value of the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## initialise cached inverse value to NULL
	i <- NULL

	## When the matrix x is changed, we reset the inverse
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) i <<- inverse

	getinverse <- function() i

	## Return the custom 'CacheMatrix' object
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Given a 'CacheMatrix' object, returns the inverse of the matrix.
## If the inverse has previously been calculated for the current matrix
## data, this cached inverse value is returned. Otherwise the matrix
## inverse is calculated, stored and returned.
cacheSolve <- function(x, ...) {
	## Retrieve the value of the matrix inverse from the custom
	## 'CacheMatrix' object
	i <- x$getinverse()

	## If the retrieved value of the matrix inverse is
	## not null, return the retrieved value
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	## Otherwise, retrieve the current matrix data
	data <- x$get()

	## Calculate, cache and return the inverse value
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

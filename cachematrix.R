## Create a matrix, solve, and cache it's value for future use.

## Function allows you to create a matrix and store its inverse in cache.
## Returns a list of functions that allow you to modify matrix, get matrix
## value, and return inverse if it has been cached.

makeCacheMatrix <- function(x = matrix()) {
	
	## Initial value of inv
	inv <- NULL
	
	## Change the value of the matrix and set value of inv to NULL
	setmatrix <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## Get the current value of the Matrix
	getmatrix <- function() x
	
	## Set the value of inv returned from cacheSolve
	setinverse <- function(z) inv <<- z
	
	## Get the value of inv
	getinverse <- function() inv
	
	## Output list of functions
	list(setmatrix = setmatrix, getmatrix = getmatrix,
	     setinverse = setinverse, getinverse = getinverse)
	
}


## Solves for the the inv of the supplied matrix, if the inv is cached
## it returns the cached value.

cacheSolve <- function(x, ...) {
	
	## Get stored value of inv
	inv <- x$getinverse()
	
	## Check to see if inv is NOT NULL, if TRUE then return cached value
	if(!is.null(inv)){
		message("Getting cached value!")
		return(inv)
	}
	
	## If inv is NULL, get matrix
	preinv <- x$getmatrix()
	
	## Return inverse of matrix
	inv <- solve(preinv)
	
	## Update setinverse with new matrix
	x$setinverse(inv)
	
	## Return inv
	inv
}

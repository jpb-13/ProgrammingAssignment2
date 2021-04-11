## R functions that caches the inverse of the matrix, and stores the matrix. 

## R function that creates a matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	set <- function(y) {
		x <<- y
		n <<- NULL
	}
	get <- function() x
	setN <- function(inverse) n <<- inverse
	getN <- function() n
	list(set = set, get=get, setN = setN, getN = getN)
}


## R function that gets the inverse of the matrix from the previous R function. 

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	n <- x$getN()
	if(!is.null(n)) {
		message("getting cached data")
		return(n)
	}
	mtrx <- x$get()
	n <- solve(mtrx, ...)
	x$setN(n)
	n
}

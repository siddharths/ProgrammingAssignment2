## Matrix inversion is usually a costly operation. These functions
## exploit the benefits of caching the inverse of a matrix rather than 
## computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	## Initialize the inverse
	i <- NULL

	## Set the matrix
	set <- function(matrix) {
		x <<- matrix
		i <<- NULL
	}

	## Get the matrix
	get <- function() {
		x
	}

	## Set the inverse
	setInverse <- function(inverse) {
		i <<- inverse
	}

	## Get the inverse
	getInverse <- function() {
		i
	}

	## Return the list
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()

        ## Return the inverse if it is already set
        if( !is.null(m) ) {
        	message("Retrieving data from cache")
        	return(m)
        }

        ## Get the matrix
        data <- x$get()

        ## Compute the inverse (Assuming the matrix is always invertible)
        m <- solve(data)

        ## Set the inverse 
        x$setInverse(m)

        ## Return the matrix
        m
}

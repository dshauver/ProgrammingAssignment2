## These functions are a demonstration of caching results.  In this
##particaular case, the functions calculate the inverse of a matrix,
##and the cache the result of the inverse function to avoid calculating
##it again if it's not already calculated.

##makeCacheMatrix defines a matrix and set of functions to pass to the
##cacheSolve funtion

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL # m is the cache.  Zero it out here.

	##define the functions we'll use in the cacheSolve function later.
	
	#function to set the value of x and m
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	#function to get the value of x
	get <- function() x

	#function to cache the result of the inverse operation
	setcache <- function(solve) m <<- solve

	#function to read the cached value (if any)
	getcache <- function() m

	#return a list of the above-defined functions.
	list(set = set, get = get,
	setcache = setcache,
	getcache = getcache)

}


## cacheSolve checks to see if the inverse of the matrix has been computed,
## prints the results if so, and computes & caches the results if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## read the cache
	m <- x$getcache()  

	##If it's not empty, return the value.
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	##get the matrix to invert
	data <- x$get()

	##invert the matrix
	m <- solve(data, ...)

	##cache the result
	x$setcache(m)

	##Print the result
	m
}


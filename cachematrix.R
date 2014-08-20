## 
## Functions create a matrix cache object (makeCacheMatrix) and inverts 
## the matrix using solve function or returns a cached value saved in 
## matrix cache object (cacheSolve).
##
## Example:
## > c <- rbind(c(1,-1/4), c(1, 1/4))
## > mco <- makeCacheMatrix(c)
## > cacheSolve(mco)
##     [,1] [,2]
## [1,]  0.5  0.5
## [2,] -2.0  2.0
## > cacheSolve(mco)
## Getting cached data
##    [,1] [,2]
## [1,]  0.5  0.5
## [2,] -2.0  2.0

## Function description:
## 		makeCacheMatrix creates a cacheable matrix object utilized
## 		by the cacheSolve function. The purpose of this object is to be 
## 		a cache for an inverted matrix.
## Arguments:
##		x - matrix 
## Returns:
## 		None
makeCacheMatrix <- function(x = matrix()) {
	inv_matrix <- NULL
	
	set <- function(y) {
		x <<- y
		inv_matrix <<- NULL
	}
	
	get <- function() x
	
	set_inv <- function(inv) inv_matrix <<- inv
	get_inv <- function() inv_matrix
	list (set = set, get = get,
			set_inv = set_inv, get_inv = get_inv)
}


## Function description:
## 		cacheSolve inverts a matrix using solve function or uses cached matrix
## 		from the matrix cache object x if one exists
## Arguments:
##		x - matrix object created by makeCacheMatrix
## Returns:
## 		inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$get_inv()
		
		if(!is.null(m)) {
			message("Getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data, ...)	
		x$set_inv(m)
		m
}

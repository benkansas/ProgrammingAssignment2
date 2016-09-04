## Put comments here that give an overall description of what your
## functions do

## creates a cached matrix
## Caches the matrix after performing an inversion
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		## Caches matrix
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	##returns setters/getters
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## solves a cached matrix
## returns cached martrix if the matrix is unchanged
## otherwise it recalculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	
	## if data is unchanged return cached matrixe
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	
	##otherwise recalculate
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

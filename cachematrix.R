## The makeCacheMatrix is a set of functions 
## that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  	set <- function(y){
    		x <<- y
    		inv <<- NULL
  	}
  	get <- function() x
  	setinv <- function(inverse) inv <<- inverse
  	getinv <- function() inv
  	list(set = set,
		get = get,
		setinv = setinv,
		getinv = getinv)
}


## The cacheSolve function computes the inverse of the special matrix 
## that is returned by the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)){
   		message("Gathering Stored Data")
    		return(inv)
  	}
  	matrix <- x$get()
  	inv <- solve(matrix, ...)
  	x$setinv(inv)
  	inv      
}
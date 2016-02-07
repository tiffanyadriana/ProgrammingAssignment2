## Two functions; one to set and get a matrix and its inverse in an intrinic variable
## second will check whether the inverse is already set, if so cached vaule is returned

## Creates list of methods to get, set, and inverse the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y){
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setinv <- function(solve) inv <<- solve
	getinv <- function () inv
	list (set=set, get=get, setinv=setinv, getinv=getinv)	 
}


## Checks whether inverse exists, if so returns it. otherwise create the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message ("getting cached data")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

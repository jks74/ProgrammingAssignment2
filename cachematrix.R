## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv_matx <- NULL		 
	set <- function(y) {
    	    x <<- y
    	    inv_matx <<- NULL
	}
  	get <- function() x
  	setinv <- function(inv) inv_matx <<- inv
  	getinv <- function() inv_matx
  	list(set = set, get = get,
       	setinv = setinv,
       	getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
   inv_matx <- x$getinv()
   if(!is.null(inv_matx)) {
    message("Getting Cached Inverse Matrix")
    return(inv_matx)
    }
    data <- x$get()
    inv_matx <- solve(data, ...)
    x$setinv(inv_matx)
    inv_matx
}

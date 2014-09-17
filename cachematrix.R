## These two functions will cache the inverse of a matrix


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## the input is any numerical matrix
## m is a list of functions created by makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve computes the inverse of a matrix
## if the matrix has already been inverted and cached, it will just use the 
## existing result and comment getting cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## see if the function list has a non-null for the inverse calculation
	## if so, then m already has the solution

	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	
        data <- x$get()
        m <- solve(data, ...)
	x$setinverse(m)
        m
}



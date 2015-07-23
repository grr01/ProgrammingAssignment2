## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##                  makeCacheMatrix return a special "vector", which is really a list containing a function to
##                  set the value of the vector
##                  get the value of the vector
##                  set the value of the matrix
##                  get the value of the matrix
##
## Parm: 			A matrix
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, 
		     get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}

## cacheSolve: 
## 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}		


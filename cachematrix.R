## function to compare if 2 matrix are equal or not
## Return TRUE or FALSE
matequal <- function(x, y) {
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}		

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##                  makeCacheMatrix return a special "vector", which is really a list containing a function to
##                  set the value of the matrix`
##                  get the value of the matrix
##                  set the value of the inversed matrix
##                  get the value of the inversed matrix
##
## Parm: 			A matrix
##

makeCacheMatrix <- function(y = matrix()) {
        m <- NULL
        set <- function(y = matrix()){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(ySolve= matrix()) m <<- ySolve
        getsolve <- function() m
        list(set = set, 
      	     get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve: 
## 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
##
## Return a matrix that is the inverse of 'x'
##

cacheSolve <- function(x = matrix(), ...) {
        
        ## Validate if we do have one in cache
        m <- z$getsolve()
        if(!is.null(m)) {
		        ## Validate if this is the same matrix as the one in the cache
				        if (matequal(x,z$get()))
				        {
				                ## let use the one cache
                        message("getting cached matrix")
                        return(m)
				        }
        }

        m <- solve(x, ...)
        ## Cache it
		    z$set(x)
        z$setsolve(m)
        m
}		


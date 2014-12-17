## These functions take a matrix, calculate its inverse and then caches the resulting 
## inverse for future without the need to recalculate the inverse.

## This function provides all the intermediate stages required to cache the matrix 
## inverse.  It passes the input matrix to the function which will calculate the 
## inverse, and retrieves the inverted matrix from that function.  Should the same 
## input matrix be supplied to the function that calculates the inverse, then this 
## function returns the inverted matrix and the inverse is not recalculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y,x,z) {
                x <<- as.matrix(y,x,z)
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function, on being passed a matrix, calls the getinverse function passing the 
## matrix to that.  If nothing is returned then the function calculates the inverse 
## of the input matrix, otherwise, it retrieves
## the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

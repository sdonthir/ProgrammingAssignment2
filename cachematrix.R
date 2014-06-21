## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m  <- NULL
        set <- function(y){
                x <<- y
                m  <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m  <<- inverse
        getinverse <- function() m 
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function will Compute the inverse for given matrix. 
## If the inverse of matrix has already been calculated, then the cachesolve will fetch the inverse from the cache
## Solve function will compute Inverse of the Matrix

cacheSolve <- function(x, ...) {
        m  <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m  <- solve(data, ...)
        x$setinverse(m)
        m 
}

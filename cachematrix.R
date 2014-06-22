## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse already cached, then retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        
        ## First checks to see if the matrix is already cached. If so, it gets the inversed matrix
        ## from the cache.
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        
        ## If the matrix is not cached, it will solve the inversed matrix and sets the inversed matrix
        ## in the cache via the setmatrix funciton.
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}

## Creates a list of function to set/get the matrix and to set/get the inverse
## It caches the inverse matrix in a different enviroment than the parent so that
## it can be used next time by cacheSolve() without having to compute it
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## Calculates the inverse of a matrix. If the inverse has been previously
## calculated and is cached, use it without calculating it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

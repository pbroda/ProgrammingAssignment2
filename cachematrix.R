makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()
                x
        setinverse <- function(inverse)
                m <<- inverse
        getinverse <- function()
                m
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
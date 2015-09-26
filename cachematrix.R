# R programming: Assignment 2 - Caching a matrix's inverse 


#' makeCacheMatrix
#' function that creates the special matrix object and can cache it's inverse
#' 
#' @param x - matrix
#'
#' @return list of 4 "methods": get, set, getinverse, setinverse
#'
#' @examples    x <- makeCacheMatrix(matrix(1:4,2,2))  #create new matrix
#'              x$get()                                #display the matrix
#'              x$set(matrix(2:5),2,2)                 #change the value of matrix
#'              x$getinverse()                         #returns the inverse of matrix x
#'              x$setinverse(solve(x$get()))           #caches the inverse of matrix x
#' 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  #"clears" the cached inverse
        set <- function(y) { 
                x <<- y #set the matrix values
                m <<- NULL #clears the cached inverse
        }
        get <- function() #function to display the matrix 
                x
        setinverse <- function(inverse) #caches the given value (inverse)
                m <<- inverse
        getinverse <- function() #returns cached value of inverse
                m
        list( #return list
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}



#' cacheSolve
#' function that returns inverse of the given matrix x (created by makeCacheMatrix).
#' If x has cached inverse (computed before), then cacheSolve returns cached value, 
#' else it computes the inverse
#'
#' @param x - matrix object created by makeCacheMatrix function
#' @param ... 
#'
#' @return matrix - an inverse of matrix x
#' @export
#'
#' @examples x <- makeCacheMatrix(matrix(1:4,2,2))     #creates object x 
#'           cacheSolve(X)                             #computes the inverse of x (and caches it)
#'           cacheSolve(X)                             #returns the inverse of x (as it was already cached)
#'           
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
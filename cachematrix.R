## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Initially, the function sets the m to null so as to calculate a new inverse
## setmatrix, getmatrix asre used to assign the mtrix and get the value of matrix
## setinv and getinv are used to find and obtain the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y){
                x <<- y
                m <<- NULL
            }
        getmatrix <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        
        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

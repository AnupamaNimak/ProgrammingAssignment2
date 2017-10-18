# Anupama Nimak Oct 2017
# This is Week 3 assignment to generate an inverse of a matrix using function Solve
# makeCacheMatrix function creates a matrix object that can cache its inverse
# cacheSolve function calculates the inverse of the matrix generated in the first function and returns it.
# Second function returns the inverse.


# Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) minv <<- solve
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#Compute inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        minv <- x$getinv()
        if (!is.null(minv)) {
                message("getting cached data.")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        
        # Returing a matrix that is inverse of x
        minv
        
}


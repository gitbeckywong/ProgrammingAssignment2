## The makeCacheMatrix and cacheSolve functions contained herein calculate the inverse of a matrix
## and store it in the "cache."  If the matrix inverse has already been previously calculated,
## then it is simply retrieved from the cache rather than being recalculated.
## The code contained herein are modified version of the makeVector and cachemean functions
## from Programming Assignment 2 of the Coursera "R Programming" course.

## This makeCacheMatrix function creates a list of functions
## that can be called elsewhere.
## Note that in accordance with the assignment directions, it is assumed that
## the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) { ## The input for this function is a matrix.
    m <- NULL   ## Sets the value of m to NULL within the function environment.
    set <- function(y) { ## This function allows you to change the supplied matrix to a new matrix, y.
        x <<- y ## Sets the newly supplied matrix y to x.
        m <<- NULL ## Resets the value of m to NULL in the parent environment (i.e. erases a cached value of m).
    }
    get <- function() x     ## This function returns (prints out) x, the supplied matrix.
    setinverse <- function(inverse) m <<- inverse ## Store m (the inverted matrix) in the parent enviornment.
    getinverse <- function() m ## If m exists in the parent environment, returns m (otherwise, returns NULL).
                               ## m will be the matrix inverse after the cacheSolve function has been run.
    list(set = set, get = get, ## Creates a list of the functions created by this function (and names them).
         setinverse = setinverse,
         getinverse = getinverse)
}


## This cacheSolve function takes a supplied matrix "x" and returns
## the inverse of the matrix.  It utilizes the makeCacheMatrix function.
## Note that in accordance with the assignment directions, it is assumed that
## the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {   ## If a value is already stored in m, retrieve the cached value.
        message("getting cached data")
        return(m)   ## Return the value of m (inverted matrix) if it already exists.
    }
    data <- x$get() ## Get the supplied matrix.
    m <- solve(data, ...)   ## Compute the inverse of the supplied matrix
                            ## and store as m within the function environment.
    x$setinverse(m) ## Stores m in parent environment to "cache" for future use.
    m   ## Return the inverted matrix.
}

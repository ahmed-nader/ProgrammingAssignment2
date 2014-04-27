## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## In this assignment, I've wrote a pair of functions that cache the inverse of a matrix. The two funcrions are: makeCacheMatrix and cacheSolve.

########################################
## First Function >> makeCacheMatrix: ##
########################################
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# By Calling this function, you are creating a matrix.
# By default, the inverse of this matrix is not calculated, and is equal to NULL.
# The created matrix is either the given (via the Function Argument), or the default 1x1 Empty Matrix.

    # Setting the Default Value for the Inverse of the Matrix.
    inv <- NULL

    # Set the value of the Matrix.
    set <- function(matrixObj) {
        x <<- matrixObj
        inv <<- NULL
    }

    # Get the value of the Matrix.
    get <- function() x
  
    # A Function to Cache/Set the Inverse of the Matrix.
    setInverse <- function(inverse) inv <<- inverse

    # A Function to Request/Get the Cached Inverse of the Matrix, or NULL if it was not Cached!
    getInverse <- function() inv
 
    # Defining the output of this function, i.e. the value to be returned!
    # The output is a list of 4 functions, set, get, setInverse, and getInverse.
    list(set = set, get = get,    
         setInverse = setInverse,  
         getInverse = getInverse)

}

###################################
## First Function >> cacheSolve: ##
###################################
## This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## For this assignment, assumbtion is made that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
# Calling this Function requires providing an instant of the special "matrix" returned by makeCacheMatrix.
    
    # Using the getInverse function to retrieve the Cached Inverse of this Matrix.
    inv <- x$getInverse()
    
    
    if(!is.null(inv)) {
        # If the Inverse of this Matrix was previously Cached (not equal NULL).
        # The following message is printed, and this Cached Inverse is returned.
        message("getting cached data")
        return(inv)
    }

    # If the Inverse of this Matrix was not previously Cached (equal NULL).
    # The Value of the Matrix Object is retrieved, and the inverse is Calculated with solve function in R.
    matrixObj <- x$get()
    inv <- solve(matrixObj, ...)
    
    # After calculating the inverse, setInverse fundtion is used to Cache/Set the Inverse of the Matrix.
    x$setInverse(inv)

    # The output of this Function is the Inverse to the provided Matrix.
    inv
}

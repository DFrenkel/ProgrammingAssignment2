## This package includes two functions:
## makeCacheMatrix function is a "constructor" that creates matrix objects
## cacheSolve function is a solver function that returns inverse of the matrix
## and caches it, since matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## 
## To try using the functions, call testCacheSolve.
## It should print sequence of messages, but no errors

## This is a "constructor" function that returns another function 
## called cacheMatrix
## New cacheMatrix function will be able to compute and cache
## the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
    # inversed matrix value, initially set to NULL
    cachedInverse <- NULL
    
    # getInverse function returns value of inversed matrix
    getInverse <- function() {
        cachedInverse  
    }

    # setInverse function will save value into cacheInverse variable
    # it will be called by cacheSolve function when it needs
    # to save new value of inversed matrix value
    setInverse <- function(newValue) {
        cachedInverse <<- newValue
    }
    
    # get function will return original matrix
    get <- function() {
        x    
    }
    
    # set function will replace the martix and reset inversed back to null
    set <- function(newValue) {
        x <<- newValue
        cachedInverse <<- NULL
    }
    
    # return a list conraining original matrix and two functions
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function returns a matrix that is the inverse of 'x'
## x is the matrix "object" created by makeCacheMatrix.
## Other parameters are used to pass to solve function
## when calculating inverse value for the first time
cacheSolve <- function(x, ...) {
    # Try getting inverse value
    result <- x$getInverse()
    
    # If the value is null, it hasn't yet been computed
    # so we compute it here and save it for the future
    if (is.null(result)) {
        result <- solve(x$get(), ...)
        x$setInverse(result)
    }
    
    # Etither way, we have computed inverse of the matrix in result variable,
    # so we can return it
    result
}

## Test function - call it to confirm the correctness
## of the two functions above
## You should see series of messages but no errors
testCacheSolve <- function() {
    # Use makeCacheMatrix to make a matrix object:
    message("Creating new matrix 4x4..")
    x <- matrix(rnorm(16, 0, 1), 4, 4)
    
    # Create Matrix object by calling
    message("Creating matrix object...")
    myMatrix <- makeCacheMatrix(x)
    
    # Test that inverse has not been compluted:
    test <- myMatrix$getInverse()
    if (is.null(test)) {
        message("As expected, inverse value has not yet been computed")
    } else {
        stop("Something is wrong! Inverse value is not set to NULL!")
    }
    
    # Compute inverse of the matrix object by calling
    message("Computing inverse...")
    myInverse <- cacheSolve(myMatrix)
    
    # Verify that myInverse was indeed cached:
    test <- myMatrix$getInverse()
    if (identical(test, myInverse)) {
        message("Inverse value was cached!")
    } else {
        stop("Something is wrong! Inverse value was not cached!")
    }
    
    # Reset the matrix:
    message("Resetting matrix values to new random 5x5 matrix...")
    myMatrix$set(matrix(rnorm(25, 0, 1), 5, 5))
    
    # Test that inverse has been reset back to NULL:
    test <- myMatrix$getInverse() 
    if (is.null(test)) {
        message("As expected, inverse value was reset back to NULL")
    } else {
        stop("Something is wrong! Inverse value was not reset to NULL!")
    }
    
    # Re-compute inverse of the matrix object by calling
    message("Computing inverse...")
    myInverse <- cacheSolve(myMatrix)
    
    # Verify that myInverse was indeed cached:
    test <- myMatrix$getInverse()
    if (identical(test, myInverse)) {
        message("Inverse value was cached!")
    } else {
        stop("Something is wrong! Inverse value was not cached!")
    }
}

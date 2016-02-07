# Below are two functions that are used to create a special object 
# that stores a numeric matrix and caches its inversed matrix.


# The first functon is makeCacheMatrix(), which creates a special "vector", 
# which is a list containing 4 functions:
# 1. set(): sets the values of the matrix
# 2. get(): gets the values of the matrix
# 3. setinv(): sets the inversed matrix
# 4. getinv(): gets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    # Set the value of the var 'i' in the current envir. within the
    # makeCacheMatrix() function to NULL
    inv <- NULL
    
    # Assign to the value of the var 'x' and 'inv' in the parent envir. of 
    # the set() function, i.e. in the envir. of the makeVector() function,
    # the value of the var. 'y' within the current envir. of the set() function
    # and NUll, respectively
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    
    # Get the value of var. 'x' in the parent envir. of get() function
    # i.e. envir. of the makeCacheMatrix() function
    get <- function() x 
    
    # Assign the value of the var. 'inversed.matrix' in the current envir. of setmean() function
    # to var. 'inv' in the parent envir. of the setinv()), 
    # i.e. envir. of makeCacheMatrix()
    setinv <- function(inversed.matrix) inv <<- inversed.matrix 
    
    # Get the value of var. 'inv' in the parent envir. of getinv() function
    # i.e. envir. of the makeCacheMatrix() functon
    getinv <- function() inv
    
    # Returns a list with the functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# The second function is cachSolve() function computes the inverse matrix 
# of the matrix matrix" created with the above function. 
# It first checks to see if the inverse of the matrix has already 
# been calculated. If so, it gets the results from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the matrix and 
# sets the value of the inverse in the cache via the setinv() function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() # Gets the value of the variable 'inv' in the envir. of 'x'
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get() # Gets the matrix from the var. 'x'
    
    inv <- solve(data, ...) # Calculate the inversed matrix
    
    # Set the value of the car 'inv' in the env. of 'x' to the value
    # of the var. 'inv' in the current envir.
    x$setinv(inv) 
    
    inv #  ## Return a matrix that is the inverse of 'x'
}

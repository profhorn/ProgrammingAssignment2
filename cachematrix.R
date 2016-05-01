## Below is a set of functions to calculate inverse of a square matrix 
#  The result is saved to speed up execution if the inverse matrix function is called more than once.

## makeCacheMatrix builds a list containing a function to do the following:
#      1. set the value of the matrix
#      2. get the value of the matrix
#      3. set the value of inverse of the matrix
#      4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve returns the inverse of a function
#  Function will calculate the inverse with the first execution of the function.
#  Subsequent calls to the function will use the saved inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        # has inverse has already been calculated?
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # If first time called function calculates the inverse 
        m1.data = x$get()
        inv = solve(m1.data, ...)
        
        # Perserve the value of the inverse in cache
        x$setinv(inv)
        
        return(inv)
}

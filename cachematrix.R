# creates the 'special' matrix with 4 functions
# get(), set(), getsolve(), setsolve()
makeCacheMatrix <- function(x = matrix()) {
    mysolve <- NULL

    # function to set the matrix
    set <- function(y) {
        x <<- y
        mysolve <<- NULL
    }

    # function to get the matrix
    get <- function() x

    # function to set the solve of matrix
    setsolve <- function(solve) mysolve <<- solve

    # function to get the solve of matrix
    getsolve <- function() mysolve

    # return the list with all internal functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# calculates the solve of the matrix and cache it in special matrix object
cacheSolve <- function(x, ...) {
        # try to get the solve of the special matrix
        mysolve <- x$getsolve()

        # if solve is not null, then return the cached solve
        if(!is.null(mysolve)) {
            message("getting cached inverse matrix")
            return(mysolve)
        }

        # calculate the inverse and cache it
        data <- x$get()
        mysolve <- solve(data, ...)
        x$setsolve(mysolve)

        ## Return a matrix that is the inverse of 'x'
        mysolve

}

## The makeCacheMatrix functions prepares the required actions to successfully solve for the inverse
## of the matrix in the cacheSolve function.  


## This first function is really just a list of values and usable functions to execute when solving.  It prepares the
## dataset to go through inversion.

makeCacheMatrix <- function(x = matrix()) {
   inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This retrieves the inv value from the makeCacheMatrix function.  It then checks if the value is NULL.  If it is not
## NULL it simply returns the data.  If inv is NULL, it gets the matrix, solves it, sets inv to the inverse
## to be retrieved at a later time, and returns the inverse.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                message("Retrieving cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
